//===- include/rld/symbol.h -------------------------------*- mode: C++ -*-===//
//*                      _           _  *
//*  ___ _   _ _ __ ___ | |__   ___ | | *
//* / __| | | | '_ ` _ \| '_ \ / _ \| | *
//* \__ \ |_| | | | | | | |_) | (_) | | *
//* |___/\__, |_| |_| |_|_.__/ \___/|_| *
//*      |___/                          *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_SYMBOL_H
#define RLD_SYMBOL_H

#include <atomic>
#include <memory>
#include <mutex>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/simple_ilist.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include "pstore/core/address.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "rld/context.h"

#include <atomic>
#include <thread>

namespace rld {

//-MARK: SpinLock
class SpinLock {
public:
  static constexpr uint64_t SpinsBeforeYield = 1000;

  void lock() {
    auto Count = uint64_t{0};
    while (Flag_.test_and_set(std::memory_order_acquire)) {
      if (++Count > SpinsBeforeYield) {
        std::this_thread::yield();
      }
    }
  }
  void unlock() { Flag_.clear(std::memory_order_release); }

private:
  std::atomic_flag Flag_ = ATOMIC_FLAG_INIT;
};

class Symbol;

//*  _   _         _      __       ___         _        _               *
//* | | | |_ _  __| |___ / _|___  / __|___ _ _| |_ __ _(_)_ _  ___ _ _  *
//* | |_| | ' \/ _` / -_)  _(_-< | (__/ _ \ ' \  _/ _` | | ' \/ -_) '_| *
//*  \___/|_||_\__,_\___|_| /__/  \___\___/_||_\__\__,_|_|_||_\___|_|   *
//*                                                                     *
//-MARK: UndefsContainer
class UndefsContainer {
public:
  UndefsContainer() : StrongUndefCount_{0U} {}

  using Container = llvm::simple_ilist<Symbol>;

  bool empty() const {
    std::lock_guard<std::mutex> const Lock{Mut_};
    return List_.empty();
  }
  // \returns The total number of undefined symbols. Note that this includes
  //   those symbols that are weakly referenced.
  size_t size() const {
    std::lock_guard<std::mutex> const Lock{Mut_};
    return List_.size();
  }
  /// \returns  The number of undefined symbols not including those that are
  ///   only weakly referenced.
  size_t strongUndefCount() const { return StrongUndefCount_.load(); }
  /// Call when we encounter a strong reference to an existing undefined symbol
  /// that was previously weakly referenced.
  void addStrongUndef() {
    auto const Prev = StrongUndefCount_++;
    (void)Prev;
    assert(Prev < std::numeric_limits<decltype(Prev)>::max());
  }

  void remove(Symbol *Sym, pstore::repo::reference_strength Strength);
  Symbol *insert(Symbol *Sym);

  Container::const_iterator begin() const { return List_.begin(); }
  Container::const_iterator end() const { return List_.end(); }

  /// A debugging aid which returns true if the cached count of strongly
  /// undefined symbols agrees with the contents of the undefined symbols list.
  bool strongUndefCountIsCorrect() const;

private:
  std::atomic<size_t> StrongUndefCount_;

  mutable std::mutex Mut_;
  Container List_;
};

struct Contribution;

//*  ___            _         _  *
//* / __|_  _ _ __ | |__  ___| | *
//* \__ \ || | '  \| '_ \/ _ \ | *
//* |___/\_, |_|_|_|_.__/\___/_| *
//*      |__/                    *
//-MARK: Symbol
class Symbol : public llvm::ilist_node<Symbol> {
public:
  class Body {
  public:
    Body(const pstore::repo::definition *const Def,
         const std::shared_ptr<const pstore::repo::fragment> &Fragment,
         const FragmentAddress FAddr, const uint32_t InputOrdinal)
        : InputOrdinal_{InputOrdinal}, Def_{Def}, Fragment_{Fragment},
          FAddr_{FAddr} {}

    pstore::repo::linkage linkage() const { return Def_->linkage(); }
    pstore::repo::visibility visibility() const { return Def_->visibility(); }

    uint32_t inputOrdinal() const { return InputOrdinal_; }
    const std::shared_ptr<const pstore::repo::fragment> &fragment() const {
      return Fragment_;
    }
    FragmentAddress fragmentAddress() const { return FAddr_; }

  private:
    /// The index number of the object file which defined this symbol. Used to
    /// guarantee that a linkonce instance from the first object file listed on
    /// the command line is retained regardless of the order in which the files
    /// are processed.
    uint32_t InputOrdinal_;
    /// The symbol's definition.
    pstore::repo::definition const *Def_;
    /// The fragment which provides the definition of this symbol.
    FragmentPtr Fragment_;
    /// The fragment's address. Used to find its shadow memory offset.
    FragmentAddress FAddr_;
  };

  /// Constructs an undefined symbol.
  ///
  /// \param N  The name of the symbol.
  /// \param Strength  The strength of the reference (weak/strong) that caused
  /// the creation of this symbol.
  explicit Symbol(StringAddress N, pstore::repo::reference_strength Strength)
      : Name_{N.absolute()},
        WeakUndefined_{Strength == pstore::repo::reference_strength::weak},
        Contribution_{nullptr} {
    assert(name() == N &&
           N.absolute() < (UINT64_C(1) << StringAddress::total_bits));
  }

  Symbol(StringAddress N, Body &&Definition)
      : Name_{N.absolute()}, WeakUndefined_{false}, Contribution_{nullptr},
        Definition_{llvm::SmallVector<Body, 1>{std::move(Definition)}} {
    assert(name() == N &&
           N.absolute() < (UINT64_C(1) << StringAddress::total_bits));
  }

  using Mutex = SpinLock;
  using BodyContainer = llvm::SmallVector<Body, 1>;
  using OptionalBodies = llvm::Optional<BodyContainer>;

  enum { DefinitionIndex, LockIndex };

  auto definition() const
      -> std::tuple<const OptionalBodies &, std::unique_lock<Mutex>> {
    std::unique_lock<Mutex> Lock{Mut_};
    return {std::cref(Definition_), std::move(Lock)};
  }

  auto definition() -> std::tuple<OptionalBodies &, std::unique_lock<Mutex>> {
    std::unique_lock<Mutex> Lock{Mut_};
    return {std::ref(Definition_), std::move(Lock)};
  }

  bool hasDefinition() const {
    std::lock_guard<Mutex> Lock{Mut_};
    assert((!Definition_.hasValue() || Definition_->size() > 0U) &&
           "A defined symbol must have a least 1 body");
    return Definition_.hasValue();
  }

  /// \returns The name of the symbol.
  StringAddress name() const;
  /// \returns The value of the symbol. Available once layout is complete.
  uint64_t value() const;

  void setFirstContribution(Contribution *const C) {
    Contribution *expected = nullptr;
    Contribution_.compare_exchange_strong(expected, C);
  }
  Contribution *contribution() const { return Contribution_.load(); }

  /// Records a reference from an external fixup.
  ///
  /// \param Strength  The strength of the reference (weak/strong).
  /// \returns True if the resulting symbol was previously only weakly
  ///   referenced, is now strongly referenced and undefined.
  bool addReference(pstore::repo::reference_strength Strength);

  /// \returns True if the symbol is undefined and all references to it are
  /// weak.
  bool allReferencesAreWeak() const {
    std::lock_guard<Mutex> Lock{Mut_};
    assert(!WeakUndefined_ ||
           !Definition_.hasValue() &&
               "A weakly undefined symbol must not have a definition");
    return WeakUndefined_;
  }

  /// Process the definition of a symbol with append linkage that "collides"
  /// with an existing definition.
  ///
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns  this on success or nullptr if there was an existing non-append
  ///   definition of the symbol.
  Symbol *updateAppendSymbol(const pstore::database &Db,
                             const pstore::repo::definition &Def,
                             const NotNull<UndefsContainer *> Undefs,
                             uint32_t InputOrdinal);

  /// Process the definition of a symbol with common linkage that "collides"
  /// with an existing definition.
  ///
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns  this on success or nullptr if there was an existing non-append
  ///   definition of the symbol.
  Symbol *updateCommonSymbol(const pstore::database &Db,
                             const pstore::repo::definition &Def,
                             const NotNull<UndefsContainer *> Undefs,
                             uint32_t InputOrdinal);

  /// Process the definition of a symbol with external linkage that "collides"
  /// with an existing definition.
  ///
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns  this on success or nullptr if there was an existing incompatible
  ///   definition of the symbol.
  Symbol *updateExternalSymbol(const pstore::database &Db,
                               const pstore::repo::definition &Def,
                               const NotNull<UndefsContainer *> Undefs,
                               uint32_t InputOrdinal);

  /// Process the definition of a symbol with link-once linkage that "collides"
  /// with an existing definition.
  ///
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns  this on success or nullptr if there was an existing incompatible
  ///   definition of the symbol.
  Symbol *updateLinkOnceSymbol(const pstore::database &Db,
                               const pstore::repo::definition &Def,
                               const NotNull<UndefsContainer *> Undefs,
                               uint32_t InputOrdinal);

  /// Process the definition of a symbol with weak linkage that "collides" with
  /// an existing definition.
  ///
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns  this on success or nullptr if there was an existing incompatible
  ///   definition of the symbol.
  Symbol *updateWeakSymbol(const pstore::database &Db,
                           const pstore::repo::definition &Def,
                           const NotNull<UndefsContainer *> Undefs,
                           uint32_t InputOrdinal);

private:
  /// If we're an earlier input file than the one which was responsible
  /// for this definition, then ours beats it out; if we're later than
  /// ours is discarded.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this.
  Symbol *replaceIfLowerOrdinal(const std::unique_lock<Mutex> &Lock,
                                const pstore::database &Db,
                                const pstore::repo::definition &Def,
                                uint32_t InputOrdinal);

  /// Associates a body with the symbol which must not already have one.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this.
  Symbol *defineImpl(std::unique_lock<Mutex> &&Lock, const pstore::database &Db,
                     const pstore::repo::definition &Def,
                     const NotNull<UndefsContainer *> Undefs,
                     uint32_t InputOrdinal);

  /// Associates a body with the symbol which must already have one.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The containing database.
  /// \param Def  The new definition.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this.
  Symbol *replaceImpl(const std::unique_lock<Mutex> &Lock,
                      const pstore::database &Db,
                      const pstore::repo::definition &Def,
                      uint32_t InputOrdinal);

  /// Associates a body with the symbol which must already have one.
  ///
  /// \param Lock  A lock on the symbol's mutex.
  /// \param Db  The containing database.
  /// \param Def  The new definition.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \param Fragment  The symbol's fragment. This is the data that is
  ///   associated with the symbol.
  /// \returns this.
  Symbol *replaceImpl(const std::unique_lock<Mutex> &Lock,
                      const pstore::database &Db,
                      const pstore::repo::definition &Def,
                      uint32_t InputOrdinal, const FragmentPtr &Fragment);

  void setName(StringAddress N);

  mutable Mutex Mut_; // TODO: is this really better than std::mutex?
  /// The StringAddress representing the name of this symbol.
  std::uint64_t Name_ : StringAddress::total_bits;
  /// Was this symbol instance created as the result of encountering a weak
  /// reference? This will always be false if the symbol is defined.
  std::uint64_t WeakUndefined_ : 1;

  /// The output contribution produced by this symbol. Set during layout.
  std::atomic<Contribution *> Contribution_;

  /// We allow an array of definitions so that append symbols can associate
  /// multiple definitions with a single name.
  llvm::Optional<BodyContainer> Definition_;
};

// name
// ~~~~
inline StringAddress Symbol::name() const {
  const std::lock_guard<decltype(Mut_)> Lock{Mut_};
  return StringAddress::make(Name_);
}

// set name
// ~~~~~~~~
inline void Symbol::setName(StringAddress N) {
  std::uint64_t const NAbs = N.absolute();
  assert(NAbs < (UINT64_C(1) << StringAddress::total_bits));
  Name_ = NAbs;
}

// add reference
// ~~~~~~~~~~~~~
inline bool Symbol::addReference(pstore::repo::reference_strength Strength) {
  std::lock_guard<Mutex> Lock{Mut_};
  const bool WasWeak = WeakUndefined_;
  // If the symbol is weakly undefined, we must not have a definition.
  assert(!WasWeak ||
         !Definition_.hasValue() &&
             "A weakly undefined symbol must not have a definition");
  WeakUndefined_ =
      WasWeak && Strength == pstore::repo::reference_strength::weak;
  return WasWeak && !WeakUndefined_;
}

//*  _   _         _      __       ___         _        _               *
//* | | | |_ _  __| |___ / _|___  / __|___ _ _| |_ __ _(_)_ _  ___ _ _  *
//* | |_| | ' \/ _` / -_)  _(_-< | (__/ _ \ ' \  _/ _` | | ' \/ -_) '_| *
//*  \___/|_||_\__,_\___|_| /__/  \___\___/_||_\__\__,_|_|_||_\___|_|   *
//*                                                                     *
//-MARK: UndefsContainer
// remove
// ~~~~~~
inline void UndefsContainer::remove(Symbol *const Sym,
                                    pstore::repo::reference_strength Strength) {
  if (Strength != pstore::repo::reference_strength::weak) {
    auto const Prev = StrongUndefCount_--;
    (void)Prev;
    assert(Prev > 0U);
  }
  std::lock_guard<std::mutex> const Lock{Mut_};
  List_.remove(*Sym); // remove this symbol from the undef list.
}

// insert
// ~~~~~~
inline Symbol *UndefsContainer::insert(Symbol *const Sym) {
  std::lock_guard<std::mutex> const Lock{Mut_};
  if (!Sym->allReferencesAreWeak()) {
    this->addStrongUndef();
  }
  List_.push_back(*Sym); // add this symbol to the undef list.
  return Sym;
}

//*  ___ _            _              ___     _     _               *
//* / __| |_  __ _ __| |_____ __ __ | _ \___(_)_ _| |_ ___ _ _ ___ *
//* \__ \ ' \/ _` / _` / _ \ V  V / |  _/ _ \ | ' \  _/ -_) '_(_-< *
//* |___/_||_\__,_\__,_\___/\_/\_/  |_| \___/_|_||_\__\___|_| /__/ *
//*                                                                *
// shadow pointer
// ~~~~~~~~~~~~~~
template <typename T>
inline std::atomic<Symbol *> *shadowPointer(Context &Ctx,
                                            pstore::typed_address<T> Addr) {
  assert(Addr.absolute() % alignof(std::atomic<Symbol *>) == 0);
  return reinterpret_cast<std::atomic<Symbol *> *>(Ctx.shadow() +
                                                   Addr.absolute());
}

template <typename T>
inline const std::atomic<const Symbol *> *
shadowPointer(const Context &Ctx, pstore::typed_address<T> Addr) {
  assert(Addr.absolute() % alignof(std::atomic<Symbol *>) == 0);
  return reinterpret_cast<const std::atomic<Symbol const *> *>(Ctx.shadow() +
                                                               Addr.absolute());
}

// set symbol shadow
// ~~~~~~~~~~~~~~~~~
/// Shadow pointers are initially null, are set to "busy" whilst the pointee is
/// being created, then finally point to a symbol instance. This order is
/// strictly followed.
///
/// \tparam CreateOp A function with the signature `symbol *()`.
/// \tparam UpdateOp A function with the signature `symbol*(symbol *)`
/// \param Sptr A pointer to the symbol's "shadow" memory.
/// \param Create A function which is called if this is the first time this
///   symbol has been encountered. It should return a pointer to the newly
///   created symbol instance.
/// \param Update A function which is called if this is an occurrence of a
///   previously encountered symbol. It should return the symbol to be used
///   (normally the function's input value).
/// \return A symbol instance or null if a "duplicate symbol" error message
///   should be issued.
template <typename CreateOp, typename UpdateOp>
Symbol *setSymbolShadow(std::atomic<Symbol *> *Sptr, CreateOp Create,
                        UpdateOp Update) {
  static auto *const Busy =
      reinterpret_cast<Symbol *>(std::numeric_limits<std::uintptr_t>::max());
  // Is it defined (or in the process of being defined) by another module?
  Symbol *Symbol = nullptr;
  if (Sptr->compare_exchange_strong(Symbol, Busy, std::memory_order_acq_rel,
                                    std::memory_order_relaxed)) {
    assert(Sptr->load(std::memory_order_acquire) == Busy);
    // This is the first time we've encountered this symbol name.
    Symbol = Create();
    assert(Symbol != Busy);
    Sptr->store(Symbol, std::memory_order_release);
    return Symbol;
  }

  // We've previously seen either a reference or a definition of this symbol.
  //
  // Spin until *ns != busy. This ensures that no other thread is in in the
  // process of creating the symbol instance.
  auto Count = uint64_t{0};
  while ((Symbol = Sptr->load(std::memory_order_acquire)) == Busy) {
    if (++Count > SpinLock::SpinsBeforeYield) {
      std::this_thread::yield();
    }
  }
  assert(Symbol != nullptr);
  return Update(Symbol);
}

using LocalSymbolsContainer = llvm::DenseMap<StringAddress, Symbol *>;
/// The Global symbols are allocated in chunks of 4MiB.
using GlobalSymbolsContainer =
    pstore::chunked_vector<Symbol, (4 * 1024 * 1024) / sizeof(Symbol)>;

void debugDumpSymbols(Context const &Ctx,
                      GlobalSymbolsContainer const &Globals);

//*   ___ _     _          _    ___ _                          *
//*  / __| |___| |__  __ _| |__/ __| |_ ___ _ _ __ _ __ _ ___  *
//* | (_ | / _ \ '_ \/ _` | (_-<__ \  _/ _ \ '_/ _` / _` / -_) *
//*  \___|_\___/_.__/\__,_|_/__/___/\__\___/_| \__,_\__, \___| *
//*                                                 |___/      *
//-MARK: GlobalsStorage
class GlobalsStorage {
public:
  explicit GlobalsStorage(unsigned NumWorkerThreads)
      : SymbolMemory_{NumWorkerThreads} {}

  /// Returns the global symbol memory block associated with the current thread,
  /// creating it if necessary.
  NotNull<GlobalSymbolsContainer *> getThreadSymbols();
  /// Splices the collection of global symbol memory blocks into a single
  /// container and returns it. The contents of this object are released in the
  /// process.
  GlobalSymbolsContainer all();

private:
  llvm::DenseMap<char *, GlobalSymbolsContainer> SymbolMemory_;
  std::mutex Mut_;
};

//*  ___            _         _   ___             _              *
//* / __|_  _ _ __ | |__  ___| | | _ \___ ___ ___| |_ _____ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ | |   / -_|_-</ _ \ \ V / -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_| |_|_\___/__/\___/_|\_/\___|_|   *
//*      |__/                                                    *
//-MARK: SymbolResolver
class SymbolResolver {
public:
  explicit SymbolResolver(Context &Ctx) : Context_{Ctx} {}

  /// \tparam Function A function with the signature void(StringAddress).
  /// \param Globals  A container which will hold new defined and undefined
  ///   symbols.
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Compilation  The database compilation record containing
  ///   the symbols to be defined and (via each fragment's xfixup vector)
  ///   referenced.
  /// \param InputOrdinal  The "index" of this compilation: used to
  ///   ensure consistent output regardless of the order of processing.
  /// \param ErrorFn  Called in the event of an error.
  template <typename Function>
  llvm::Optional<LocalSymbolsContainer>
  defineSymbols(const NotNull<GlobalSymbolsContainer *> Globals,
                const NotNull<UndefsContainer *> Undefs,
                const pstore::repo::compilation &Compilation,
                uint32_t InputOrdinal, Function ErrorFn);

  /// Creates a symbol table entry with no associated definition.
  ///
  /// \param Globals  The container which holds defined and undefined symbols.
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Name  The name of the symbol being created.
  /// \param Strength  Is this symbol table entry being created as the
  ///   result of a weak reference?
  /// \returns  The newly created symbol.
  static Symbol *addUndefined(NotNull<GlobalSymbolsContainer *> Globals,
                              NotNull<UndefsContainer *> Undefs,
                              StringAddress Name,
                              pstore::repo::reference_strength Strength);

  /// Records a reference to a symbol.
  ///
  /// \param Symbol  The referenced symbol.
  /// \param Globals  The container which holds defined and undefined symbols.
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Name  The name of the symbol being referenced.
  /// \param Strength  Is this a weak or strong reference to the symbol?
  /// \returns  The referenced symbol.
  static Symbol *addReference(NotNull<Symbol *> Sym,
                              NotNull<GlobalSymbolsContainer *> Globals,
                              NotNull<UndefsContainer *> Undefs,
                              StringAddress Name,
                              pstore::repo::reference_strength Strength);

private:
  Symbol *defineSymbol(const NotNull<GlobalSymbolsContainer *> Globals,
                       const NotNull<UndefsContainer *> Undefs,
                       const pstore::repo::definition &Def,
                       uint32_t InputCount);

  Symbol *add(const NotNull<GlobalSymbolsContainer *> Globals,
              const pstore::repo::definition &Def, uint32_t InputOrdinal);

  Context &Context_;
};

// define symbols
// ~~~~~~~~~~~~~~
template <typename Function>
llvm::Optional<LocalSymbolsContainer>
SymbolResolver::defineSymbols(const NotNull<GlobalSymbolsContainer *> Globals,
                              const NotNull<UndefsContainer *> Undefs,
                              const pstore::repo::compilation &Compilation,
                              const uint32_t InputOrdinal,
                              const Function ErrorFn) {
  bool Error = false;

  LocalSymbolsContainer Locals{Compilation.size()};

  // Define the symbols in this module.
  for (pstore::repo::definition const &Def : Compilation) {
    if (Symbol *const Symbol =
            this->defineSymbol(Globals, Undefs, Def, InputOrdinal)) {

      Locals.insert({Def.name, Symbol});
    } else {
      Error = true;
      ErrorFn(Def.name); // Allow the error to be reported to the user.
    }
  }

  if (Error) {
    return {llvm::None};
  }
  return {std::move(Locals)};
}

// reference symbol
// ~~~~~~~~~~~~~~~~
/// Called when a symbol is referenced by an external fixup.
///
/// \param Ctxt  The global linking context.
/// \param Locals  The collection of symbols defined by the compilation being
/// processed.
/// \param Globals  The container which holds defined and undefined
/// symbols.
/// \param Undefs  The global collection of undefined symbols.
/// \param Name  The name of the symbol being created.
/// \param Strength  The strength of the reference (weak/strong) that caused
/// the creation of this symbol.
Symbol *referenceSymbol(Context &Ctxt, LocalSymbolsContainer const &Locals,
                        NotNull<GlobalSymbolsContainer *> const Globals,
                        NotNull<UndefsContainer *> const Undefs,
                        StringAddress Name,
                        pstore::repo::reference_strength Strength);

} // end namespace rld
#endif // RLD_SYMBOL_H
