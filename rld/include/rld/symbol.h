//*                      _           _  *
//*  ___ _   _ _ __ ___ | |__   ___ | | *
//* / __| | | | '_ ` _ \| '_ \ / _ \| | *
//* \__ \ |_| | | | | | | |_) | (_) | | *
//* |___/\__, |_| |_| |_|_.__/ \___/|_| *
//*      |___/                          *
//===- include/rld/symbol.h -----------------------------------------------===//
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

//*               _      __                 _        _               *
//*  _  _ _ _  __| |___ / _|___  __ ___ _ _| |_ __ _(_)_ _  ___ _ _  *
//* | || | ' \/ _` / -_)  _(_-< / _/ _ \ ' \  _/ _` | | ' \/ -_) '_| *
//*  \_,_|_||_\__,_\___|_| /__/ \__\___/_||_\__\__,_|_|_||_\___|_|   *
//*                                                                  *
//-MARK: UndefsContainer
class UndefsContainer {
public:
  using container = llvm::simple_ilist<Symbol>;

  bool empty() const {
    std::unique_lock<std::mutex> const Lock{mut_};
    return list_.empty();
  }
  size_t size() const {
    std::unique_lock<std::mutex> const Lock{mut_};
    return list_.size();
  }
  void remove(Symbol *Sym);
  void insert(Symbol *Sym);

  container::const_iterator begin() const { return list_.begin(); }
  container::const_iterator end() const { return list_.end(); }

private:
  mutable std::mutex mut_;
  container list_;
};

struct Contribution;

//*                _         _  *
//*  ____  _ _ __ | |__  ___| | *
//* (_-< || | '  \| '_ \/ _ \ | *
//* /__/\_, |_|_|_|_.__/\___/_| *
//*     |__/                    *
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

  explicit Symbol(StringAddress N) {
    setName(N);
    assert(name() == N);
  }

  Symbol(StringAddress N, Body &&Definition)
      : Definition_{llvm::SmallVector<Body, 1>{std::move(Definition)}} {
    setName(N);
    assert(name() == N);
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
    if (Contribution_ == nullptr) {
      Contribution_ = C;
    }
  }
  Contribution *contribution() const { return Contribution_; }

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
  Symbol *replaceIfLowerOrdinal(const std::lock_guard<Mutex> &Lock,
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
  Symbol *defineImpl(const std::lock_guard<Mutex> &Lock,
                     const pstore::database &Db,
                     const pstore::repo::definition &Def,
                     const NotNull<UndefsContainer *> Undefs,
                     uint32_t InputOrdinal);

  /// Associates a body with the symbol which must already have one.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The containing database.
  /// \param Def  The new definition.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  /// Used to impose an order on the symbol definitions that is not related to
  /// the order in which the files are scanned.
  /// \returns this.
  Symbol *replaceImpl(const std::lock_guard<Mutex> &Lock,
                      const pstore::database &Db,
                      const pstore::repo::definition &Def,
                      uint32_t InputOrdinal);

  /// Associates a body with the symbol which must already have one.
  ///
  /// \param Lock  A lock on the symbol's mutex.
  /// \param Db  The containing database.
  /// \param Def  The new definition.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  /// Used to impose an order on the symbol definitions that is not related to
  /// the order in which the files are scanned.
  /// \param Fragment  The symbol's fragment. This is the data that is
  /// associated with the symbol.
  /// \returns this.
  Symbol *replaceImpl(const std::lock_guard<Mutex> &Lock,
                      const pstore::database &Db,
                      const pstore::repo::definition &Def,
                      uint32_t InputOrdinal, const FragmentPtr &Fragment);

  void setName(StringAddress N);

  mutable Mutex Mut_; // TODO: is this really better than std::mutex?
  static constexpr size_t NameElements_ = (StringAddress::total_bits + 7U) / 8U;
  /// The next field is a StringAddress, but optimized so that we don't need
  /// 8-bytes and 8-byte alignment.
  uint8_t Name_[NameElements_]; // StringAddress Name_;

  /// The output contribution produced by this symbol. Set during layout.
  Contribution *Contribution_ = nullptr;

  /// We allow an array of definitions so that append symbols can associate
  /// multiple definitions with a single name.
  llvm::Optional<BodyContainer> Definition_;
};

// name
// ~~~~
inline StringAddress Symbol::name() const {
  const std::lock_guard<decltype(Mut_)> Lock{Mut_};
  static_assert(NameElements_ == 5U, "Expected the Name_ field to be 5 bytes");
  return StringAddress::make(static_cast<uint64_t>(Name_[0]) |
                             (static_cast<uint64_t>(Name_[1]) << 8) |
                             (static_cast<uint64_t>(Name_[2]) << 16) |
                             (static_cast<uint64_t>(Name_[3]) << 24) |
                             (static_cast<uint64_t>(Name_[4]) << 32));
}

// set name
// ~~~~~~~~
inline void Symbol::setName(StringAddress N) {
  std::uint64_t const NAbs = N.absolute();
  static_assert(NameElements_ == 5U, "Expected the Name_ field to be 5 bytes");
  Name_[0] = static_cast<uint8_t>(NAbs & 0xFF);
  Name_[1] = static_cast<uint8_t>((NAbs >> 8) & 0xFF);
  Name_[2] = static_cast<uint8_t>((NAbs >> 16) & 0xFF);
  Name_[3] = static_cast<uint8_t>((NAbs >> 24) & 0xFF);
  Name_[4] = static_cast<uint8_t>((NAbs >> 32) & 0xFF);
}

//*               _      __                 _        _               *
//*  _  _ _ _  __| |___ / _|___  __ ___ _ _| |_ __ _(_)_ _  ___ _ _  *
//* | || | ' \/ _` / -_)  _(_-< / _/ _ \ ' \  _/ _` | | ' \/ -_) '_| *
//*  \_,_|_||_\__,_\___|_| /__/ \__\___/_||_\__\__,_|_|_||_\___|_|   *
//*                                                                  *
inline void UndefsContainer::remove(Symbol *Sym) {
  std::lock_guard<std::mutex> const Lock{mut_};
  list_.remove(*Sym); // remove this symbol from the undef list.
}

inline void UndefsContainer::insert(Symbol *Sym) {
  std::lock_guard<std::mutex> const Lock{mut_};
  list_.push_back(*Sym); // add this symbol to the undef list.
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
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Globals  A container which will hold new defined and undefined
  ///   symbols.
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

  static Symbol *addUndefined(const NotNull<GlobalSymbolsContainer *> Globals,
                              const NotNull<UndefsContainer *> Undefs,
                              const StringAddress Name);

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
Symbol *referenceSymbol(Context &Ctx, StringAddress Name,
                        const LocalSymbolsContainer &Locals,
                        const NotNull<GlobalSymbolsContainer *> Globals,
                        const NotNull<UndefsContainer *> Undefs);

} // end namespace rld
#endif // RLD_SYMBOL_H
