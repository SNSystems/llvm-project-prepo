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

#include "rld/Context.h"
#include "rld/Contribution.h"
#include "rld/SectionArray.h"
#include "rld/Shadow.h"

#include "pstore/core/address.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/simple_ilist.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <atomic>
#include <memory>
#include <mutex>
#include <thread>

namespace rld {

/// A function to be used as unique_ptr<> deleter which simply does nothing. For
/// these pointers, storage is managed elsewhere.
constexpr void nopFree(void *) noexcept {}

/// A type similar to unique_ptr<> but where placement new is used for
/// allocation and therefore the deleter function is a no-op.
template <typename T>
using PlacementUniquePtr = std::unique_ptr<T, decltype(&nopFree)>;

template <typename T, typename... Args>
std::enable_if_t<!std::is_array<T>::value, PlacementUniquePtr<T>>
makePlacementUniquePtr(void *ptr, Args &&...args) {
  return PlacementUniquePtr<T>{new (ptr) T(std::forward<Args>(args)...),
                               &nopFree};
}

template <typename T>
std::enable_if_t<!std::is_array<T>::value, PlacementUniquePtr<T>>
makeNullPlacementUniquePtr() {
  return PlacementUniquePtr<T>{nullptr, &nopFree};
}

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
    const std::lock_guard<std::mutex> Lock{Mut_};
    return List_.empty();
  }
  // \returns The total number of undefined symbols. Note that this includes
  //   those symbols that are weakly referenced.
  size_t size() const {
    const std::lock_guard<std::mutex> Lock{Mut_};
    return List_.size();
  }
  /// \returns  The number of undefined symbols not including those that are
  ///   only weakly referenced.
  size_t strongUndefCount() const { return StrongUndefCount_.load(); }
  /// Call when we encounter a strong reference to an existing undefined symbol
  /// that was previously weakly referenced.
  void addStrongUndef() {
    const auto Prev = StrongUndefCount_++;
    (void)Prev;
    assert(Prev < std::numeric_limits<decltype(Prev)>::max());
  }

  void remove(Symbol *Sym, pstore::repo::binding Strength);
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

constexpr bool isLocalLinkage(const pstore::repo::linkage L) {
  return L == pstore::repo::linkage::internal ||
         L == pstore::repo::linkage::internal_no_symbol;
}

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
    Body(const pstore::repo::definition *const Def, FragmentPtr &&Fragment,
         const FragmentAddress FAddr, const uint32_t InputOrdinal)
        : InputOrdinal_{InputOrdinal}, Def_{Def},
          Fragment_{std::move(Fragment)}, FAddr_{FAddr}, ResolveMap_{} {}

    Body(const pstore::repo::definition *const Def, const FragmentPtr &Fragment,
         const FragmentAddress FAddr, const uint32_t InputOrdinal)
        : InputOrdinal_{InputOrdinal}, Def_{Def}, Fragment_{Fragment},
          FAddr_{FAddr}, ResolveMap_{} {}

    Body(Body const &) = delete;
    Body(Body &&Other) noexcept
        : InputOrdinal_{Other.InputOrdinal_}, Def_{Other.Def_},
          Fragment_{std::move(Other.Fragment_)}, FAddr_{std::move(
                                                     Other.FAddr_)},
          ResolveMap_{std::move(Other.ResolveMap_)} {}

    Body &operator=(Body const &) = delete;
    Body &operator=(Body &&Other) noexcept {
      if (this != &Other) {
        InputOrdinal_ = Other.InputOrdinal_;
        Def_ = Other.Def_;
        Fragment_ = std::move(Other.Fragment_);
        FAddr_ = std::move(Other.FAddr_);
        ResolveMap_ = std::move(Other.ResolveMap_);
      }
      return *this;
    }

    const pstore::repo::definition &definition() const { return *Def_; }
    pstore::repo::linkage linkage() const { return Def_->linkage(); }
    pstore::repo::visibility visibility() const { return Def_->visibility(); }
    bool hasLocalLinkage() const { return isLocalLinkage(this->linkage()); }

    uint32_t inputOrdinal() const { return InputOrdinal_; }
    const FragmentPtr &fragment() const { return Fragment_; }
    FragmentAddress fragmentAddress() const { return FAddr_; }

    // TODO: using an section_kind array of pointers here is wasteful. Most
    // fragments don't contain a large number of sections so we're burning a lot
    // of unnecessary space. Could move this to a separate container and
    // use something like pstore::sparse_array<> instead.
    using ResType =
        EnumIndexedArray<pstore::repo::section_kind,
                         pstore::repo::section_kind::last, Symbol **>;
    ResType &resolveMap() { return ResolveMap_; }
    const ResType &resolveMap() const { return ResolveMap_; }

  private:
    /// The index number of the object file which defined this symbol. Used to
    /// guarantee that a linkonce instance from the first object file listed on
    /// the command line is retained regardless of the order in which the files
    /// are processed.
    uint32_t InputOrdinal_;
    /// The symbol's definition.
    const pstore::repo::definition *Def_;
    /// The fragment which provides the definition of this symbol.
    FragmentPtr Fragment_;
    /// The fragment's address. Used to find its shadow memory offset.
    FragmentAddress FAddr_;

    ResType ResolveMap_;
  };

  static pstore::address stringBodyFromIndirect(const pstore::database &DB,
                                                const StringAddress Name) {
    return pstore::indirect_string::read(DB, Name).in_store_address();
  }

  /// Constructs an undefined symbol.
  ///
  /// \param Name  The name of the symbol.
  /// \param NameLength  The number of code units in the name of the symbol.
  /// \param Binding  The weak/strong binding of the reference that caused
  ///   the creation of this symbol.
  explicit Symbol(pstore::address Name, size_t NameLength,
                  pstore::repo::binding Binding)
      : Name_{Name.absolute()}, WeakUndefined_{Binding ==
                                               pstore::repo::binding::weak},
        NameLength_{NameLength}, Contribution_{nullptr}, HasPLT_{false} {
    assert(name() == Name &&
           Name.absolute() < (UINT64_C(1) << StringAddress::total_bits));
  }

  /// Constructs a symbol with an initial definition.
  ///
  /// \param Name  The name of the symbol.
  /// \param NameLength  The number of code units in the name of the symbol.
  /// \param Definition The initial body (value) of the symbol.
  Symbol(pstore::address Name, size_t NameLength, Body &&Definition)
      : Name_{Name.absolute()}, WeakUndefined_{false}, NameLength_{NameLength},
        Contribution_{nullptr} {
    assert(name() == Name &&
           Name.absolute() < (UINT64_C(1) << StringAddress::total_bits));

    llvm::SmallVector<Body, 1> D;
    D.emplace_back(std::move(Definition));
    Definition_.emplace(std::move(D));
  }

  using Mutex = SpinLock;
  using BodyContainer = llvm::SmallVector<Body, 1>;
  using OptionalBodies = llvm::Optional<BodyContainer>;

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
  pstore::address name() const;
  /// \return The length of the symbol name.
  size_t nameLength() const;

  bool hasLocalLinkage() const {
    const auto Def = this->definition();
    const auto &Bodies = std::get<rld::Symbol::OptionalBodies const &>(Def);
    return Bodies.hasValue() && Bodies->front().hasLocalLinkage();
  }

  /// \returns The value of the symbol. Available once layout is complete.
  uint64_t value() const;

  void setPLTIndex(unsigned Index) { PLTIndex_ = Index; }
  unsigned pltIndex() const { return PLTIndex_; }
  void setGOTIndex(unsigned Index) { GOTIndex_ = Index; }
  unsigned gotIndex() const { return GOTIndex_; }

  Contribution *setFirstContribution(Contribution *const C) {
    Contribution *expected = nullptr;
    Contribution_.compare_exchange_strong(expected, C);
    return C;
  }
  Contribution *contribution() const { return Contribution_.load(); }

  /// Invoked when a PLT-related fixup targeting this symbol is encountered. The
  /// first time the function is called it will return true telling the caller
  /// to create a PLT entry; subsequent calls will return false.
  bool shouldCreatePLTEntry() {
    bool Expected = false;
    return HasPLT_.compare_exchange_strong(Expected, true);
  }
  /// Invoked when a GOT-related fixup targeting this symbol is encountered. The
  /// first time the function is called it will return true telling the caller
  /// to create a GOT entry; subsequent calls will return false.
  bool shouldCreateGOTEntry() {
    bool Expected = false;
    return HasGOT_.compare_exchange_strong(Expected, true);
  }

  /// Records a reference from an external fixup.
  ///
  /// \param Binding  The weak/strong binding of the reference.
  /// \returns True if the resulting symbol was previously only weakly
  ///   referenced, is now strongly referenced and undefined.
  bool addReference(pstore::repo::binding Binding);

  /// \returns True if the symbol is undefined and all references to it are
  /// weak.
  bool allReferencesAreWeak() const {
    std::lock_guard<Mutex> Lock{Mut_};
    return this->allReferencesAreWeak(Lock);
  }

  bool allReferencesAreWeak(std::unique_lock<Mutex> const &) const {
    assert((!WeakUndefined_ || !Definition_.hasValue()) &&
           "A weakly undefined symbol must not have a definition");
    return WeakUndefined_;
  }
  bool allReferencesAreWeak(std::lock_guard<Mutex> const &) const {
    assert((!WeakUndefined_ || !Definition_.hasValue()) &&
           "A weakly undefined symbol must not have a definition");
    return WeakUndefined_;
  }

#ifndef NDEBUG
  void checkInvariants() const;
#else
  void checkInvariants() const {}
#endif

  /// Process the definition of a symbol with append linkage that "collides"
  /// with an existing definition.
  ///
  /// \param Db  The owning database.
  /// \param Def  The new definition.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
  Symbol *updateExternalSymbol(const pstore::database &Db,
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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
  Symbol *updateWeakSymbol(const pstore::database &Db,
                           const pstore::repo::definition &Def,
                           const NotNull<UndefsContainer *> Undefs,
                           uint32_t InputOrdinal);

  bool setNextEmit(Symbol *const Next) {
    assert(Next != nullptr && Next != this);
    if (Next->NextEmit != nullptr) {
      return false;
    }
    NextEmit = Next;
    return true;
  }

  // Set by the layout thread and read during final output.
  Symbol *NextEmit = nullptr;

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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
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
  /// \returns  The symbol (i.e. 'this') or null (if there was an existing
  ///   incompatible definition of the symbol).
  Symbol *replaceImpl(const std::unique_lock<Mutex> &Lock,
                      const pstore::database &Db,
                      const pstore::repo::definition &Def,
                      uint32_t InputOrdinal, FragmentPtr &&Fragment);

  mutable Mutex Mut_; // TODO: is this really better than std::mutex?
  /// The StringAddress representing the name of this symbol.
  const uint64_t Name_ : StringAddress::total_bits;
  /// Was this symbol instance created as the result of encountering a weak
  /// reference? This will always be false if the symbol is defined.
  uint64_t WeakUndefined_ : 1;
  const size_t NameLength_;

  /// The output contribution produced by this symbol. Set during layout.
  std::atomic<Contribution *> Contribution_;

  /// True if an associated PLT entry has been created for this symbol.
  std::atomic<bool> HasPLT_{false};
  /// True if an associated GOT entry has been created for this symbol.
  std::atomic<bool> HasGOT_{false};
  /// The index of this symbol's entry in the PLT (if present).
  unsigned PLTIndex_ = 0;
  /// The index for this symbol's entry in the GOT (if present).
  unsigned GOTIndex_ = 0;
  /// We allow an array of definitions so that append symbols can associate
  /// multiple definitions with a single name.
  llvm::Optional<BodyContainer> Definition_;
};

// name
// ~~~~
inline pstore::address Symbol::name() const { return pstore::address{Name_}; }

// name length
// ~~~~~~~~~~~
inline size_t Symbol::nameLength() const { return NameLength_; }

// add reference
// ~~~~~~~~~~~~~
inline bool Symbol::addReference(pstore::repo::binding Binding) {
  std::lock_guard<Mutex> Lock{Mut_};
  const bool WasWeak = WeakUndefined_;
  // If the symbol is weakly undefined, we must not have a definition.
  assert((!WasWeak || !Definition_.hasValue()) &&
         "A weakly undefined symbol must not have a definition");
  WeakUndefined_ = WasWeak && Binding == pstore::repo::binding::weak;
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
                                    pstore::repo::binding Binding) {
  if (Binding != pstore::repo::binding::weak) {
    const auto Prev = StrongUndefCount_--;
    (void)Prev;
    assert(Prev > 0U);
  }
  const std::lock_guard<std::mutex> Lock{Mut_};
  List_.remove(*Sym); // remove this symbol from the undef list.
}

// insert
// ~~~~~~
inline Symbol *UndefsContainer::insert(Symbol *const Sym) {
  const std::lock_guard<std::mutex> Lock{Mut_};
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
#if 0
// symbol shadow
// ~~~~~~~~~~~~~
template <typename T>
inline std::atomic<Symbol *> *symbolShadow(Context &Ctx,
                                           pstore::typed_address<T> Addr) {
  assert(Addr.absolute() % alignof(std::atomic<Symbol *>) == 0);
  return reinterpret_cast<std::atomic<Symbol *> *>(Ctx.shadow() +
                                                   Addr.absolute());
}

template <typename T>
inline const std::atomic<const Symbol *> *
symbolShadow(const Context &Ctx, pstore::typed_address<T> Addr) {
  assert(Addr.absolute() % alignof(std::atomic<Symbol *>) == 0);
  return reinterpret_cast<const std::atomic<const Symbol *> *>(Ctx.shadow() +
                                                               Addr.absolute());
}
#endif

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
/// \returns  A tuple containing a pointer to the symbol (i.e. 'this') or null
///   (if there was an existing incompatible definition of the symbol) and a
///   boolean indicating success.
template <typename CreateOp, typename UpdateOp>
std::tuple<NotNull<Symbol *>, bool>
setSymbolShadow(std::atomic<Symbol *> *Sptr, CreateOp Create, UpdateOp Update) {
  static auto *const Busy =
      reinterpret_cast<Symbol *>(std::numeric_limits<std::uintptr_t>::max());
  // Is it defined (or in the process of being defined) by another module?
  Symbol *Sym = nullptr;
  if (Sptr->compare_exchange_strong(Sym, Busy, std::memory_order_acq_rel,
                                    std::memory_order_relaxed)) {
    assert(Sptr->load(std::memory_order_acquire) == Busy);
    // This is the first time we've encountered this symbol name.
    Sym = Create();
    assert(Sym != Busy);
    Sptr->store(Sym, std::memory_order_release);
    return std::make_tuple(Sym, true);
  }

  // We've previously seen either a reference or a definition of this symbol.
  //
  // Spin until *ns != busy. This ensures that no other thread is in in the
  // process of creating the symbol instance.
  auto Count = uint64_t{0};
  while ((Sym = Sptr->load(std::memory_order_acquire)) == Busy) {
    if (++Count > SpinLock::SpinsBeforeYield) {
      std::this_thread::yield();
    }
  }
  assert(Sym != nullptr);
  Symbol *const S = Update(Sym);
  return std::make_tuple(Sym, S != nullptr);
}

/// A view on of the global symbol table from the point of view of an individual
/// compilation.
struct CompilationSymbolsView {
  struct Value {
    explicit Value(Symbol *Sym) : Sym{Sym} {}
    bool operator==(const Value &Other) const {
      return Sym == Other.Sym && Ifx == Other.Ifx;
    }
    bool operator!=(const Value &Other) const { return !operator==(Other); }

    Symbol *Sym;
    ContributionSpArray *Ifx = nullptr;
  };
  using Container = llvm::DenseMap<StringAddress, Value>;

  explicit CompilationSymbolsView(const unsigned InitialReserve)
      : Map{InitialReserve} {}
  CompilationSymbolsView(const CompilationSymbolsView &) = delete;
  CompilationSymbolsView(CompilationSymbolsView &&Other)
      : Map{std::move(Other.Map)} {}

  CompilationSymbolsView &
  operator=(const CompilationSymbolsView &Other) = delete;
  CompilationSymbolsView &operator=(CompilationSymbolsView &&Other) {
    if (&Other != this) {
      Map = std::move(Other.Map);
    }
    return *this;
  }

  Container Map;
};

using SymbolVector = llvm::SmallVector<Symbol *, 256>;
class GOTPLTContainer {
public:
  SymbolVector GOT;
  SymbolVector PLT;

  void append(GOTPLTContainer const &C) {
    append(&GOT, C.GOT);
    append(&PLT, C.PLT);
  }

  bool empty() const { return GOT.empty() && PLT.empty(); }

private:
  static void append(SymbolVector *const Out, SymbolVector const &In) {
    Out->reserve(Out->size() + In.size());
    std::copy(std::begin(In), std::end(In), std::back_inserter(*Out));
  }
};

/// The Global symbols are allocated in chunks of 4MiB.
using GlobalSymbolsContainer =
    pstore::chunked_sequence<Symbol, (4 * 1024 * 1024) / sizeof(Symbol)>;

void debugDumpSymbols(const Context &Ctx,
                      const GlobalSymbolsContainer &Globals);

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
  NotNull<GlobalSymbolsContainer *> getThreadStorage();
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
  llvm::Optional<CompilationSymbolsView>
  defineSymbols(const NotNull<GlobalSymbolsContainer *> Globals,
                const NotNull<UndefsContainer *> Undefs,
                const pstore::repo::compilation &Compilation,
                uint32_t InputOrdinal, Function ErrorFn);

  /// Given an existing symbol updates it as necessary from a new repo
  /// definition.
  ///
  /// \param Sym  The symbol whose definition is to be updated.
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Def  The repository definition from which the updates occurs.
  /// \param InputOrdinal  The "index" of this compilation: used to
  ///   ensure consistent output regardless of the order of processing.
  /// \returns The original symbol pointer, \p Sym.
  Symbol *updateSymbol(Symbol *const Sym,
                       const NotNull<UndefsContainer *> Undefs,
                       const pstore::repo::definition &Def,
                       const uint32_t InputOrdinal);

  /// Creates a symbol table entry with no associated definition.
  ///
  /// \param Globals  The container which holds defined and undefined symbols.
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Name  The name of the symbol being created.
  /// \param NameLength The length of the symbol name.
  /// \param Binding  Is this symbol table entry being created as the
  ///   result of a weak reference?
  /// \returns  The newly created symbol.
  static NotNull<Symbol *>
  addUndefined(NotNull<GlobalSymbolsContainer *> Globals,
               NotNull<UndefsContainer *> Undefs, pstore::address Name,
               size_t NameLength, pstore::repo::binding Binding);

  /// Records a reference to a symbol.
  ///
  /// \param Sym  The referenced symbol.
  /// \param Globals  The container which holds defined and undefined symbols.
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Name  The name of the symbol being referenced.
  /// \param Binding  Is this a weak or strong reference to the symbol?
  /// \returns  The referenced symbol.
  static NotNull<Symbol *>
  addReference(NotNull<Symbol *> Sym, NotNull<GlobalSymbolsContainer *> Globals,
               NotNull<UndefsContainer *> Undefs, StringAddress Name,
               pstore::repo::binding Binding);

private:
  std::tuple<NotNull<Symbol *>, bool>
  defineSymbol(const NotNull<GlobalSymbolsContainer *> Globals,
               const NotNull<UndefsContainer *> Undefs,
               const pstore::repo::definition &Def, uint32_t InputCount);

  NotNull<Symbol *> add(const NotNull<GlobalSymbolsContainer *> Globals,
                        pstore::address Name, size_t Length,
                        const pstore::repo::definition &Def,
                        uint32_t InputOrdinal);

  Context &Context_;
};

// define symbols
// ~~~~~~~~~~~~~~
template <typename Function>
llvm::Optional<CompilationSymbolsView>
SymbolResolver::defineSymbols(const NotNull<GlobalSymbolsContainer *> Globals,
                              const NotNull<UndefsContainer *> Undefs,
                              const pstore::repo::compilation &Compilation,
                              const uint32_t InputOrdinal,
                              const Function ErrorFn) {
  bool Error = false;

  CompilationSymbolsView Locals{Compilation.size()};

  // Define the symbols in this module.
  for (const pstore::repo::definition &Def : Compilation) {
    const auto DS = this->defineSymbol(Globals, Undefs, Def, InputOrdinal);
    Symbol *const Sym = std::get<NotNull<Symbol *>>(DS);
    const bool Ok = std::get<bool>(DS);
    if (!Ok || Sym == nullptr) {
      Error = true;
      ErrorFn(Sym); // Allow the error to be reported to the user.
    } else {
      // Definition succeeded. Record the symbol that corresponds to this name
      // in this compilation. Fixup resolution will consult this map before
      // falling back to the global symbol table.
      const std::pair<CompilationSymbolsView::Container::iterator, bool> Res =
          Locals.Map.try_emplace(Def.name, CompilationSymbolsView::Value{Sym});
      (void)Res;
      assert(Res.second);
    }
  }

  if (Error) {
    return {llvm::None};
  }
  return {std::move(Locals)};
}

class GroupSet;

// reference symbol
// ~~~~~~~~~~~~~~~~
/// Called when a symbol is referenced by an external fixup.
///
/// \param Ctxt  The global linking context.
/// \param Locals  The collection of symbols defined by the compilation being
///   processed.
/// \param Globals  The container which holds defined and undefined
///   symbols.
/// \param Undefs  The global collection of undefined symbols.
/// \param NextGroup The group of CompilationRefs to be processed by the next
///   top-level pass.
/// \param Name  The name of the symbol being created.
/// \param Binding  The weak/strong binding of the reference that caused
///   the creation of this symbol.
/// \return  A pointer to the referenced symbol.
std::tuple<shadow::TaggedPointer, bool>
referenceSymbol(Context &Ctxt, const CompilationSymbolsView &Locals,
                const NotNull<GlobalSymbolsContainer *> Globals,
                const NotNull<UndefsContainer *> Undefs,
                const NotNull<GroupSet *> NextGroup, StringAddress Name,
                pstore::repo::binding Binding);

} // end namespace rld
#endif // RLD_SYMBOL_H
