//===- lib/symbol.cpp -----------------------------------------------------===//
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
#include "rld/symbol.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include "rld/Contribution.h"
#include "rld/OutputSection.h"

#include <algorithm>
#include <type_traits>

using pstore::repo::linkage;

namespace {

constexpr auto DebugType = "rld-SymbolTable";

/// Returns the mask value of a specific enumeration value; that is, for enum
/// value v returns 1 << v. The underlying type of the enum must be unsigned.
///
/// \tparam Enum The enumeration type.
/// \param x The value whose mask is to be returned.
/// \returns The mask value of a specific enumeration value; that is, for enum
///   value v returns 1 << v.
template <typename Enum,
          typename = typename std::enable_if<std::is_unsigned<
              typename std::underlying_type<Enum>::type>::value>::type>
constexpr auto maskValue(Enum x) noexcept -> unsigned {
  using UT = typename std::underlying_type<Enum>::type;
  assert(static_cast<UT>(x) < sizeof(unsigned) * 8);
  return 1U << static_cast<UT>(x);
}

/// Computes a mask value for a series of zero or more members of an enumeration
/// type,
///
/// \tparam Enum The enumeration type.
/// \tparam Values A collection of zero or more members of the enumeration type
/// \p Enum.
template <typename Enum, Enum... Values> struct MaskOf;

template <typename Enum>
struct MaskOf<Enum> : std::integral_constant<unsigned int, 0U> {};

template <typename Enum, Enum First, Enum... Rest>
struct MaskOf<Enum, First, Rest...>
    : std::integral_constant<unsigned, maskValue<Enum>(First) |
                                           MaskOf<Enum, Rest...>::value> {};

template <linkage... Values, typename Enum>
constexpr bool isAnyOf(Enum v) noexcept {
  return (maskValue(v) & MaskOf<linkage, Values...>::value) != 0;
}

} // end anonymous namespace

namespace rld {
//*  ___            _         _  *
//* / __|_  _ _ __ | |__  ___| | *
//* \__ \ || | '  \| '_ \/ _ \ | *
//* |___/\_, |_|_|_|_.__/\___/_| *
//*      |__/                    *
//-MARK: Symbol
// value
// ~~~~~
uint64_t Symbol::value() const {
  const Contribution *const C = Contribution_.load();
  // A weakly referenced symbol may be undefined here and has the value 0.
  if (C == nullptr) {
    return 0;
  }
  assert(C != nullptr && C->OScn != nullptr);
  return C->OScn->VirtualAddr + C->Offset;
}

// check invariants
// ~~~~~~~~~~~~~~~~
#ifndef NDEBUG
void Symbol::checkInvariants() const {
  auto BodiesAndLock = this->definition();
  llvm::Optional<Symbol::BodyContainer> const &OptionalBodies =
      std::get<Symbol::DefinitionIndex>(BodiesAndLock);
  if (!OptionalBodies) {
    return;
  }

  auto const &Bodies = OptionalBodies.getValue();

  assert((Bodies.size() == 1U ||
          (Bodies.size() >= 1U &&
           Bodies.front().linkage() == pstore::repo::linkage::append)) &&
         "A symbol must have 1 body unless it has append linkage");

  const auto BodiesEnd = std::end(Bodies);

  assert(std::is_sorted(std::begin(Bodies), BodiesEnd,
                        [](Symbol::Body const &A, Symbol::Body const &B) {
                          return A.inputOrdinal() < B.inputOrdinal();
                        }) &&
         "Symbol bodies must be sorted by input ordinal");
  assert(std::adjacent_find(std::begin(Bodies), BodiesEnd,
                            [](Symbol::Body const &A, Symbol::Body const &B) {
                              return A.inputOrdinal() == B.inputOrdinal();
                            }) == BodiesEnd &&
         "Symbol body input ordinals must be unique");
}
#endif

// replace if lower ordinal
// ~~~~~~~~~~~~~~~~~~~~~~~~
inline Symbol *Symbol::replaceIfLowerOrdinal(
    const std::unique_lock<SpinLock> &Lock, const pstore::database &Db,
    const pstore::repo::definition &Def, const uint32_t InputOrdinal) {
  (void)Lock;
  assert(Lock.owns_lock());
  assert(Definition_ && Definition_->size() == 1U &&
         Definition_->front().inputOrdinal() != InputOrdinal);
  if (InputOrdinal < Definition_->front().inputOrdinal()) {
    auto &ExistingBody = Definition_->front();

    const rld::FragmentPtr Fragment =
        ExistingBody.fragmentAddress() == Def.fext.addr
            ? ExistingBody.fragment()
            : pstore::repo::fragment::load(Db, Def.fext);
    Definition_.emplace(llvm::SmallVector<Body, 1>{
        {Body{&Def, Fragment, Def.fext.addr, InputOrdinal}}});
  }
  return this;
}

// define impl
// ~~~~~~~~~~~
inline auto Symbol::defineImpl(std::unique_lock<SpinLock> &&Lock,
                               const pstore::database &Db,
                               const pstore::repo::definition &Def,
                               const NotNull<UndefsContainer *> Undefs,
                               const uint32_t InputOrdinal) -> Symbol * {
  (void)Lock;
  assert(Lock.owns_lock());
  assert(!Definition_ &&
         "defineImpl was called for a symbol which is already defined");
  Definition_.emplace(llvm::SmallVector<Body, 1>{
      {Body{&Def, pstore::repo::fragment::load(Db, Def.fext), Def.fext.addr,
            InputOrdinal}}});

  const bool WasWeakUndefined = WeakUndefined_;
  // A weakly undefined sybol must not have a definition so here we ensure that
  // this symbol is not tagged as weakly undefined.
  WeakUndefined_ = false;
  Lock.unlock();

  // Remove this symbol from the undef list.
  Undefs->remove(this, WasWeakUndefined
                           ? pstore::repo::reference_strength::weak
                           : pstore::repo::reference_strength::strong);
  return this;
}

// replace impl
// ~~~~~~~~~~~~
inline auto Symbol::replaceImpl(const std::unique_lock<SpinLock> &Lock,
                                const pstore::database &Db,
                                const pstore::repo::definition &Def,
                                const uint32_t InputOrdinal,
                                const FragmentPtr &Fragment) -> Symbol * {
  (void)Lock;
  assert(Lock.owns_lock());
  assert(Definition_ &&
         "If we're replacing a definition, then we must already have one.");
  Definition_.emplace(llvm::SmallVector<Body, 1>{
      {Body{&Def, Fragment, Def.fext.addr, InputOrdinal}}});
  return this;
}

inline auto Symbol::replaceImpl(const std::unique_lock<SpinLock> &Lock,
                                const pstore::database &Db,
                                const pstore::repo::definition &Def,
                                const uint32_t InputOrdinal) -> Symbol * {
  return this->replaceImpl(Lock, Db, Def, InputOrdinal,
                           pstore::repo::fragment::load(Db, Def.fext));
}

// update append symbol
// ~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateAppendSymbol(const pstore::database &Db,
                                const pstore::repo::definition &Def,
                                const NotNull<UndefsContainer *> Undefs,
                                const uint32_t InputOrdinal) -> Symbol * {
  assert(Def.linkage() == linkage::append);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  // If we don't have a definition, create one.
  if (!Definition_) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }

  // Add this symbol to the existing sorted vector for this symbol.
  auto &Bodies = Definition_.getValue();

  // We've already got a definition for this symbol. Append linkage may
  // only collide with another append-linkage symbol.
  assert(Bodies.size() >= 1 &&
         "An append symbol definition must have at least 1 body");
  if (Bodies[0].linkage() != linkage::append) {
    return nullptr; // Error.
  }

  assert(!std::any_of(std::begin(Bodies), std::end(Bodies),
                      [=](Symbol::Body const &B) {
                        return B.inputOrdinal() == InputOrdinal;
                      }) &&
         "Found an existing symbol body associated with this input ordinal");
  assert(std::all_of(std::begin(Bodies), std::end(Bodies),
                     [](Symbol::Body const &B) {
                       return B.linkage() == linkage::append;
                     }) &&
         "All bodies of an append-linkage symbol must have append linkage");

  // We keep the collection of bodies sorted by input ordinal. That enables the
  // layout thread to find the body associated with a specific file just with a
  // binary search. We use an insertion-sort because we expect mostly to be
  // inserting at the end of the collection. Other threads for later input files
  // may have added their contribution to this list already so the search is
  // essential.
  auto const Pos = std::find_if(
      std::rbegin(Bodies), std::rend(Bodies),
      [=](Symbol::Body const &B) { return B.inputOrdinal() < InputOrdinal; });
  assert(Def.linkage() == linkage::append);
  Bodies.insert(Pos.base(),
                Symbol::Body{&Def, pstore::repo::fragment::load(Db, Def.fext),
                             Def.fext.addr, InputOrdinal});
  assert(std::is_sorted(std::begin(Bodies), std::end(Bodies),
                        [](Symbol::Body const &A, Symbol::Body const &B) {
                          return A.inputOrdinal() < B.inputOrdinal();
                        }) &&
         "Append symbol bodies must be sorted by input ordinal");
  return this;
}

// update external symbol
// ~~~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateExternalSymbol(const pstore::database &Db,
                                  const pstore::repo::definition &Def,
                                  const NotNull<UndefsContainer *> Undefs,
                                  const uint32_t InputOrdinal) -> Symbol * {
  assert(Def.linkage() == linkage::external);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};

  // Do we have a definition of the symbol? If not make one.
  if (!Definition_) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }

  // We've already got a definition for this symbol and we're not
  // allowing replacement.
  assert(Definition_->size() == 1 &&
         "An external symbol definition must have exactly 1 body");
  if (isAnyOf<linkage::append, linkage::external, linkage::link_once_any,
              linkage::link_once_odr>((*Definition_)[0].linkage())) {
    return nullptr; // Error.
  }
  return this->replaceImpl(Lock, Db, Def, InputOrdinal);
}

// update common symbol
// ~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateCommonSymbol(pstore::database const &Db,
                                pstore::repo::definition const &Def,
                                NotNull<UndefsContainer *> const Undefs,
                                uint32_t InputOrdinal) -> Symbol * {
  assert(Def.linkage() == linkage::common);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  if (!Definition_) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }
  // If we have already have an external definition of this symbol, we
  // just use it,
  assert(Definition_->size() == 1);
  switch ((*Definition_)[0].linkage()) {
  case linkage::external:
    return this; // common hits external: ignored
  case linkage::append:
  case linkage::link_once_any:
  case linkage::link_once_odr:
    return nullptr; // common hits append/link-once: error
  case linkage::internal:
  case linkage::internal_no_symbol:
    llvm_unreachable("Internal symbols can't collide");
  case linkage::weak_any:
  case linkage::weak_odr:
    return this->replaceImpl(Lock, Db, Def, InputOrdinal);
  case linkage::common:
    auto const BssSize = [](FragmentPtr const &F) {
      assert(F->has_section(pstore::repo::section_kind::bss));
      return F->at<pstore::repo::section_kind::bss>().size();
    };
    FragmentPtr Fragment = pstore::repo::fragment::load(Db, Def.fext);
    std::size_t const ThisSize = BssSize(Fragment);
    std::size_t const ExistingSize = BssSize(Definition_->front().fragment());

    // Replace the existing definition if this one is larger or from a
    // lower input-ordinal. The latter condition ensures stable output
    // regardless of the order in which we are processing input files.
    if (ThisSize > ExistingSize ||
        (ThisSize == ExistingSize &&
         InputOrdinal < Definition_->front().inputOrdinal())) {
      return this->replaceImpl(Lock, Db, Def, InputOrdinal, Fragment);
    }
    return this;
  }
  llvm_unreachable("Unknown linkage type");
}

// update link once symbol
// ~~~~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateLinkOnceSymbol(pstore::database const &Db,
                                  pstore::repo::definition const &Def,
                                  NotNull<UndefsContainer *> const Undefs,
                                  uint32_t InputOrdinal) -> Symbol * {
  assert(Def.linkage() == linkage::link_once_any ||
         Def.linkage() == linkage::link_once_odr);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  if (!Definition_) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }
  assert(Definition_->size() == 1);
  // Link-once may collide with link-once, but nothing else.
  if (!isAnyOf<linkage::link_once_any, linkage::link_once_odr>(
          Definition_->front().linkage())) {
    return nullptr; // Error.
  }

  // If we're an earlier input file than the one
  // which was responsible for this definition, then
  // ours beats it out; if we're later than ours is
  // discarded.
  return this->replaceIfLowerOrdinal(Lock, Db, Def, InputOrdinal);
}

// update weak symbol
// ~~~~~~~~~~~~~~~~~~
auto Symbol::updateWeakSymbol(pstore::database const &Db,
                              pstore::repo::definition const &Def,
                              NotNull<UndefsContainer *> const Undefs,
                              uint32_t InputOrdinal) -> Symbol * {
  assert(Def.linkage() == linkage::weak_any ||
         Def.linkage() == linkage::weak_odr);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  if (!Definition_) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }
  assert(Definition_->size() == 1U &&
         "The should be exactly 1 weak symbol definition");
  switch (Definition_->front().linkage()) {
  case linkage::append:
  case linkage::link_once_any:
  case linkage::link_once_odr:
    // We've already got a definition for this symbol and we're not
    // allowing replacement.
    return nullptr; // Error.
  case linkage::common:
  case linkage::external:
    // If weak collides with common or external, it is ignored.
    return this; // Ignored.
  case linkage::internal:
  case linkage::internal_no_symbol:
    llvm_unreachable("Internal symbols can't collide");
  case linkage::weak_any:
  case linkage::weak_odr:
    // If we're an earlier input file than the one which was responsible
    // for this definition, then ours beats it out; if we're later than
    // ours is discarded.
    return this->replaceIfLowerOrdinal(Lock, Db, Def, InputOrdinal);
  }
  llvm_unreachable("Unknown linkage");
}

// debug dump symbols
// ~~~~~~~~~~~~~~~~~~
void debugDumpSymbols(Context const &Ctx,
                      GlobalSymbolsContainer const &Globals) {
    auto &OS = llvm::dbgs();
    OS << "There are " << Globals.size() << " symbols\n";
    for (auto const &S : Globals) {
      // Note that requesting a symbol's definition returns an owned lock on the
      // object. That means that we need to get the name first since accessing
      // both will try to acquire the same lock.
      StringAddress const Name = S.name();
      auto const X = S.definition();
      auto const &Def = std::get<Symbol::DefinitionIndex>(X);

      auto const IsDefined = Def.hasValue();
      pstore::shared_sstring_view Owner;
      OS << "  " << stringViewAsRef(loadString(Ctx.Db, Name, &Owner))
         << ": defined: " << (IsDefined ? "yes" : "no");
      if (IsDefined) {
        auto Sep = ", ordinals: [";
        for (auto const &Body : *Def) {
          OS << Sep << Body.inputOrdinal();
          Sep = ",";
        }
        OS << ']';
      }
      OS << "\n";
    }
}

//*   ___ _     _          _    ___ _                          *
//*  / __| |___| |__  __ _| |__/ __| |_ ___ _ _ __ _ __ _ ___  *
//* | (_ | / _ \ '_ \/ _` | (_-<__ \  _/ _ \ '_/ _` / _` / -_) *
//*  \___|_\___/_.__/\__,_|_/__/___/\__\___/_| \__,_\__, \___| *
//*                                                 |___/      *
//-MARK: GlobalsStorage
// get thread symbols
// ~~~~~~~~~~~~~~~~~~
NotNull<GlobalSymbolsContainer *> GlobalsStorage::getThreadSymbols() {
  static thread_local char tls = 0;
  auto *const ptr = &tls;
  std::lock_guard<std::mutex> const _{Mut_};
  return &SymbolMemory_.try_emplace(ptr).first->second;
}

// all
// ~~~
rld::GlobalSymbolsContainer GlobalsStorage::all() {
  std::lock_guard<std::mutex> const _{Mut_};
  rld::GlobalSymbolsContainer Result;
  for (auto &&CM : SymbolMemory_) {
    Result.splice(std::move(CM.second));
  }
  SymbolMemory_.clear();
  return Result;
}

//*  _   _         _      __       ___         _        _               *
//* | | | |_ _  __| |___ / _|___  / __|___ _ _| |_ __ _(_)_ _  ___ _ _  *
//* | |_| | ' \/ _` / -_)  _(_-< | (__/ _ \ ' \  _/ _` | | ' \/ -_) '_| *
//*  \___/|_||_\__,_\___|_| /__/  \___\___/_||_\__\__,_|_|_||_\___|_|   *
//*                                                                     *
//-MARK: UndefsContainer
bool UndefsContainer::strongUndefCountIsCorrect() const {
  return std::accumulate(std::begin(List_), std::end(List_), 0UL,
                         [](const unsigned long Acc, const Symbol &S) {
                           return Acc + static_cast<unsigned long>(
                                            !S.allReferencesAreWeak());
                         }) == StrongUndefCount_;
}

//*  ___            _         _   ___             _              *
//* / __|_  _ _ __ | |__  ___| | | _ \___ ___ ___| |_ _____ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ | |   / -_|_-</ _ \ \ V / -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_| |_|_\___/__/\___/_|\_/\___|_|   *
//*      |__/                                                    *
//-MARK: SymbolResolver
// add undefined [static]
// ~~~~~~~~~~~~~
NotNull<Symbol *>
SymbolResolver::addUndefined(const NotNull<GlobalSymbolsContainer *> Globals,
                             const NotNull<UndefsContainer *> Undefs,
                             const StringAddress Name, const size_t NameLength,
                             const pstore::repo::reference_strength Strength) {
  Symbol *const Sym = &Globals->emplace_back(Name, NameLength, Strength);
  Undefs->insert(Sym);
  return Sym;
}

// add reference [static]
// ~~~~~~~~~~~~~
NotNull<Symbol *> SymbolResolver::addReference(
    NotNull<Symbol *> Sym, NotNull<GlobalSymbolsContainer *> Globals,
    NotNull<UndefsContainer *> Undefs, StringAddress Name,
    pstore::repo::reference_strength Strength) {

  if (Sym->addReference(Strength)) {
    Undefs->addStrongUndef();
  }
  return Sym;
}

// add
// ~~~
Symbol *SymbolResolver::add(NotNull<GlobalSymbolsContainer *> const Globals,
                            pstore::repo::definition const &Def,
                            uint32_t InputOrdinal) {
  const size_t Length = stringLength(Context_.Db, Def.name);
  Context_.ELFStringTableSize.fetch_add(Length + 1U, std::memory_order_relaxed);

  return &Globals->emplace_back(
      Def.name, Length,
      Symbol::Body(&Def, pstore::repo::fragment::load(Context_.Db, Def.fext),
                   Def.fext.addr, InputOrdinal));
}

// define symbol
// ~~~~~~~~~~~~~
Symbol *
SymbolResolver::defineSymbol(NotNull<GlobalSymbolsContainer *> const Globals,
                             NotNull<UndefsContainer *> const Undefs,
                             pstore::repo::definition const &Def,
                             uint32_t InputOrdinal) {

  llvmDebug(DebugType, Context_.IOMut, [&] {
    pstore::shared_sstring_view Owner;
    llvm::dbgs() << "> " << Def.name.absolute() << ' '
                 << stringViewAsRef(loadString(Context_.Db, Def.name, &Owner))
                 << '\n';
  });

  // Create a new symbol with definition 'Def'.
  auto AddSymbol = [&]() { return this->add(Globals, Def, InputOrdinal); };

  switch (Def.linkage()) {
  case linkage::append:
    return setSymbolShadow(symbolShadow(Context_, Def.name), AddSymbol,
                           [&](Symbol *const Sym) {
                             return Sym->updateAppendSymbol(
                                 Context_.Db, Def, Undefs, InputOrdinal);
                           });

  case linkage::common:
    return setSymbolShadow(symbolShadow(Context_, Def.name), AddSymbol,
                           [&](Symbol *const Sym) {
                             return Sym->updateCommonSymbol(
                                 Context_.Db, Def, Undefs, InputOrdinal);
                           });

  case linkage::external:
    return setSymbolShadow(symbolShadow(Context_, Def.name), AddSymbol,
                           [&](Symbol *const Sym) {
                             return Sym->updateExternalSymbol(
                                 Context_.Db, Def, Undefs, InputOrdinal);
                           });

  case linkage::internal:
  case linkage::internal_no_symbol:
    return AddSymbol();

  case linkage::link_once_any:
  case linkage::link_once_odr:
    return setSymbolShadow(symbolShadow(Context_, Def.name), AddSymbol,
                           [&](Symbol *const Sym) {
                             return Sym->updateLinkOnceSymbol(
                                 Context_.Db, Def, Undefs, InputOrdinal);
                           });

  case linkage::weak_any:
  case linkage::weak_odr:
    return setSymbolShadow(
        symbolShadow(Context_, Def.name), AddSymbol, [&](Symbol *const Sym) {
          return Sym->updateWeakSymbol(Context_.Db, Def, Undefs, InputOrdinal);
        });
  }
  llvm_unreachable("Unknown linkage");
}

// reference symbol
// ~~~~~~~~~~~~~~~~
NotNull<Symbol *>
referenceSymbol(Context &Ctxt, LocalSymbolsContainer const &Locals,
                NotNull<GlobalSymbolsContainer *> const Globals,
                NotNull<UndefsContainer *> const Undefs, StringAddress Name,
                pstore::repo::reference_strength Strength) {

  // Do we have a local definition for this symbol?
  auto const NamePos = Locals.find(Name);
  if (NamePos != Locals.end()) {
    // Yes, the symbol was defined by this module. Use it.
    return NamePos->second;
  }

  return setSymbolShadow(
      symbolShadow(Ctxt, Name),
      [&]() {
        // Called for a reference to a (thus far) undefined symbol.
        const size_t Length = stringLength(Ctxt.Db, Name);
        Ctxt.ELFStringTableSize.fetch_add(Length + 1U,
                                          std::memory_order_relaxed);
        return SymbolResolver::addUndefined(Globals, Undefs, Name, Length,
                                            Strength);
      },
      [&](Symbol *const Sym) {
        // Called if we see a reference to a symbol already in the symbol table.
        return SymbolResolver::addReference(Sym, Globals, Undefs, Name,
                                            Strength);
      });
}

} // end namespace rld
