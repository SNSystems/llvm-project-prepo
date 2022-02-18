//===- lib/Symbol.cpp -----------------------------------------------------===//
//*  ____                  _           _  *
//* / ___| _   _ _ __ ___ | |__   ___ | | *
//* \___ \| | | | '_ ` _ \| '_ \ / _ \| | *
//*  ___) | |_| | | | | | | |_) | (_) | | *
//* |____/ \__, |_| |_| |_|_.__/ \___/|_| *
//*        |___/                          *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/Symbol.h"

#include "rld/Contribution.h"
#include "rld/GroupSet.h"
#include "rld/OutputSection.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

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
  std::tuple<const Contents &, std::unique_lock<Mutex>> ContentsAndLock =
      this->contentsAndLock();
  auto const &OptBodies = std::get<const Contents &>(ContentsAndLock);
  if (!holdsAlternative<BodyContainer>(OptBodies)) {
    return;
  }

  auto const &Bodies = get<BodyContainer>(OptBodies);

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
  assert(holdsAlternative<BodyContainer>(Contents_) &&
         get<BodyContainer>(Contents_).size() == 1U &&
         get<BodyContainer>(Contents_).front().inputOrdinal() != InputOrdinal);

  Body &ExistingBody = get<BodyContainer>(Contents_).front();
  if (InputOrdinal >= ExistingBody.inputOrdinal()) {
    return this; // Keep the existing definition.
  }
  ExistingBody = Symbol::Body{&Def,
                              ExistingBody.fragmentAddress() == Def.fext.addr
                                  ? ExistingBody.fragment()
                                  : pstore::repo::fragment::load(Db, Def.fext),
                              Def.fext.addr, InputOrdinal};
  return this; // Use this definition instead.
}

// define impl
// ~~~~~~~~~~~
inline Symbol *Symbol::defineImpl(std::unique_lock<SpinLock> &&Lock,
                                  const pstore::database &Db,
                                  const pstore::repo::definition &Def,
                                  const NotNull<UndefsContainer *> Undefs,
                                  const uint32_t InputOrdinal) {
  (void)Lock;
  assert(Lock.owns_lock());
  assert(holdsAlternative<Reference>(Contents_) &&
         "defineImpl was called for a symbol which is already defined");

  {
    BodyContainer B;
    B.emplace_back(&Def, pstore::repo::fragment::load(Db, Def.fext),
                   Def.fext.addr, InputOrdinal);
    Contents_.emplace<BodyContainer>(std::move(B));
  }

  const bool WasWeakUndefined = WeakUndefined_;
  // A weakly undefined sybol must not have a definition so here we ensure that
  // this symbol is not tagged as weakly undefined.
  WeakUndefined_ = false;
  Lock.unlock();

  // Remove this symbol from the undef list.
  Undefs->remove(this, WasWeakUndefined ? pstore::repo::binding::weak
                                        : pstore::repo::binding::strong);
  return this;
}

// replace impl
// ~~~~~~~~~~~~
inline Symbol *Symbol::replaceImpl(const std::unique_lock<SpinLock> &Lock,
                                   const pstore::database &Db,
                                   const pstore::repo::definition &Def,
                                   const uint32_t InputOrdinal,
                                   FragmentPtr &&Fragment) {
  (void)Lock;
  assert(Lock.owns_lock());
  assert(holdsAlternative<BodyContainer>(Contents_) &&
         "If we're replacing a definition, then we must already have one.");

  BodyContainer B;
  B.emplace_back(&Def, std::move(Fragment), Def.fext.addr, InputOrdinal);
  Contents_.emplace<BodyContainer>(std::move(B));
  return this;
}

inline Symbol *Symbol::replaceImpl(const std::unique_lock<SpinLock> &Lock,
                                   const pstore::database &Db,
                                   const pstore::repo::definition &Def,
                                   const uint32_t InputOrdinal) {
  return this->replaceImpl(Lock, Db, Def, InputOrdinal,
                           pstore::repo::fragment::load(Db, Def.fext));
}

// update append symbol
// ~~~~~~~~~~~~~~~~~~~~
Symbol *Symbol::updateAppendSymbol(const pstore::database &Db,
                                   const pstore::repo::definition &Def,
                                   const NotNull<UndefsContainer *> Undefs,
                                   const uint32_t InputOrdinal) {
  assert(Def.linkage() == linkage::append);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  // If we don't have a definition, create one.
  if (holdsAlternative<Reference>(Contents_)) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }

  // Add this symbol to the existing sorted vector for this symbol.
  auto &Bodies = get<BodyContainer>(Contents_);

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
Symbol *Symbol::updateExternalSymbol(const pstore::database &Db,
                                     const pstore::repo::definition &Def,
                                     const NotNull<UndefsContainer *> Undefs,
                                     const uint32_t InputOrdinal) {
  assert(Def.linkage() == linkage::external);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};

  // Do we have a definition of the symbol? If not make one.
  if (holdsAlternative<Reference>(Contents_)) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }

  // We've already got a definition for this symbol and we're not
  // allowing replacement.
  auto &D = get<BodyContainer>(Contents_);
  assert(D.size() == 1 &&
         "An external symbol definition must have exactly 1 body");
  if (isAnyOf<linkage::append, linkage::external>(D[0].linkage())) {
    return nullptr; // Error.
  }
  return this->replaceImpl(Lock, Db, Def, InputOrdinal);
}

// update common symbol
// ~~~~~~~~~~~~~~~~~~~~
Symbol *Symbol::updateCommonSymbol(const pstore::database &Db,
                                   const pstore::repo::definition &Def,
                                   const NotNull<UndefsContainer *> Undefs,
                                   const uint32_t InputOrdinal) {
  assert(Def.linkage() == linkage::common);

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  if (holdsAlternative<Reference>(Contents_)) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }
  // If we have already have an external definition of this symbol, we
  // just use it,
  auto &D = get<BodyContainer>(Contents_);
  assert(D.size() == 1 &&
         "A common symbol should only ever have a single body");
  switch (D[0].linkage()) {
  case linkage::external:
    return this; // common hits external: ignored
  case linkage::append:
    return nullptr; // common hits append: error
  case linkage::internal:
  case linkage::internal_no_symbol:
    llvm_unreachable("Internal symbols can't collide");
  case linkage::link_once_any:
  case linkage::link_once_odr:
  case linkage::weak_any:
  case linkage::weak_odr:
    return this->replaceImpl(Lock, Db, Def, InputOrdinal);
  case linkage::common:
    auto const BssSize = [](FragmentPtr const &F) {
      assert(F->has_section(pstore::repo::section_kind::bss));
      return F->at<pstore::repo::section_kind::bss>().size();
    };

    Symbol::Body &B = D.front();
    if (B.fragmentAddress() == Def.fext.addr) {
      // The same fragment as the existing definition so therefore the same
      // size.
      return this->replaceIfLowerOrdinal(Lock, Db, Def, InputOrdinal);
    }

    FragmentPtr Fragment = pstore::repo::fragment::load(Db, Def.fext);
    std::size_t const ThisSize = BssSize(Fragment);
    std::size_t const ExistingSize = BssSize(B.fragment());

    // Replace the existing definition if this one is larger or from a
    // lower input-ordinal. The latter condition ensures stable output
    // regardless of the order in which we are processing input files.
    if (ThisSize > ExistingSize ||
        (ThisSize == ExistingSize && InputOrdinal < B.inputOrdinal())) {
      return this->replaceImpl(Lock, Db, Def, InputOrdinal,
                               std::move(Fragment));
    }
    // Not larger and not a later input file. Ignored.
    return this;
  }
  llvm_unreachable("Unknown linkage type");
}

// update weak symbol
// ~~~~~~~~~~~~~~~~~~
Symbol *Symbol::updateWeakSymbol(const pstore::database &Db,
                                 const pstore::repo::definition &Def,
                                 const NotNull<UndefsContainer *> Undefs,
                                 const uint32_t InputOrdinal) {
  assert((isAnyOf<linkage::link_once_any, linkage::link_once_odr,
                  linkage::weak_any, linkage::weak_odr>(Def.linkage())));

  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  if (holdsAlternative<Reference>(Contents_)) {
    return this->defineImpl(std::move(Lock), Db, Def, Undefs, InputOrdinal);
  }
  auto &D = get<BodyContainer>(Contents_);
  assert(D.size() == 1 && "A weak symbol should only ever have a single body");
  switch (D.front().linkage()) {
  case linkage::append:
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
  case linkage::link_once_any:
  case linkage::link_once_odr:
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
    auto const Name = S.name();
    auto const ContentsAndLock = S.contentsAndLock();
    auto const &Contents = std::get<const Symbol::Contents &>(ContentsAndLock);

    auto const IsDefined = holdsAlternative<Symbol::BodyContainer>(Contents);
    pstore::shared_sstring_view Owner;
    OS << "  " << stringViewAsRef(loadString(Ctx.Db, Name, &Owner))
       << ": defined: " << (IsDefined ? "yes" : "no");
    if (IsDefined) {
      auto Sep = ", ordinals: [";
      for (auto const &Body : get<Symbol::BodyContainer>(Contents)) {
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
// get thread storage
// ~~~~~~~~~~~~~~~~~~
NotNull<GlobalSymbolsContainer *> GlobalsStorage::getThreadStorage() {
  static thread_local char tls = 0;
  auto *const ptr = &tls;
  const std::lock_guard<std::mutex> _{Mut_};
  return &SymbolMemory_.try_emplace(ptr).first->second;
}

// all
// ~~~
GlobalSymbolsContainer GlobalsStorage::all() {
  const std::lock_guard<std::mutex> _{Mut_};
  GlobalSymbolsContainer Result;
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
  const auto Count =
      std::accumulate(std::begin(List_), std::end(List_), 0UL,
                      [](const unsigned long Acc, const Symbol &S) {
                        const bool IsStrongUndef =
                            !S.isDefinition() && !S.allReferencesAreWeak();
                        return Acc + static_cast<unsigned long>(IsStrongUndef);
                      });
  return Count == StrongUndefCount_;
}

//*  ___            _         _   ___             _              *
//* / __|_  _ _ __ | |__  ___| | | _ \___ ___ ___| |_ _____ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ | |   / -_|_-</ _ \ \ V / -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_| |_|_\___/__/\___/_|\_/\___|_|   *
//*      |__/                                                    *
//-MARK: SymbolResolver
// add undefined [static]
// ~~~~~~~~~~~~~
NotNull<Symbol *> SymbolResolver::addUndefined(
    const NotNull<GlobalSymbolsContainer *> Globals,
    const NotNull<UndefsContainer *> Undefs, const pstore::address Name,
    const size_t NameLength, const pstore::repo::binding Binding,
    uint32_t InputOrdinal) {
  Symbol *const Sym =
      &Globals->emplace_back(Name, NameLength, Binding, InputOrdinal);
  Undefs->insert(Sym);
  return Sym;
}

// add reference [static]
// ~~~~~~~~~~~~~
NotNull<Symbol *> SymbolResolver::addReference(
    NotNull<Symbol *> Sym, NotNull<GlobalSymbolsContainer *> Globals,
    NotNull<UndefsContainer *> Undefs, StringAddress Name,
    const pstore::repo::binding Binding, const uint32_t InputOrdinal) {

  if (Sym->addReference(Binding, InputOrdinal)) {
    Undefs->addStrongUndef();
  }
  return Sym;
}

// add
// ~~~
NotNull<Symbol *>
SymbolResolver::add(const NotNull<GlobalSymbolsContainer *> Globals,
                    const pstore::address Name, const size_t Length,
                    const pstore::repo::definition &Def,
                    const uint32_t InputOrdinal) {
  return &Globals->emplace_back(
      Name, Length,
      Symbol::Body(&Def, pstore::repo::fragment::load(Context_.Db, Def.fext),
                   Def.fext.addr, InputOrdinal));
}

// update symbol
// ~~~~~~~~~~~~~
Symbol *SymbolResolver::updateSymbol(Symbol *const Sym,
                                     const NotNull<UndefsContainer *> Undefs,
                                     const pstore::repo::definition &Def,
                                     const uint32_t InputOrdinal) {
  switch (Def.linkage()) {
  case linkage::append:
    return Sym->updateAppendSymbol(Context_.Db, Def, Undefs, InputOrdinal);
  case linkage::common:
    return Sym->updateCommonSymbol(Context_.Db, Def, Undefs, InputOrdinal);
  case linkage::external:
    return Sym->updateExternalSymbol(Context_.Db, Def, Undefs, InputOrdinal);
  case linkage::link_once_any:
  case linkage::link_once_odr:
  case linkage::weak_any:
  case linkage::weak_odr:
    return Sym->updateWeakSymbol(Context_.Db, Def, Undefs, InputOrdinal);
  case linkage::internal:
  case linkage::internal_no_symbol:
    llvm_unreachable("Can't update a symbol with internal linkage");
  }
  return Sym;
}

// as symbol bool
// ~~~~~~~~~~~~~~
static auto asSymbolBool(std::tuple<shadow::TaggedPointer, bool> &&T) {
  return std::make_tuple(
      NotNull<Symbol *>{std::get<shadow::TaggedPointer>(T).get_if<Symbol *>()},
      std::get<bool>(T));
}

// define symbol
// ~~~~~~~~~~~~~
std::tuple<NotNull<Symbol *>, bool>
SymbolResolver::defineSymbol(const NotNull<GlobalSymbolsContainer *> Globals,
                             const NotNull<UndefsContainer *> Undefs,
                             const pstore::repo::definition &Def,
                             const uint32_t InputOrdinal) {

  llvmDebug(DebugType, Context_.IOMut, [&] {
    pstore::shared_sstring_view Owner;
    llvm::dbgs() << "> " << Def.name.absolute() << ' '
                 << stringViewAsRef(loadString(Context_.Db, Def.name, &Owner))
                 << '\n';
  });

  // Create a new symbol with definition 'Def'.
  const auto AddSymbol = [&] {
    llvmDebug(DebugType, Context_.IOMut, [&] {
      llvm::dbgs() << "  Create def: " << loadStdString(Context_.Db, Def.name)
                   << '\n';
    });
    const auto IndirStr = pstore::indirect_string::read(Context_.Db, Def.name);
    const size_t Length = IndirStr.length();
    Context_.ELFStringTableSize.fetch_add(Length + 1U,
                                          std::memory_order_relaxed);
    return shadow::TaggedPointer{this->add(Globals, IndirStr.in_store_address(),
                                           Length, Def, InputOrdinal)};
  };
  const auto Update = [&](shadow::AtomicTaggedPointer *, Symbol *const Sym) {
    llvmDebug(DebugType, Context_.IOMut, [&] {
      llvm::dbgs() << "  Update symbol: "
                   << loadStdString(Context_.Db, Sym->name()) << '\n';
    });
    return shadow::TaggedPointer{
        this->updateSymbol(Sym, Undefs, Def, InputOrdinal)};
  };
  // Add a symbol which has the same name as a CompilationRef. The symbol
  // wins...
  const auto AddSymbolFromCompilationRef =
      [&](shadow::AtomicTaggedPointer *const P, CompilationRef *const CR) {
        llvmDebug(DebugType, Context_.IOMut, [&] {
          llvm::dbgs() << "  CompilationRef to Symbol: "
                       << loadStdString(Context_.Db, Def.name) << '\n';
        });
        return CR->Sym == nullptr ? AddSymbol() : Update(P, CR->Sym);
      };

  if (isLocalLinkage(Def.linkage())) {
    return std::make_tuple(AddSymbol().get_if<Symbol *>(), true);
  }
  return asSymbolBool(shadow::set(Context_.shadowPointer(Def.name), AddSymbol,
                                  AddSymbolFromCompilationRef, Update));
}

// reference symbol
// ~~~~~~~~~~~~~~~~
std::tuple<shadow::TaggedPointer, bool>
referenceSymbol(Context &Ctxt, const CompilationSymbolsView &Locals,
                const NotNull<GlobalSymbolsContainer *> Globals,
                const NotNull<UndefsContainer *> Undefs,
                const NotNull<GroupSet *> NextGroup, StringAddress Name,
                const pstore::repo::binding Binding,
                const uint32_t InputOrdinal) {

  // Do we have a local definition for this symbol?
  const auto NamePos = Locals.Map.find(Name);
  if (NamePos != Locals.Map.end()) {
    // Yes, the symbol was defined by this module. Use it.
    return std::make_tuple(shadow::TaggedPointer{NamePos->second.Sym}, true);
  }

  // Called for a reference to an as yet undefined symbol.
  const auto CreateUndef = [&] {
    const auto IndirStr = pstore::indirect_string::read(Ctxt.Db, Name);
    const size_t Length = IndirStr.length();
    Ctxt.ELFStringTableSize.fetch_add(Length + 1U, std::memory_order_relaxed);
    return SymbolResolver::addUndefined(Globals, Undefs,
                                        IndirStr.in_store_address(), Length,
                                        Binding, InputOrdinal);
  };
  // Despite what its name suggests, this function does not create an undef
  // symbol. We need to keep the CompilationRef record in the shadow memory in
  // order that the we can get the correct compilation when it comes time to
  // turn 'NextGroup' into the set of compilations for the next iteration. Bear
  // in mind that a specific CompilationRef record can be replaced if we later
  // find a definition in a library member with an earlier position than the one
  // we have here.
  const auto CreateUndefFromCompilationRef =
      [&](shadow::AtomicTaggedPointer *const P, CompilationRef *const CR) {
        NextGroup->insert(P);
        if (CR->Sym == nullptr) {
          CR->Sym = CreateUndef();
        }
        return shadow::TaggedPointer{CR};
      };
  const auto Update = [&](shadow::AtomicTaggedPointer *, Symbol *const Sym) {
    // Called if we see a reference to a symbol already in the symbol
    // table.
    return shadow::TaggedPointer{SymbolResolver::addReference(
        Sym, Globals, Undefs, Name, Binding, InputOrdinal)};
  };

  return shadow::set(
      Ctxt.shadowPointer(Name),
      [&] { return shadow::TaggedPointer{CreateUndef()}; },
      CreateUndefFromCompilationRef, Update);
}

} // end namespace rld
