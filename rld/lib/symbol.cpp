#include "rld/symbol.h"

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

// replaceIfLowerOrdinal
// ~~~~~~~~~~~~~~~~~~~~~
inline auto
Symbol::replaceIfLowerOrdinal(std::lock_guard<SpinLock> const & /*Lock*/,
                              pstore::database const &Db,
                              pstore::repo::compilation_member const &CM,
                              uint32_t const InputOrdinal) -> Symbol * {
  assert(Definition_ && Definition_->size() == 1U &&
         Definition_->front().inputOrdinal() != InputOrdinal);
  if (InputOrdinal < Definition_->front().inputOrdinal()) {
    auto &ExistingBody = Definition_->front();

    rld::FragmentPtr const Fragment =
        ExistingBody.fragmentAddress() == CM.fext.addr
            ? ExistingBody.fragment()
            : pstore::repo::fragment::load(Db, CM.fext);
    Definition_.emplace(llvm::SmallVector<Body, 1>{
        {Body{CM.linkage(), Fragment, CM.fext.addr, InputOrdinal}}});
  }
  return this;
}

// defineImpl
// ~~~~~~~~~~
inline auto Symbol::defineImpl(std::lock_guard<SpinLock> const & /*Lock*/,
                               pstore::database const &Db,
                               pstore::repo::compilation_member const &CM,
                               NotNull<UndefsContainer *> const Undefs,
                               uint32_t const InputOrdinal) -> Symbol * {
  assert(!Definition_ &&
         "defineImpl was called for a symbol which is already defined");
  Definition_.emplace(llvm::SmallVector<Body, 1>{
      {Body{CM.linkage(), pstore::repo::fragment::load(Db, CM.fext),
            CM.fext.addr, InputOrdinal}}});
  Undefs->remove(this); // remove this symbol from the undef list.
  return this;
}

// replaceImpl
// ~~~~~~~~~~~
inline auto Symbol::replaceImpl(std::lock_guard<SpinLock> const & /*Lock*/,
                                pstore::database const &Db,
                                pstore::repo::compilation_member const &CM,
                                uint32_t const InputOrdinal,
                                FragmentPtr const &Fragment) -> Symbol * {
  assert(Definition_ &&
         "If we're replacing a definition, then we must already have one.");
  Definition_.emplace(llvm::SmallVector<Body, 1>{
      {Body{CM.linkage(), Fragment, CM.fext.addr, InputOrdinal}}});
  return this;
}

inline auto Symbol::replaceImpl(std::lock_guard<SpinLock> const &Lock,
                                pstore::database const &Db,
                                pstore::repo::compilation_member const &CM,
                                uint32_t const InputOrdinal) -> Symbol * {
  return this->replaceImpl(Lock, Db, CM, InputOrdinal,
                           pstore::repo::fragment::load(Db, CM.fext));
}

// update append symbol
// ~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateAppendSymbol(pstore::database const &Db,
                                pstore::repo::compilation_member const &CM,
                                NotNull<UndefsContainer *> const Undefs,
                                uint32_t InputOrdinal) -> Symbol * {
  assert(CM.linkage() == linkage::append);

  std::lock_guard<decltype(Mut_)> const Lock{Mut_};
  // If we don't have a definition, create one.
  if (!Definition_) {
    return this->defineImpl(Lock, Db, CM, Undefs, InputOrdinal);
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
  // essental.
  auto const Pos = std::find_if(
      std::rbegin(Bodies), std::rend(Bodies),
      [=](Symbol::Body const &B) { return B.inputOrdinal() < InputOrdinal; });
  Bodies.insert(Pos.base(),
                Symbol::Body{linkage::append,
                             pstore::repo::fragment::load(Db, CM.fext),
                             CM.fext.addr, InputOrdinal});
  assert(std::is_sorted(std::begin(Bodies), std::end(Bodies),
                        [](Symbol::Body const &A, Symbol::Body const &B) {
                          return A.inputOrdinal() < B.inputOrdinal();
                        }) &&
         "Append symbol bodies must be sorted by input ordinal");
  return this;
}

// updateExternalSymbol
// ~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateExternalSymbol(pstore::database const &Db,
                                  pstore::repo::compilation_member const &CM,
                                  NotNull<UndefsContainer *> const Undefs,
                                  uint32_t InputOrdinal) -> Symbol * {
  assert(CM.linkage() == linkage::external);

  std::lock_guard<decltype(Mut_)> const Lock{Mut_};

  // Do we have a definition of the symbol? If not make one.
  if (!Definition_) {
    return this->defineImpl(Lock, Db, CM, Undefs, InputOrdinal);
  }

  // We've already got a definition for this symbol and we're not
  // allowing replacement.
  assert(Definition_->size() == 1 &&
         "An external symbol definition must have exactly 1 body");
  if (isAnyOf<linkage::append, linkage::external, linkage::link_once_any,
              linkage::link_once_odr>((*Definition_)[0].linkage())) {
    return nullptr; // Error.
  }
  return this->replaceImpl(Lock, Db, CM, InputOrdinal);
}

// updateCommonSymbol
// ~~~~~~~~~~~~~~~~~~
auto Symbol::updateCommonSymbol(pstore::database const &Db,
                                pstore::repo::compilation_member const &CM,
                                NotNull<UndefsContainer *> const Undefs,
                                uint32_t InputOrdinal) -> Symbol * {
  assert(CM.linkage() == linkage::common);

  std::lock_guard<decltype(Mut_)> const Lock{Mut_};
  if (!Definition_) {
    return this->defineImpl(Lock, Db, CM, Undefs, InputOrdinal);
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
    return this->replaceImpl(Lock, Db, CM, InputOrdinal);
  case linkage::common:
    auto const BssSize = [](FragmentPtr const &F) {
      assert(F->has_section(pstore::repo::section_kind::bss));
      return F->at<pstore::repo::section_kind::bss>().size();
    };
    FragmentPtr Fragment = pstore::repo::fragment::load(Db, CM.fext);
    std::size_t const ThisSize = BssSize(Fragment);
    std::size_t const ExistingSize = BssSize(Definition_->front().fragment());

    // Replace the existing definition if this one is larger or from a
    // lower input-ordinal. The latter condition ensures stable output
    // regardless of the order in which we are processing input files.
    if (ThisSize > ExistingSize ||
        (ThisSize == ExistingSize &&
         InputOrdinal < Definition_->front().inputOrdinal())) {
      return this->replaceImpl(Lock, Db, CM, InputOrdinal, Fragment);
    }
    return this;
  }
  llvm_unreachable("Unknown linkage type");
}

// updateLinkOnceSymbol
// ~~~~~~~~~~~~~~~~~~~~
auto Symbol::updateLinkOnceSymbol(pstore::database const &Db,
                                  pstore::repo::compilation_member const &CM,
                                  NotNull<UndefsContainer *> const Undefs,
                                  uint32_t InputOrdinal) -> Symbol * {
  assert(CM.linkage() == linkage::link_once_any ||
         CM.linkage() == linkage::link_once_odr);

  std::lock_guard<decltype(Mut_)> const Lock{Mut_};
  if (!Definition_) {
    return this->defineImpl(Lock, Db, CM, Undefs, InputOrdinal);
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
  return this->replaceIfLowerOrdinal(Lock, Db, CM, InputOrdinal);
}

// updateWeakSymbol
// ~~~~~~~~~~~~~~~~
auto Symbol::updateWeakSymbol(pstore::database const &Db,
                              pstore::repo::compilation_member const &CM,
                              NotNull<UndefsContainer *> const Undefs,
                              uint32_t InputOrdinal) -> Symbol * {
  assert(CM.linkage() == linkage::weak_any ||
         CM.linkage() == linkage::weak_odr);

  std::lock_guard<decltype(Mut_)> const Lock{Mut_};
  if (!Definition_) {
    return this->defineImpl(Lock, Db, CM, Undefs, InputOrdinal);
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
    return this->replaceIfLowerOrdinal(Lock, Db, CM, InputOrdinal);
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
      auto X = S.definition();
      auto const &Def = std::get<Symbol::DefinitionIndex>(X);

      auto const IsDefined = Def.hasValue();
      pstore::shared_sstring_view Owner;
      OS << "  " << stringViewAsRef(loadString(Ctx.Db, S.name(), &Owner))
         << ": defined: " << (IsDefined ? "Yes" : "No");
      if (IsDefined) {
        auto Sep = "ordinals: [";
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

// getThreadSymbols
// ~~~~~~~~~~~~~~~~
NotNull<GlobalSymbolsContainer *> GlobalsStorage::getThreadSymbols() {
  static thread_local char tls = 0;
  auto *const ptr = &tls;
  std::unique_lock<std::mutex> const _{Mut_};
  return &SymbolMemory_.try_emplace(ptr).first->second;
}

// all
// ~~~
rld::GlobalSymbolsContainer GlobalsStorage::all() {
  std::unique_lock<std::mutex> const _{Mut_};
  rld::GlobalSymbolsContainer Result;
  for (auto &&CM : SymbolMemory_) {
    Result.splice(std::move(CM.second));
  }
  SymbolMemory_.clear();
  return Result;
}

//*  ___            _         _   ___             _              *
//* / __|_  _ _ __ | |__  ___| | | _ \___ ___ ___| |_ _____ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ | |   / -_|_-</ _ \ \ V / -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_| |_|_\___/__/\___/_|\_/\___|_|   *
//*      |__/                                                    *

// addUndefined
// ~~~~~~~~~~~~
auto SymbolResolver::addUndefined(
    NotNull<GlobalSymbolsContainer *> const Globals,
    NotNull<UndefsContainer *> const Undefs, StringAddress const Name)
    -> Symbol * {
  Symbol *const Sym = &Globals->emplace_back(Name);
  Undefs->insert(Sym);
  return Sym;
}

// add
// ~~~
auto SymbolResolver::add(NotNull<GlobalSymbolsContainer *> const Globals,
                         pstore::repo::compilation_member const &CM,
                         uint32_t InputOrdinal) -> Symbol * {
  return &Globals->emplace_back(
      CM.name, Symbol::Body(CM.linkage(),
                            pstore::repo::fragment::load(Context_.Db, CM.fext),
                            CM.fext.addr, InputOrdinal));
}

// defineSymbol
// ~~~~~~~~~~~~
Symbol *
SymbolResolver::defineSymbol(NotNull<GlobalSymbolsContainer *> const Globals,
                             NotNull<UndefsContainer *> const Undefs,
                             pstore::repo::compilation_member const &CM,
                             uint32_t InputOrdinal) {

  llvmDebug(DebugType, Context_.IOMut, [&] {
    pstore::shared_sstring_view Owner;
    llvm::dbgs() << "> " << CM.name.absolute() << ' '
                 << stringViewAsRef(loadString(Context_.Db, CM.name, &Owner))
                 << '\n';
  });

  // Defines a symbol whose body is the supplied fragment.
  auto AddSymbol = [&]() { return this->add(Globals, CM, InputOrdinal); };

  switch (CM.linkage()) {
  case linkage::append:
    return setSymbolShadow(
        shadowPointer(Context_, CM.name), AddSymbol, [&](Symbol *const Sym) {
          return Sym->updateAppendSymbol(Context_.Db, CM, Undefs, InputOrdinal);
        });

  case linkage::common:
    return setSymbolShadow(
        shadowPointer(Context_, CM.name), AddSymbol, [&](Symbol *const Sym) {
          return Sym->updateCommonSymbol(Context_.Db, CM, Undefs, InputOrdinal);
        });

  case linkage::external:
    return setSymbolShadow(shadowPointer(Context_, CM.name), AddSymbol,
                           [&](Symbol *const Sym) {
                             return Sym->updateExternalSymbol(
                                 Context_.Db, CM, Undefs, InputOrdinal);
                           });

  case linkage::internal:
  case linkage::internal_no_symbol:
    return AddSymbol();

  case linkage::link_once_any:
  case linkage::link_once_odr:
    return setSymbolShadow(shadowPointer(Context_, CM.name), AddSymbol,
                           [&](Symbol *const Sym) {
                             return Sym->updateLinkOnceSymbol(
                                 Context_.Db, CM, Undefs, InputOrdinal);
                           });

  case linkage::weak_any:
  case linkage::weak_odr:
    return setSymbolShadow(
        shadowPointer(Context_, CM.name), AddSymbol, [&](Symbol *const Sym) {
          return Sym->updateWeakSymbol(Context_.Db, CM, Undefs, InputOrdinal);
        });
  }
  llvm_unreachable("Unknown linkage");
}

Symbol *referenceSymbol(Context &Ctx, StringAddress Name,
                        LocalSymbolsContainer const &Locals,
                        NotNull<GlobalSymbolsContainer *> const Globals,
                        NotNull<UndefsContainer *> const Undefs) {

  // Do we have a local definition for this symbol?
  auto const NamePos = Locals.find(Name);
  if (NamePos != Locals.end()) {
    // Yes, the symbol was defined by this module. Use it.
    return NamePos->second;
  }

  return setSymbolShadow(
      shadowPointer(Ctx, Name),
      [&]() {
        // Called for a reference to a (thus far) undefined symbol.
        return SymbolResolver::addUndefined(Globals, Undefs, Name);
      },
      [](Symbol *const Sym) {
        // Called if we see a reference to a fully defined symbol. Use it.
        return Sym;
      });
}

} // end namespace rld
