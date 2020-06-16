#include "rld/XfxScanner.h"
#include "pstore/mcrepo/section.hpp"
#include "rld/context.h"

using namespace rld;

namespace {

constexpr auto DebugType = "rld-xfx-scanner";

struct State {
  State(Context &C, LocalSymbolsContainer const &L,
        NotNull<GlobalSymbolsContainer *> const G,
        NotNull<UndefsContainer *> const U)
      : Ctxt{C}, Locals{L}, Globals{G}, Undefs{U} {}

  State(State const &) = delete;
  State &operator=(State const &) = delete;

  Context &Ctxt;
  LocalSymbolsContainer const &Locals;
  NotNull<GlobalSymbolsContainer *> const Globals;
  NotNull<UndefsContainer *> const Undefs;
};

// resolve
// ~~~~~~~
template <pstore::repo::section_kind Kind>
void resolve(State &S,
             pstore::typed_address<pstore::repo::fragment> FragmentAddress,
             const pstore::repo::fragment &Fragment) {

  const auto *const Section = Fragment.atp<Kind>();

  const auto *const FragmentBase = reinterpret_cast<const uint8_t *>(&Fragment);
  const auto *const SectionBase = reinterpret_cast<const uint8_t *>(Section);
  assert(SectionBase > FragmentBase);
  auto const SectionOffset = SectionBase - FragmentBase;
  auto ShadowXfx = UintptrAddress{FragmentAddress.to_address() + SectionOffset};

  for (pstore::repo::external_fixup const &Xfx : Section->xfixups()) {
    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "-> " << loadStdString(S.Ctxt.Db, Xfx.name) << '\n';
    });
    std::atomic<Symbol *> *const Shadow = shadowPointer(S.Ctxt, ShadowXfx++);
    assert(Shadow != nullptr && *Shadow == nullptr);
    *Shadow = referenceSymbol(S.Ctxt, Xfx.name, S.Locals, S.Globals, S.Undefs);
  }
}

template <>
inline void resolve<pstore::repo::section_kind::dependent>(
    State & /*S*/,
    pstore::typed_address<pstore::repo::fragment> /*FragmentAddress*/,
    const pstore::repo::fragment & /*Fragment*/) {}

} // end anonymous namespace

// resolve xfixups
// ~~~~~~~~~~~~~~~
bool rld::resolveXfixups(Context &Context, LocalSymbolsContainer const &Locals,
                         NotNull<rld::GlobalSymbolsContainer *> const Globals,
                         NotNull<UndefsContainer *> const Undefs,
                         uint32_t InputOrdinal) {

  State S{Context, Locals, Globals, Undefs};

  for (auto const &NS : Locals) {
    Symbol const *const Sym = NS.second;
    assert(Sym && "All local definitions must have an associated symbol");

    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "xfx for def \"" << loadStdString(S.Ctxt.Db, Sym->name())
                   << "\"\n";
    });

    auto BodiesAndLock = Sym->definition();
    llvm::Optional<Symbol::BodyContainer> const &Bodies =
        std::get<Symbol::DefinitionIndex>(BodiesAndLock);

    assert(Bodies.hasValue() && "All local symbol definition must have a body");
    assert((Bodies->size() == 1U ||
            (Bodies->size() >= 1U &&
             Bodies->front().linkage() == pstore::repo::linkage::append)) &&
           "A symbol must have 1 body unless it has append linkage");

    for (Symbol::Body const &Def : *Bodies) {
      // If this symbol body came from a different compilation then skip it.
      // TODO: restructure this code so that only definitions from this
      // compilation get this far.
      if (Def.inputOrdinal() != InputOrdinal) {
        continue;
      }
      const std::shared_ptr<const pstore::repo::fragment> &Fragment =
          Def.fragment();
      for (pstore::repo::section_kind Kind : *Fragment) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    resolve<pstore::repo::section_kind::a>(S, Def.fragmentAddress(),           \
                                           *Fragment);                         \
    break;

        switch (Kind) {
          PSTORE_MCREPO_SECTION_KINDS
        case pstore::repo::section_kind::last:
          llvm_unreachable("Bad section kind");
          break;
        }
#undef X
      }
    }
  }

  return true;
}
