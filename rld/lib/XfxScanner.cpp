//===- lib/XfxScanner.cpp -------------------------------------------------===//
//* __  __ __        ____                                   *
//* \ \/ // _|_  __ / ___|  ___ __ _ _ __  _ __   ___ _ __  *
//*  \  /| |_\ \/ / \___ \ / __/ _` | '_ \| '_ \ / _ \ '__| *
//*  /  \|  _|>  <   ___) | (_| (_| | | | | | | |  __/ |    *
//* /_/\_\_| /_/\_\ |____/ \___\__,_|_| |_|_| |_|\___|_|    *
//*                                                         *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
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
void resolve(State &S, FragmentAddress FAddr,
             const pstore::repo::fragment &Fragment) {

  const auto *const Section = Fragment.atp<Kind>();

  const auto *const FragmentBase = reinterpret_cast<const uint8_t *>(&Fragment);
  const auto *const SectionBase = reinterpret_cast<const uint8_t *>(Section);
  assert(SectionBase > FragmentBase);
  auto const SectionOffset = SectionBase - FragmentBase;
  auto ShadowXfx = UintptrAddress{FAddr.to_address() + SectionOffset};

  for (pstore::repo::external_fixup const &Xfx : Section->xfixups()) {
    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "-> " << loadStdString(S.Ctxt.Db, Xfx.name) << '\n';
    });
    std::atomic<Symbol *> *const Shadow = symbolShadow(S.Ctxt, ShadowXfx++);
    assert(Shadow != nullptr && "No shadow memory for an xfixup!");
    assert(Shadow->load(std::memory_order_acquire) == nullptr &&
           "An xfixup has already been resolved");
    // TODO: get the ordering right for this atomic write.
    Shadow->store(referenceSymbol(S.Ctxt, S.Locals, S.Globals, S.Undefs,
                                  Xfx.name, Xfx.strength()),
                  std::memory_order_release);
  }
}

template <>
inline void resolve<pstore::repo::section_kind::linked_definitions>(
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
    llvm::Optional<Symbol::BodyContainer> const &OptionalBodies =
        std::get<Symbol::DefinitionIndex>(BodiesAndLock);

    assert(OptionalBodies.hasValue() &&
           "All local symbol definition must have a body");
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

    // Find the symbol body that is associated with this compilation.
    const auto Pos =
        std::lower_bound(std::begin(Bodies), BodiesEnd, InputOrdinal,
                         [](Symbol::Body const &A, uint32_t Ord) {
                           return A.inputOrdinal() < Ord;
                         });
    if (Pos == BodiesEnd) {
      return true;
    }

    Symbol::Body const &Def = *Pos;
    assert(Def.inputOrdinal() == InputOrdinal);

    const std::shared_ptr<const pstore::repo::fragment> &Fragment =
        Def.fragment();
    auto *const IsDone = reinterpret_cast<std::atomic<bool> *>(
        S.Ctxt.shadow() + Def.fragmentAddress().absolute());
    bool Expected = false;
    if (!IsDone->compare_exchange_strong(Expected, true)) {
      return true;
    }

    for (const pstore::repo::section_kind Kind : *Fragment) {
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

  return true;
}
