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
void resolve(State &S, const pstore::repo::fragment &Fragment,
             FragmentShadow FS) {
  std::atomic<Symbol *> *ShadowXfx = FS.xfxSymbols<Kind>(Fragment);
  const auto *const Section = Fragment.atp<Kind>();

  for (pstore::repo::external_fixup const &Xfx : Section->xfixups()) {
    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "-> " << loadStdString(S.Ctxt.Db, Xfx.name) << '\n';
    });
    assert(ShadowXfx->load(std::memory_order_acquire) == nullptr &&
           "An xfixup has already been resolved");
    ShadowXfx->store(referenceSymbol(S.Ctxt, S.Locals, S.Globals, S.Undefs,
                                     Xfx.name, Xfx.strength()),
                     std::memory_order_release);
    ++ShadowXfx;
  }
}

template <>
inline void resolve<pstore::repo::section_kind::linked_definitions>(
    State & /*S*/, const pstore::repo::fragment & /*Fragment*/,
    FragmentShadow /*FS*/) {}

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

    Sym->checkInvariants();

    auto BodiesAndLock = Sym->definition();
    llvm::Optional<Symbol::BodyContainer> const &OptionalBodies =
        std::get<Symbol::DefinitionIndex>(BodiesAndLock);
    assert(OptionalBodies.hasValue() &&
           "All local symbol definitions must have a body");
    auto const &Bodies = OptionalBodies.getValue();
    const auto BodiesEnd = std::end(Bodies);
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

    FragmentShadow FS =
        FragmentShadow::make(Context, InputOrdinal, Def.fragmentAddress());
    // Have we processed this fragment already?
    if (!FS.markDone()) {
      return true;
    }
    const std::shared_ptr<const pstore::repo::fragment> &Fragment =
        Def.fragment();
    for (const pstore::repo::section_kind Kind : *Fragment) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    resolve<pstore::repo::section_kind::a>(S, *Fragment, FS);                  \
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
