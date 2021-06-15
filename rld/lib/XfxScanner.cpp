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

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"

#include "rld/context.h"

#include <array>

using namespace rld;

using SymbolPtrContainer = pstore::chunked_vector<Symbol *>;

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
Symbol **resolve(State &S, const pstore::repo::fragment &Fragment,
                 const NotNull<SymbolPtrContainer *> ResolvedFixups,
                 const NotNull<LocalPLTsContainer *> PLTSymbols) {
  const auto *const Section = Fragment.atp<Kind>();

  pstore::repo::container<pstore::repo::external_fixup> const XFixups =
      Section->xfixups();
  Symbol **Result = reserveContiguous(ResolvedFixups.get(), XFixups.size());
  Symbol **Resolved = Result;
  for (pstore::repo::external_fixup const &Xfx : XFixups) {
    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "  " << Kind << '+' << Xfx.offset << " -> "
                   << loadStdString(S.Ctxt.Db, Xfx.name) << '\n';
    });

    Symbol *const Sym = referenceSymbol(S.Ctxt, S.Locals, S.Globals, S.Undefs,
                                        Xfx.name, Xfx.strength());
    assert(Sym != nullptr && "referenceSymbol must not return nullptr");
    // FIXME: PLT relocation handling is currently disabled.
#if 0
    if (Xfx.type == llvm::ELF::R_X86_64_PLT32) {
      // Note that there's currently no consideration given to whether this PLT
      // relocation can be relaxed.
      if (Sym->shouldCreatePLTEntry()) {
        PLTSymbols->push_back(Sym);
        S.Ctxt.PLTEntries.fetch_add(1U, std::memory_order_relaxed);
      }
    }
#endif
    *(Resolved++) = Sym;
  }
  return Result;
}

template <>
inline Symbol **resolve<pstore::repo::section_kind::linked_definitions>(
    State & /*S*/, const pstore::repo::fragment & /*Fragment*/,
    const NotNull<SymbolPtrContainer *> ResolvedFixups,
    const NotNull<LocalPLTsContainer *> /* PLTSymbols*/) {

  return nullptr;
}

} // end anonymous namespace

// resolve xfixups
// ~~~~~~~~~~~~~~~
LocalPLTsContainer
rld::resolveXfixups(Context &Context, const LocalSymbolsContainer &Locals,
                    const NotNull<GlobalSymbolsContainer *> Globals,
                    const NotNull<UndefsContainer *> Undefs,
                    const NotNull<SymbolPtrContainer *> ResolvedFixups,
                    uint32_t InputOrdinal) {

  LocalPLTsContainer PLTSymbols;
  State S{Context, Locals, Globals, Undefs};

  for (auto &NS : Locals) {
    std::pair<Symbol *, bool> const &SymDef = NS.second;
    Symbol *const Sym = SymDef.first;
    assert(Sym && "All local definitions must have an associated symbol");

    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "xfx for def \"" << loadStdString(S.Ctxt.Db, Sym->name())
                   << "\"\n";
    });

    if (!SymDef.second) {
      // We re-used a definition from another compilation. No need to redo the
      // symbol resolution for its fixups.
      llvmDebug(DebugType, S.Ctxt.IOMut,
                [&]() { llvm::dbgs() << "  skipped\n"; });
      continue;
    }

    Sym->checkInvariants();

    auto BodiesAndLock = Sym->definition();
    llvm::Optional<Symbol::BodyContainer> &OptionalBodies =
        std::get<Symbol::DefinitionIndex>(BodiesAndLock);
    assert(OptionalBodies.hasValue() &&
           "All local symbol definitions must have a body");
    auto &Bodies = OptionalBodies.getValue();
    const auto BodiesEnd = std::end(Bodies);
    // Find the symbol body that is associated with this compilation.
    auto Pos = std::lower_bound(std::begin(Bodies), BodiesEnd, InputOrdinal,
                                [](Symbol::Body const &A, uint32_t Ord) {
                                  return A.inputOrdinal() < Ord;
                                });
    if (Pos == BodiesEnd || Pos->inputOrdinal() != InputOrdinal) {
      assert(false && "We found no associated body");
      continue;
    }

    Symbol::Body &Def = *Pos;

    const FragmentPtr &Fragment = Def.fragment();
    Symbol::Body::ResType &ResolveMap = Def.resolveMap();
    for (const pstore::repo::section_kind Kind : *Fragment) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    ResolveMap[pstore::repo::section_kind::a] =                                \
        resolve<pstore::repo::section_kind::a>(S, *Fragment, ResolvedFixups,   \
                                               &PLTSymbols);                   \
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

  return PLTSymbols;
}
