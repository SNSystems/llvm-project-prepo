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

// pstore
#include "pstore/mcrepo/section.hpp"
#include "pstore/mcrepo/section_sparray.hpp"

// LLVM
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"

// rld
#include "rld/CSAlloc.h"
#include "rld/GroupSet.h"
#include "rld/context.h"

// Standard library
#include <array>

using namespace rld;


namespace {

constexpr auto DebugType = "rld-xfx-scanner";

struct State {
  State(Context &C, const NotNull<CompilationSymbolsView *> L,
        const NotNull<GlobalSymbolsContainer *> G,
        const NotNull<UndefsContainer *> U, const NotNull<GroupSet *> NG)
      : Ctxt{C}, Locals{L}, Globals{G}, Undefs{U}, NextGroup{NG} {}

  State(State const &) = delete;
  State &operator=(State const &) = delete;

  Context &Ctxt;
  const NotNull<CompilationSymbolsView *> Locals;
  const NotNull<GlobalSymbolsContainer *> Globals;
  const NotNull<UndefsContainer *> Undefs;
  const NotNull<GroupSet *> NextGroup;
};

static Symbol *symbol(shadow::TaggedPointer TP) {
  if (auto *const CR = TP.get_if<CompilationRef *>()) {
    return CR->Sym;
  }
  assert(TP.get_if<Symbol *>() != nullptr);
  return TP.get_if<Symbol *>();
}

// resolve
// ~~~~~~~
template <pstore::repo::section_kind Kind>
Symbol **resolve(State &S, const pstore::repo::fragment &Fragment,
                 const NotNull<FixupStorage::Container *> ResolvedFixups,
                 const NotNull<GOTPLTContainer *> GOTPLTs) {
  const auto *const Section = Fragment.atp<Kind>();

  pstore::repo::container<pstore::repo::external_fixup> const XFixups =
      Section->xfixups();
  Symbol **Result = csAlloc<Symbol *>(ResolvedFixups.get(), XFixups.size());
  Symbol **Resolved = Result;
  for (pstore::repo::external_fixup const &Xfx : XFixups) {
    llvmDebug(DebugType, S.Ctxt.IOMut, [&]() {
      llvm::dbgs() << "  " << Kind << '+' << Xfx.offset << " -> "
                   << loadStdString(S.Ctxt.Db, Xfx.name) << '\n';
    });

    std::tuple<shadow::TaggedPointer, bool> Ref =
        referenceSymbol(S.Ctxt, *S.Locals, S.Globals, S.Undefs, S.NextGroup,
                        Xfx.name, Xfx.strength());

    Symbol *const Sym = symbol(std::get<shadow::TaggedPointer>(Ref));
    assert(Sym != nullptr && "referenceSymbol must not return nullptr");
    switch (Xfx.type) {

#if 0
      // FIXME: PLT relocation handling is currently disabled.
    case llvm::ELF::R_X86_64_PLT32:
      // Note that there's currently no consideration given to whether this PLT
      // relocation can be relaxed.
      if (Sym->shouldCreatePLTEntry()) {
        PLTSymbols->push_back(Sym);
        S.Ctxt.PLTEntries.fetch_add(1U, std::memory_order_relaxed);
      }
      break;
#endif
    case llvm::ELF::R_X86_64_GOTPCREL:
      if (Sym->shouldCreateGOTEntry()) {
        GOTPLTs->GOT.push_back(Sym);
        S.Ctxt.GOTEntries.fetch_add(1U, std::memory_order_relaxed);
      }
      break;
    }
    *(Resolved++) = Sym;
  }
  return Result;
}

template <>
inline Symbol **resolve<pstore::repo::section_kind::linked_definitions>(
    State & /*S*/, const pstore::repo::fragment & /*Fragment*/,
    const NotNull<FixupStorage::Container *> ResolvedFixups,
    const NotNull<GOTPLTContainer *> GOTPLTs) {

  return nullptr;
}

} // end anonymous namespace

namespace {

/// \tparam NewFragmentFunction A function with signature compatible with
/// `void(Symbol::Body &)`.
template <typename NewFragmentFunction>
void forEachNewFragment(Context &Context,
                        const NotNull<CompilationSymbolsView *> Locals,
                        const uint32_t InputOrdinal, NewFragmentFunction f) {
  for (auto &NS : Locals->Map) {
    Symbol *const Sym = NS.second.Sym;
    assert(Sym && "All local definitions must have an associated symbol");

    //    llvmDebug(DebugType, Context.IOMut, [&]() {
    //      llvm::dbgs() << "fixups for def \""
    //                   << loadStdString(Context.Db, Sym->name()) << "\"\n";
    //    });

    auto BodiesAndLock = Sym->definition();
    auto &OptionalBodies = std::get<Symbol::OptionalBodies &>(BodiesAndLock);
    assert(OptionalBodies.hasValue() &&
           "All local symbol definitions must have a body");
    auto &Bodies = OptionalBodies.getValue();
    assert(Bodies.size() >= 1U && "Defined symbols must have a body");
    if (Bodies.size() == 1U) {
      auto &Body = Bodies[0];
      if (Body.inputOrdinal() == InputOrdinal || Body.hasLocalLinkage()) {
        f(NS, Body);
      }
    } else {
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
      f(NS, *Pos);
    }
  }
}

} // end anonymous namespace

// externals
// ~~~~~~~~~
static void externals(State &S, Symbol::Body &Def,
                      const NotNull<FixupStorage::Container *> ResolvedFixups,
                      uint32_t InputOrdinal,
                      const NotNull<GOTPLTContainer *> GOTPLTs) {

  const FragmentPtr &Fragment = Def.fragment();
  Symbol::Body::ResType &ResolveMap = Def.resolveMap();
  for (const pstore::repo::section_kind Kind : *Fragment) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    ResolveMap[pstore::repo::section_kind::a] =                                \
        resolve<pstore::repo::section_kind::a>(S, *Fragment, ResolvedFixups,   \
                                               GOTPLTs);                       \
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

// internals
// ~~~~~~~~~
static ContributionSpArray *
internals(Symbol::Body &Def, const NotNull<FixupStorage::Container *> Storage) {
  const FragmentPtr &Fragment = Def.fragment();
  const size_t NumSections = Fragment->size();
  // FIXME: correct this space optimization
  //  if (NumSections < 2U && the sole section has no internal fixups) {
  //    return nullptr;
  //  }

  auto *const Ptr =
      csAlloc(Storage.get(), ContributionSpArray::size_bytes(NumSections),
              alignof(ContributionSpArray));
  assert(reinterpret_cast<std::uintptr_t>(Ptr) % alignof(ContributionSpArray) ==
             0 &&
         "Storage must be aligned correctly");
  return new (Ptr)
      ContributionSpArray(std::begin(*Fragment), std::end(*Fragment));
}

// resolve fixups
// ~~~~~~~~~~~~~~
GOTPLTContainer rld::resolveFixups(
    Context &Context, const NotNull<CompilationSymbolsView *> Locals,
    const NotNull<GlobalSymbolsContainer *> Globals,
    const NotNull<UndefsContainer *> Undefs, uint32_t InputOrdinal,
    const NotNull<FixupStorage::Container *> Storage,
    const NotNull<GroupSet *> NextGroup) {

  GOTPLTContainer GOTPLTs;
  State S{Context, Locals, Globals, Undefs, NextGroup};

  forEachNewFragment(Context, Locals, InputOrdinal,
                     [&](CompilationSymbolsView::Container::value_type &NS,
                         Symbol::Body &Def) {
                       externals(S, Def, Storage, InputOrdinal, &GOTPLTs);
                       NS.second.Ifx = internals(Def, Storage);
                     });
  return GOTPLTs;
}
