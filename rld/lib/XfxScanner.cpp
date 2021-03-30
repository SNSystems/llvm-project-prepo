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

    Symbol *const Sym = referenceSymbol(S.Ctxt, S.Locals, S.Globals, S.Undefs,
                                        Xfx.name, Xfx.strength());

    if (Xfx.type == llvm::ELF::R_X86_64_PLT32) {
      if (Sym->shouldCreatePLTEntry()) {
        S.Ctxt.PLTEntries.fetch_add(1U, std::memory_order_relaxed);

        static std::mutex PLTMut;
        static std::vector<uint8_t> PLT;
        const std::unique_lock<std::mutex> PLTLock{PLTMut};
        if (PLT.empty()) {
          // add the PLT header
          // TODO: hang an object representing the target architecture/ABI from
          // the context and invoke a virtual method on it to do this.
          static const std::array<uint8_t, 16> PLTData{{
              0xff, 0x35, 0, 0, 0, 0, // pushq GOTPLT+8(%rip)
              0xff, 0x25, 0, 0, 0, 0, // jmp *GOTPLT+16(%rip)
              0x0f, 0x1f, 0x40, 0,    // nop
          }};
          PLT.reserve(PLTData.size());
          std::copy(std::begin(PLTData), std::end(PLTData),
                    std::back_inserter(PLT));
          uint8_t *Ptr = &PLT.front();
          //                    memcpy(buf, PLTData, sizeof(PLTData));
          uint64_t GOTPLT = 0; // uint64_t gotPlt = in.gotPlt->getVA();
          uint64_t PLT = 0; // uint64_t plt = in.ibtPlt ? in.ibtPlt->getVA() :
                            // in.plt->getVA();
          llvm::support::endian::write32le(Ptr + 2,
                                           GOTPLT - PLT + 2); // GOTPLT+8
          llvm::support::endian::write32le(Ptr + 8,
                                           GOTPLT - PLT + 4); // GOTPLT+16
        }

        static const std::array<uint8_t, 16> Inst{{
            0xff, 0x25, 0, 0, 0, 0, // jmpq *got(%rip)
            0x68, 0, 0, 0, 0,       // pushq <relocation index>
            0xe9, 0, 0, 0, 0,       // jmpq plt[0]
        }};
        uint8_t *Ptr = &PLT.back() + 1;
        std::copy(std::begin(Inst), std::end(Inst), std::back_inserter(PLT));

        // llvm::support::endian::write32le(buf + 2, sym.getGotPltVA() -
        // pltEntryAddr - 6); llvm::support::endian::write32le(buf + 7,
        // sym.pltIndex); llvm::support::endian::write32le(buf + 12,
        // in.plt->getVA() - pltEntryAddr - 16);

#if 0
            PLT.emplace_back ();
#endif
        static int Count = 0;
        std::cout << "plt " << ++Count << '\n';
      }
    }

    ShadowXfx->store(Sym, std::memory_order_release);
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
