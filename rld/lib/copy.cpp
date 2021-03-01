//*                         *
//*   ___ ___  _ __  _   _  *
//*  / __/ _ \| '_ \| | | | *
//* | (_| (_) | |_) | |_| | *
//*  \___\___/| .__/ \__, | *
//*           |_|    |___/  *
//===- lib/copy.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/copy.h"

#include "rld/MathExtras.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/Timer.h"

#include <cassert>
#include <sstream>

using namespace rld;

static constexpr auto DebugType = "rld-copy";

using ExternalFixup = pstore::repo::external_fixup;

template <uint8_t Relocation>
static void apply(uint8_t * /*Out*/, const Symbol *const /*Sym*/,
                  const ExternalFixup & /*XFixup*/) {
  llvm_unreachable("Relocation type is unsupported");
}

template <>
inline void apply<llvm::ELF::R_X86_64_NONE>(uint8_t *, const Symbol *,
                                            const ExternalFixup &) {}

template <>
inline void apply<llvm::ELF::R_X86_64_64>(uint8_t *const Out,
                                          const Symbol *const Sym,
                                          const ExternalFixup &XFixup) {
  const auto S = Sym != nullptr ? Sym->value() : UINT64_C(0);
  const int64_t A = XFixup.addend;
  // TODO: range check.
  auto Value = S + A;
  llvm::support::ulittle64_t::ref{Out} = Value;
}

template <>
inline void apply<llvm::ELF::R_X86_64_32S>(uint8_t *const Out,
                                           const Symbol *const Sym,
                                           const ExternalFixup &XFixup) {
  const auto S = Sym != nullptr ? Sym->value() : UINT64_C(0);
  const int64_t A = XFixup.addend;
  // TODO: range check.
  const auto Value = S + A;
  llvm::support::ulittle32_t::ref{Out} = Value;
}

template <>
inline void apply<llvm::ELF::R_X86_64_PLT32>(uint8_t *const Out,
                                             const Symbol *const Sym,
                                             const ExternalFixup &XFixup) {}


template <pstore::repo::section_kind SKind,
          typename SType = typename pstore::repo::enum_to_section<SKind>::type>
static void copySection(Context &Ctxt, Contribution const &Contribution,
                        uint8_t *Dest) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "copy to "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest)) << '\n';
  });

  auto *const Section = reinterpret_cast<SType const *>(Contribution.Section);
  auto const &D = Section->payload();
  std::memcpy(Dest, D.begin(), D.size());

  // The contribution's shadow memory contains an array of symbol pointers; one
  // for each external fixup.
  auto XfxSymbol = reinterpret_cast<const std::atomic<Symbol *> *>(
      Ctxt.shadow() + Contribution.XfxShadow.absolute());
  for (ExternalFixup const &XFixup : Section->xfixups()) {
    Symbol const *const Sym = XfxSymbol->load();
    assert((Sym != nullptr ||
            XFixup.strength() == pstore::repo::reference_strength::weak) &&
           "An xfixup symbol may only be unresolved if the reference is weak");

    llvmDebug(DebugType, Ctxt.IOMut, [&]() {
      llvm::dbgs() << "  xfx type:" << static_cast<unsigned>(XFixup.type)
                   << " symbol: " << loadStdString(Ctxt.Db, Sym->name())
                   << '\n';
    });

    switch (XFixup.type) {
#define ELF_RELOC(Name, Value)                                                 \
  case llvm::ELF::Name:                                                        \
    apply<llvm::ELF::Name>(Dest + XFixup.offset, Sym, XFixup);                 \
    break;
#include "llvm/BinaryFormat/ELFRelocs/x86_64.def"
#undef ELF_RELOC
    }
    ++XfxSymbol;
  }
}

template <>
void copySection<pstore::repo::section_kind::bss>(Context &Ctxt,
                                                  Contribution const &S,
                                                  std::uint8_t *Dest) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "BSS fill " << reinterpret_cast<std::uintptr_t>(Dest)
                 << '\n';
  });
  static_assert(
      std::is_same<
          pstore::repo::enum_to_section<pstore::repo::section_kind::bss>::type,
          pstore::repo::bss_section>::value,
      "BSS section kind must map to BSS section type");
}

template <>
void copySection<pstore::repo::section_kind::linked_definitions>(
    Context &, Contribution const &, std::uint8_t *) {
  // discard
}

#define X(K)                                                                   \
  case SectionKind::K:                                                         \
    return "Copy " #K;
#define RLD_X(a) X(a)
static constexpr char const *jobName(const SectionKind SectionK) {
  switch (SectionK) {
    PSTORE_MCREPO_SECTION_KINDS
    RLD_SECTION_KINDS
  default:
    llvm_unreachable("Unknown section kind");
    break;
  }
  return nullptr;
}
#undef RLD_X
#undef X

namespace rld {

void copyToOutput(
    Context &Ctxt, llvm::ThreadPool &Workers, uint8_t *const Data,
    const Layout &Lout,
    const rld::SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    uint64_t TargetDataOffset) {

  forEachSectionKind([&](const SectionKind SectionK) {
    const OutputSection::ContributionVector &Contributions =
        Lout.Sections[SectionK].Contributions;
    if (Contributions.empty()) {
      return;
    }
    assert(SectionFileOffsets[SectionK].hasValue() &&
           "No layout position for a section with contributions");
    Workers.async(
        [SectionK, &Ctxt, Data, &Contributions](const uint64_t Start) {
          llvm::NamedRegionTimer CopyTimer(jobName(SectionK), "Copy section",
                                           rld::TimerGroupName,
                                           rld::TimerGroupDescription);
          for (Contribution const &Contribution : Contributions) {
            llvmDebug(DebugType, Ctxt.IOMut,
                      [SectionK]() { llvm::dbgs() << SectionK << ": "; });

            auto *const Dest =
                Data + alignTo(Contribution.Offset, Contribution.Align) + Start;
            switch (SectionK) {
#define X(a)                                                                   \
  case SectionKind::a:                                                         \
    copySection<pstore::repo::section_kind::a>(Ctxt, Contribution, Dest);      \
    break;
#define RLD_X(a)                                                               \
  case SectionKind::a:                                                         \
    break;

              PSTORE_MCREPO_SECTION_KINDS
              RLD_SECTION_KINDS

#undef RLD_X
#undef X
            default:
              llvm_unreachable("Bad section kind");
              break;
            }
          }
        },
        TargetDataOffset + *SectionFileOffsets[SectionK]);
  });
}

} // end namespace rld
