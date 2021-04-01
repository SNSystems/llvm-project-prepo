//===- lib/copy.cpp -------------------------------------------------------===//
//*                         *
//*   ___ ___  _ __  _   _  *
//*  / __/ _ \| '_ \| | | | *
//* | (_| (_) | |_) | |_| | *
//*  \___\___/| .__/ \__, | *
//*           |_|    |___/  *
//===----------------------------------------------------------------------===//
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

// Field: word32
// Calculation: S + A
// The R_X86_64_32 and R_X86_64_32S relocations truncate the computed value to
// 32-bits. The linker must verify that the generated value for the R_X86_64_32
// (R_X86_64_32S) relocation zero-extends (sign-extends) to the original 64-bit
// value.
template <>
inline void apply<llvm::ELF::R_X86_64_32>(uint8_t *const Out,
                                          const Symbol *const Sym,
                                          const ExternalFixup &XFixup) {
  const auto S = Sym != nullptr ? Sym->value() : UINT64_C(0);
  const int64_t A = XFixup.addend;
  const auto Value = S + A;
  llvm::support::ulittle32_t::ref{Out} = Value;
}

template <>
inline void apply<llvm::ELF::R_X86_64_32S>(uint8_t *const Out,
                                           const Symbol *const Sym,
                                           const ExternalFixup &XFixup) {
  const auto S = Sym != nullptr ? Sym->value() : UINT64_C(0);
  const int64_t A = XFixup.addend;
  // TODO: range check.
  const auto Value = S + A;
  llvm::support::little32_t::ref{Out} = Value;
}

template <>
inline void apply<llvm::ELF::R_X86_64_PLT32>(uint8_t *const Out,
                                             const Symbol *const Sym,
                                             const ExternalFixup &XFixup) {}

#if 0
template <>
inline void apply<llvm::ELF::R_X86_64_PC32>(uint8_t *const Out,
                                            const Symbol *const Sym,
                                            const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_GOTPCREL>(uint8_t *const Out,
                                                const Symbol *const Sym,
                                                const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_TLSGD>(uint8_t *const Out,
                                             const Symbol *const Sym,
                                             const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_TLSLD>(uint8_t *const Out,
                                             const Symbol *const Sym,
                                             const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_DTPOFF32>(uint8_t *const Out,
                                                const Symbol *const Sym,
                                                const ExternalFixup &XFixup) {}
#endif

template <SectionKind SKind>
void copyContribution(Context &Ctxt, Contribution const &Contribution,
                      uint8_t *Dest) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "copy to "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest)) << '\n';
  });

  using SectionType = typename pstore::repo::enum_to_section<
      ToPstoreSectionKind<SKind>::value>::type;
  auto *const Section =
      reinterpret_cast<SectionType const *>(Contribution.Section);
  auto const &D = Section->payload();
  std::memcpy(Dest, D.begin(), D.size());

  // The contribution's shadow memory contains an array of symbol pointers; one
  // for each external fixup.

  const std::atomic<Symbol *> *XfxSymbol = Contribution.XfxSymbols;
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
void copyContribution<SectionKind::bss>(Context &Ctxt, Contribution const &S,
                                        std::uint8_t *Dest) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "BSS fill "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest)) << '\n';
  });
  static_assert(
      std::is_same<
          pstore::repo::enum_to_section<pstore::repo::section_kind::bss>::type,
          pstore::repo::bss_section>::value,
      "BSS section kind must map to BSS section type");
}

template <>
void copyContribution<SectionKind::linked_definitions>(Context &,
                                                       Contribution const &,
                                                       uint8_t *) {
  // discard
}

template <SectionKind SectionK>
void copySection(Context &Ctxt, const Layout &Lout, uint8_t *Data) {
  for (Contribution const &Contribution :
       Lout.Sections[SectionK].Contributions) {
    llvmDebug(DebugType, Ctxt.IOMut,
              []() { llvm::dbgs() << SectionK << ": "; });

    auto *const Dest = Data + alignTo(Contribution.Offset, Contribution.Align);
    switch (SectionK) {
#define X(a)                                                                   \
  case SectionKind::a:                                                         \
    copyContribution<SectionKind::a>(Ctxt, Contribution, Dest);                \
    break;
      PSTORE_MCREPO_SECTION_KINDS
#undef X
    default:
      llvm_unreachable("Bad section kind");
      break;
    }
  }
}

template <>
void copySection<SectionKind::shstrtab>(Context & /*Ctxt*/,
                                        const Layout & /*Lout*/,
                                        std::uint8_t * /*Dest*/) {}
template <>
void copySection<SectionKind::strtab>(Context & /*Ctxt*/,
                                      const Layout & /*Lout*/,
                                      std::uint8_t * /*Dest*/) {}
template <>
void copySection<SectionKind::symtab>(Context & /*Ctxt*/,
                                      const Layout & /*Lout*/,
                                      std::uint8_t * /*Dest*/) {}

template <>
void copySection<SectionKind::plt>(Context &Ctxt, const Layout &Lout,
                                   std::uint8_t *Dest) {
  // FIXME: build the PLT section
  memset(Dest, 0xFF, (Ctxt.PLTEntries.load() + 1) * 8);
}

static constexpr char const *jobName(const SectionKind SectionK) {
  switch (SectionK) {
#define X(K)                                                                   \
  case SectionKind::K:                                                         \
    return "Copy " #K;
#define RLD_X(a) X(a)
    PSTORE_MCREPO_SECTION_KINDS
    RLD_SECTION_KINDS
#undef RLD_X
#undef X
  default:
    llvm_unreachable("Unknown section kind");
    break;
  }
  return nullptr;
}

bool hasDataToCopy(SectionKind SectionK, const Context &Ctxt,
                   const Layout &Lout) {
  switch (SectionK) {
#define X(K)                                                                   \
  case SectionKind::K:                                                         \
    return !Lout.Sections[SectionKind::K].Contributions.empty();
    PSTORE_MCREPO_SECTION_KINDS
#undef X
  case SectionKind::plt:
    return Ctxt.PLTEntries.load() > 0U;

  case SectionKind::shstrtab:
  case SectionKind::strtab:
  case SectionKind::symtab:
  case SectionKind::last:
    return false;
  }
  llvm_unreachable("Unhandled section type");
}

namespace rld {

void copyToOutput(
    Context &Ctxt, llvm::ThreadPool &Workers, uint8_t *const Data,
    const Layout &Lout,
    const rld::SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    uint64_t TargetDataOffset) {

  forEachSectionKind([&](const SectionKind SectionK) {
    if (!hasDataToCopy(SectionK, Ctxt, Lout)) {
      return;
    }
    assert(SectionFileOffsets[SectionK].hasValue() &&
           "No layout position for a section with contributions");
    Workers.async(
        [SectionK, &Ctxt, Data, &Lout](const uint64_t Start) {
          llvm::NamedRegionTimer CopyTimer(jobName(SectionK), "Copy section",
                                           rld::TimerGroupName,
                                           rld::TimerGroupDescription);

          switch (SectionK) {
#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    copySection<SectionKind::x>(Ctxt, Lout, Data + Start);                     \
    break;
#define RLD_X(x) X(x)
            PSTORE_MCREPO_SECTION_KINDS
            RLD_SECTION_KINDS
#undef RLD_X
#undef X
          case SectionKind::last:
            break;
          }
        },
        TargetDataOffset + *SectionFileOffsets[SectionK]);
  });
}

} // end namespace rld
