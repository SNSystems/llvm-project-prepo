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
using InternalFixup = pstore::repo::internal_fixup;

template <uint8_t Relocation>
static void applyExternal(uint8_t *const Out, const Contribution &Src,
                          const Layout &Layout, const Symbol &Sym,
                          const ExternalFixup &XFixup) {
  llvm_unreachable("Relocation type is unsupported");
}

template <uint8_t Relocation>
static void applyInternal(uint8_t *const Out, const Contribution &Src,
                          const Layout &Layout, const Contribution &Target,
                          const InternalFixup &Fixup) {
  llvm_unreachable("Relocation type is unsupported");
}

inline uint64_t getS(const Symbol &Sym) { return Sym.value(); }
inline uint64_t getS(const Contribution &Contribution) {
  return Contribution.OScn->VirtualAddr + Contribution.Offset;
}
inline uint64_t getA(const ExternalFixup &Fixup) { return Fixup.addend; }
inline uint64_t getA(const InternalFixup &Fixup) { return Fixup.addend; }

template <>
inline void applyExternal<llvm::ELF::R_X86_64_NONE>(
    uint8_t *const /*Out*/, const Contribution & /*Src*/,
    const Layout & /*Layout*/, const Symbol & /*Target*/,
    const ExternalFixup & /*XFixup*/) {}
template <>
inline void applyInternal<llvm::ELF::R_X86_64_NONE>(
    uint8_t *const /*Out*/, const Contribution & /*Src*/,
    const Layout & /*Layout*/, const Contribution & /*Target*/,
    const InternalFixup & /*Fixup*/) {}

template <typename TargetType, typename FixupType>
static inline void
apply_R_X86_64_64(uint8_t *const Out, const Contribution & /*Src*/,
                  const Layout & /*Layout*/, const TargetType &Target,
                  const FixupType &Fixup) {
  llvm::support::ulittle64_t::ref{Out} = getS(Target) + getA(Fixup);
}
template <>
inline void applyExternal<llvm::ELF::R_X86_64_64>(uint8_t *const Out,
                                                  const Contribution &Src,
                                                  const Layout &Layout,
                                                  const Symbol &Target,
                                                  const ExternalFixup &Fixup) {
  apply_R_X86_64_64(Out, Src, Layout, Target, Fixup);
}
template <>
inline void applyInternal<llvm::ELF::R_X86_64_64>(uint8_t *const Out,
                                                  const Contribution &Src,
                                                  const Layout &Layout,
                                                  const Contribution &Target,
                                                  const InternalFixup &Fixup) {
  apply_R_X86_64_64(Out, Src, Layout, Target, Fixup);
}

// Field: word32
// Calculation: S + A
// The R_X86_64_32 and R_X86_64_32S relocations truncate the computed value to
// 32-bits. The linker must verify that the generated value for the R_X86_64_32
// (R_X86_64_32S) relocation zero-extends (sign-extends) to the original 64-bit
// value.
template <typename TargetType, typename FixupType>
static inline void
apply_R_X86_64_32(uint8_t *const Out, const Contribution & /*Src*/,
                  const Layout & /*Layout*/, const TargetType &Target,
                  const FixupType &Fixup) {
  llvm::support::ulittle32_t::ref{Out} = getS(Target) + getA(Fixup);
}

template <>
inline void applyExternal<llvm::ELF::R_X86_64_32>(uint8_t *const Out,
                                                  const Contribution &Src,
                                                  const Layout &Layout,
                                                  const Symbol &Target,
                                                  const ExternalFixup &Fixup) {
  apply_R_X86_64_32(Out, Src, Layout, Target, Fixup);
}
template <>
inline void applyInternal<llvm::ELF::R_X86_64_32>(uint8_t *const Out,
                                                  const Contribution &Src,
                                                  const Layout &Layout,
                                                  const Contribution &Target,
                                                  const InternalFixup &Fixup) {
  apply_R_X86_64_32(Out, Src, Layout, Target, Fixup);
}

template <typename TargetType, typename FixupType>
static inline void
apply_R_X86_64_32S(uint8_t *const Out, const Contribution & /*Src*/,
                   const Layout & /*Layout*/, const TargetType &Target,
                   const FixupType &Fixup) {
  llvm::support::little32_t::ref{Out} = getS(Target) + getA(Fixup);
}
template <>
inline void applyExternal<llvm::ELF::R_X86_64_32S>(uint8_t *const Out,
                                                   const Contribution &Src,
                                                   const Layout &Layout,
                                                   const Symbol &Target,
                                                   const ExternalFixup &Fixup) {
  apply_R_X86_64_32S(Out, Src, Layout, Target, Fixup);
}
template <>
inline void applyInternal<llvm::ELF::R_X86_64_32S>(uint8_t *const Out,
                                                   const Contribution &Src,
                                                   const Layout &Layout,
                                                   const Contribution &Target,
                                                   const InternalFixup &Fixup) {
  apply_R_X86_64_32S(Out, Src, Layout, Target, Fixup);
}

template <typename TargetType, typename FixupType>
static inline void
apply_R_X86_64_PC32(uint8_t *const Out, const Contribution &Src,
                    const Layout & /*Layout*/, const TargetType &Target,
                    const FixupType &Fixup) {
  const auto S = getS(Target);
  const int64_t A = getA(Fixup);
  assert(alignTo(Src.Offset, Src.Align) == Src.Offset);
  const uint64_t P = Src.OScn->VirtualAddr + Src.Offset + Fixup.offset;
  llvm::support::little32_t::ref{Out} = S + A - P;
}
template <>
inline void applyExternal<llvm::ELF::R_X86_64_PC32>(
    uint8_t *const Out, const Contribution &Src, const Layout &Layout,
    const Symbol &Target, const ExternalFixup &Fixup) {
  return apply_R_X86_64_PC32(Out, Src, Layout, Target, Fixup);
}
template <>
inline void applyInternal<llvm::ELF::R_X86_64_PC32>(
    uint8_t *const Out, const Contribution &Src, const Layout &Layout,
    const Contribution &Target, const InternalFixup &Fixup) {
  return apply_R_X86_64_PC32(Out, Src, Layout, Target, Fixup);
}

template <>
inline void applyExternal<llvm::ELF::R_X86_64_PLT32>(
    uint8_t *const Out, const Contribution &Contribution, const Layout &Layout,
    const Symbol &Sym, const ExternalFixup &XFixup) {
  if (Sym.hasDefinition()) {
    return applyExternal<llvm::ELF::R_X86_64_PC32>(Out, Contribution, Layout,
                                                   Sym, XFixup);
  }

  const uint64_t L =
      Layout.Sections[SectionKind::plt].VirtualAddr + (Sym.pltIndex() + 1) * 16;
  const int64_t A = XFixup.addend;
  assert(alignTo(Contribution.Offset, Contribution.Align) ==
         Contribution.Offset);
  const uint64_t P =
      Contribution.OScn->VirtualAddr + Contribution.Offset + XFixup.offset;
  llvm::support::little32_t::ref{Out} = L + A - P;
}

#if 0
template <>
inline void apply<llvm::ELF::R_X86_64_GOTPCREL>(uint8_t *const Out,
                                            const Contribution &Contribution,
                                            const Layout &Layout,
                                            const Symbol *const Sym,
                                            const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_TLSGD>(uint8_t *const Out,
                                            const Contribution &Contribution,
                                            const Layout &Layout,
                                            const Symbol *const Sym,
                                            const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_TLSLD>(uint8_t *const Out,
                                            const Contribution &Contribution,
                                            const Layout &Layout,
                                            const Symbol *const Sym,
                                            const ExternalFixup &XFixup) {}
template <>
inline void apply<llvm::ELF::R_X86_64_DTPOFF32>(uint8_t *const Out,
                                            const Contribution &Contribution,
                                            const Layout &Layout,
                                            const Symbol *const Sym,
                                            const ExternalFixup &XFixup) {}
#endif

template <SectionKind SKind,
          typename SectionType = typename pstore::repo::enum_to_section<
              ToPstoreSectionKind<SKind>::value>::type>
static void applyInternalFixups(uint8_t *Dest, Context &Ctxt,
                                const SectionType &Section,
                                const Contribution &C, const Layout &Lout) {
  const ContributionSpArray *const IfxContributions = C.IfxContributions;
  assert(IfxContributions != nullptr);
  if (IfxContributions == nullptr) {
    return;
  }
  for (InternalFixup const &IFixup : Section.ifixups()) {
    llvmDebug(DebugType, Ctxt.IOMut, [&]() {
      llvm::dbgs() << "  ifx type:" << static_cast<unsigned>(IFixup.type)
                   << '\n';
    });

    const Contribution &Target =
        *(*IfxContributions)[IFixup.section].Contribution;
    assert((*IfxContributions)[IFixup.section].Kind == IFixup.section &&
           "An ifixup contribution didn't point to a target section of the "
           "correct kind");
    assert(Target.SectionK == toRldSectionKind(IFixup.section) &&
           "An ifixup's target section is of the wrong kind");
    assert(Target.OScn->SectionK == LayoutBuilder::mapInputToOutputSection(
                                        toRldSectionKind(IFixup.section)));
    switch (IFixup.type) {
#define ELF_RELOC(Name, Value)                                                 \
  case llvm::ELF::Name:                                                        \
    applyInternal<llvm::ELF::Name>(Dest + IFixup.offset, Target, Lout, Target, \
                                   IFixup);                                    \
    break;
#include "llvm/BinaryFormat/ELFRelocs/x86_64.def"
#undef ELF_RELOC
    }
  }
}

template <typename SectionType>
static void applyExternalFixups(uint8_t *Dest, Context &Ctxt,
                                const SectionType &Section,
                                const Contribution &C, const Layout &Lout) {
  const Symbol *const *XfxSymbols = C.XfxSymbols;
  for (ExternalFixup const &XFixup : Section.xfixups()) {
    const Symbol *const Symbol = *XfxSymbols;
    llvmDebug(DebugType, Ctxt.IOMut, [&]() {
      llvm::dbgs() << "  xfx type:" << static_cast<unsigned>(XFixup.type)
                   << " symbol: " << loadStdString(Ctxt.Db, Symbol->name())
                   << '\n';
    });

    switch (XFixup.type) {
#define ELF_RELOC(Name, Value)                                                 \
  case llvm::ELF::Name:                                                        \
    applyExternal<llvm::ELF::Name>(Dest + XFixup.offset, C, Lout, *Symbol,     \
                                   XFixup);                                    \
    break;
#include "llvm/BinaryFormat/ELFRelocs/x86_64.def"
#undef ELF_RELOC
    }
    ++XfxSymbols;
  }
}

template <SectionKind SKind>
uint8_t *copyContribution(uint8_t *Dest, Context &Ctxt, const Contribution &C,
                          const Layout &Lout) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "copy to "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest)) << '\n';
  });
  using SectionType = typename pstore::repo::enum_to_section<
      ToPstoreSectionKind<SKind>::value>::type;
  auto *const Section = reinterpret_cast<SectionType const *>(C.Section);
  if (Section == nullptr) {
    return Dest;
  }
  const auto &D = Section->payload();
  const auto Size = D.size();
  std::memcpy(Dest, D.begin(), Size);
  applyExternalFixups<SectionType>(Dest, Ctxt, *Section, C, Lout);
  applyInternalFixups<SKind>(Dest, Ctxt, *Section, C, Lout);
  return Dest + Size;
}

template <>
uint8_t *copyContribution<SectionKind::bss>(uint8_t *Dest, Context &Ctxt,
                                            const Contribution &S,
                                            const Layout &Lout) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "BSS fill "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest)) << '\n';
  });
  static_assert(
      std::is_same<
          pstore::repo::enum_to_section<pstore::repo::section_kind::bss>::type,
          pstore::repo::bss_section>::value,
      "BSS section kind must map to BSS section type");
  return Dest;
}

template <>
uint8_t *copyContribution<SectionKind::linked_definitions>(uint8_t *Dest,
                                                           Context &,
                                                           const Contribution &,
                                                           const Layout &) {
  // discard
  return Dest;
}

template <SectionKind SectionK>
uint8_t *copySection(uint8_t *Data, Context &Ctxt, const Layout &Lout,
                     const LocalPLTsContainer & /*PLT*/) {
  auto *Dest = Data;
  for (Contribution const &Contribution :
       Lout.Sections[SectionK].Contributions) {
    llvmDebug(DebugType, Ctxt.IOMut,
              []() { llvm::dbgs() << SectionK << ": "; });

    Dest = Data + alignTo(Contribution.Offset, Contribution.Align);
    switch (SectionK) {
#define X(a)                                                                   \
  case SectionKind::a:                                                         \
    Dest = copyContribution<SectionKind::a>(Dest, Ctxt, Contribution, Lout);   \
    break;
      PSTORE_MCREPO_SECTION_KINDS
#undef X

    case SectionKind::init_array:
      Dest = copyContribution<SectionKind::read_only>(Dest, Ctxt, Contribution,
                                                      Lout);
      break;
    default:
      llvm_unreachable("Bad section kind");
      break;
    }
    // TODO: check that Dest is increasing.
  }
  return Dest;
}

template <>
uint8_t *
copySection<SectionKind::shstrtab>(uint8_t *Dest, Context & /*Ctxt*/,
                                   const Layout & /*Lout*/,
                                   const LocalPLTsContainer & /*PLT*/) {
  return Dest;
}
template <>
uint8_t *copySection<SectionKind::strtab>(uint8_t *Dest, Context & /*Ctxt*/,
                                          const Layout & /*Lout*/,
                                          const LocalPLTsContainer & /*PLT*/) {
  return Dest;
}
template <>
uint8_t *copySection<SectionKind::symtab>(uint8_t *Dest, Context & /*Ctxt*/,
                                          const Layout & /*Lout*/,
                                          const LocalPLTsContainer & /*PLT*/) {
  return Dest;
}

template <>
uint8_t *
copySection<SectionKind::gotplt>(std::uint8_t *Dest, Context &Ctxt,
                                 const Layout &Lout,
                                 const LocalPLTsContainer &PLTSymbols) {
  return Dest;
}

template <>
uint8_t *copySection<SectionKind::plt>(std::uint8_t *Dest, Context &Ctxt,
                                       const Layout &Lout,
                                       const LocalPLTsContainer &PLTSymbols) {
  assert(!PLTSymbols.empty());

  {
    // add the PLT header
    // TODO: hang an object representing the target architecture/ABI from
    // the context and invoke a virtual method on it to do this.
    static const std::array<uint8_t, 16> PLTData{{
        0xFF, 0x35, 0, 0, 0, 0, // pushq GOTPLT+8(%rip)
        0xFF, 0x25, 0, 0, 0, 0, // jmp *GOTPLT+16(%rip)
        0x0F, 0x1F, 0x40, 0,    // nop
    }};

    auto Out = std::copy(std::begin(PLTData), std::end(PLTData), Dest);

    uint64_t GOTPLT = 0; // uint64_t gotPlt = in.gotPlt->getVA();
    uint64_t PLT =
        0; // uint64_t plt = in.ibtPlt ? in.ibtPlt->getVA() : in.plt->getVA();
    llvm::support::endian::write32le(Dest + 2, GOTPLT - PLT + 2); // GOTPLT+8
    llvm::support::endian::write32le(Dest + 8, GOTPLT - PLT + 4); // GOTPLT+16
    Dest = Out;
  }

  auto PLTVirtualAddr = Lout.Sections[SectionKind::plt].VirtualAddr;
  const auto GOTPLTVirtualAddr = Lout.Sections[SectionKind::gotplt].VirtualAddr;
  for (const Symbol *const Sym : PLTSymbols) {
    static const std::array<uint8_t, 16> Inst{{
        0xFF, 0x25, 0, 0, 0, 0, // jmpq *got(%rip)
        0x68, 0, 0, 0, 0,       // pushq <relocation index>
        0xE9, 0, 0, 0, 0,       // jmpq plt[0]
    }};
    auto *const Out = std::copy(std::begin(Inst), std::end(Inst), Dest);
    llvm::support::endian::write32le(Dest + 2,
                                     GOTPLTVirtualAddr - PLTVirtualAddr - 6U);
    llvm::support::endian::write32le(Dest + 7, Sym->pltIndex());
    llvm::support::endian::write32le(Dest + 12,
                                     0 /*in.plt->getVA() - pltEntryAddr - 16*/);
    PLTVirtualAddr += 16;
    Dest = Out;
  }
  return Dest;
}

static constexpr char const *jobName(const SectionKind SectionK) {
  switch (SectionK) {
#define X(K)                                                                   \
  case SectionKind::K:                                                         \
    return "Copy " #K;
#define RLD_X(a) X(a)
    RLD_ALL_SECTION_KINDS
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
  auto SectionHasNoContributions = [&](SectionKind K) {
    return Lout.Sections[K].Contributions.empty();
  };
  switch (SectionK) {
  case SectionKind::init_array:
  case SectionKind::fini_array:
    return !SectionHasNoContributions(SectionK);
#define X(K)                                                                   \
  case SectionKind::K:                                                         \
    return !SectionHasNoContributions(SectionKind::K);

    PSTORE_MCREPO_SECTION_KINDS
#undef X
  case SectionKind::rela_plt:
  case SectionKind::gotplt:
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
    const Layout &Lout, const LocalPLTsContainer &PLTs,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    uint64_t TargetDataOffset) {

  forEachSectionKindInFileOrder([&](const SectionKind SectionK) {
    if (!hasDataToCopy(SectionK, Ctxt, Lout)) {
      return;
    }
    assert(SectionFileOffsets[SectionK].hasValue() &&
           "No layout position for a section with data to copy");
    Workers.async(
        [SectionK, &Ctxt, &Lout, &PLTs](uint8_t *const Out) {
          llvm::NamedRegionTimer CopyTimer(jobName(SectionK), "Copy section",
                                           rld::TimerGroupName,
                                           rld::TimerGroupDescription);

          switch (SectionK) {
#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    copySection<SectionKind::x>(Out, Ctxt, Lout, PLTs);                        \
    break;
#define RLD_X(x) X(x)
            RLD_ALL_SECTION_KINDS
#undef RLD_X
#undef X
          case SectionKind::last:
            break;
          }
        },
        Data + TargetDataOffset + *SectionFileOffsets[SectionK]);
  });
}

} // end namespace rld
