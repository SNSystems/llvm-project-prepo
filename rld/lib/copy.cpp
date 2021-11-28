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

#include "rld/MPMCQueue.h"
#include "rld/MathExtras.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/Timer.h"

#include <cassert>
#include <sstream>

using namespace rld;

static constexpr auto DebugType = "rld-copy";

using ExternalFixup = pstore::repo::external_fixup;
using InternalFixup = pstore::repo::internal_fixup;

static const char *relocationName(const uint8_t Relocation) {
  const char *N = "Unknown";
  switch (Relocation) {
#define ELF_RELOC(Name, Value)                                                 \
  case Value:                                                                  \
    N = #Name;                                                                 \
    break;
#include "llvm/BinaryFormat/ELFRelocs/x86_64.def"
#undef ELF_RELOC
  }
  return N;
}

namespace {

template <uint8_t Relocation> class applier {
public:
  template <typename TargetType, typename FixupType>
  static void apply(uint8_t *const Out, Context &Context,
                    const Contribution &Src, const Layout &Layout,
                    const TargetType &Target, const FixupType &Fixup) {
    warnNotImplemented(Context);
    llvm_unreachable("Relocation type is unsupported");
  }

  static void applyExternal(uint8_t *const Out, Context &Context,
                            const Contribution &Src, const Layout &Layout,
                            const Symbol &Target, const ExternalFixup &Fixup) {
    return apply(Out, Context, Src, Layout, Target, Fixup);
  }

  static void applyInternal(uint8_t *const Out, Context &Context,
                            const Contribution &Src, const Layout &Layout,
                            const Contribution &Target,
                            const InternalFixup &Fixup) {
    return apply(Out, Context, Src, Layout, Target, Fixup);
  }

  static inline uint64_t getS(const Symbol &Sym) { return Sym.value(); }
  static inline uint64_t getS(const Contribution &Contribution) {
    return Contribution.OScn->VirtualAddr + Contribution.Offset;
  }
  static inline uint64_t getA(const ExternalFixup &Fixup) {
    return Fixup.addend;
  }
  static inline uint64_t getA(const InternalFixup &Fixup) {
    return Fixup.addend;
  }

private:
  static void warnNotImplemented(Context &Context) {
    static bool Warned = false;
    if (!Warned) {
      Warned = true;
      std::lock_guard<decltype(Context.IOMut)> _{Context.IOMut};
      llvm::outs() << "Warning: " << relocationName(Relocation)
                   << " is not yet supported\n";
    }
  }
};

// R_X86_64_NONE
// ~~~~~~~~~~~~~
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_NONE>::apply(uint8_t *const, Context &,
                                              const Contribution &,
                                              const Layout &,
                                              const TargetType &,
                                              const FixupType &) {}

// R_X86_64_64
// ~~~~~~~~~~~
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_64>::apply(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  llvm::support::ulittle64_t::ref{Out} = getS(Target) + getA(Fixup);
}

// R_X86_64_32
// ~~~~~~~~~~~
// Field: word32
// Calculation: S + A
// The R_X86_64_32 and R_X86_64_32S relocations truncate the computed value to
// 32-bits. The linker must verify that the generated value for the R_X86_64_32
// (R_X86_64_32S) relocation zero-extends (sign-extends) to the original 64-bit
// value.
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_32>::apply(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  llvm::support::ulittle32_t::ref{Out} = getS(Target) + getA(Fixup);
}

// R_X86_64_32S
// ~~~~~~~~~~~~
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_32S>::apply(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  llvm::support::little32_t::ref{Out} = getS(Target) + getA(Fixup);
}

// R_X86_64_PC32
// ~~~~~~~~~~~~~
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_PC32>::apply(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  const auto S = getS(Target);
  const int64_t A = getA(Fixup);
  assert(alignTo(Src.Offset, Src.Align) == Src.Offset);
  const uint64_t P = Src.OScn->VirtualAddr + Src.Offset + Fixup.offset;
  llvm::support::little32_t::ref{Out} = S + A - P;
}

// R_X86_64_PLT32
// ~~~~~~~~~~~~~~
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_PLT32>::apply(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  if (Target.hasDefinition()) {
    return applier<llvm::ELF::R_X86_64_PC32>::apply(Out, Context, Src, Layout,
                                                    Target, Fixup);
  }
  const uint64_t L = Layout.Sections[SectionKind::plt].VirtualAddr +
                     (Target.pltIndex() + 1) * 16;
  const int64_t A = Fixup.addend;
  const uint64_t P = Src.OScn->VirtualAddr + Src.Offset + Fixup.offset;
  llvm::support::little32_t::ref{Out} = L + A - P;
}

template <>
void applier<llvm::ELF::R_X86_64_PLT32>::applyInternal(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const Contribution &Target,
    const InternalFixup &Fixup) {
  llvm_unreachable("an internal R_X86_64_PLT32 fixup was encountered");
}

// R_X86_64_GOTPCREL
// ~~~~~~~~~~~~~~~~~
// Field: word32
// Calculation: G + GOT + A - P
// A    The addend used to compute the value of the relocatable field.
// G    The offset into the global offset table at which the relocation entry’s
//        symbol will reside during execution.
// GOT  The address of the global offset table.
// P    The place (section offset or address) of the storage unit being
//        relocated.

template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_GOTPCREL>::apply(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  const auto A = Fixup.addend;
  const auto G = (Target.gotIndex() + 1) * 8U;
  const auto GOT = Layout.Sections[SectionKind::got].VirtualAddr;
  const uint64_t P = Src.OScn->VirtualAddr + Src.Offset + Fixup.offset;

  llvm::support::little32_t::ref{Out} = G + GOT + A - P;
}

template <>
void applier<llvm::ELF::R_X86_64_GOTPCREL>::applyInternal(
    uint8_t *const Out, Context &Context, const Contribution &Src,
    const Layout &Layout, const Contribution &Target,
    const InternalFixup &Fixup) {
  llvm_unreachable("an internal GOTPCREL fixup was encountered");
}

// R_X86_64_TPOFF32
// ~~~~~~~~~~~~~~~~
template <>
template <typename TargetType, typename FixupType>
void applier<llvm::ELF::R_X86_64_TPOFF32>::apply(
    uint8_t *const Out, Context & /*Context*/, const Contribution &Src,
    const Layout &Layout, const TargetType &Target, const FixupType &Fixup) {
  const auto S = getS(Target);
  const int64_t A = getA(Fixup);
  assert(alignTo(Src.Offset, Src.Align) == Src.Offset);
  const uint64_t P = Layout.Segments[SegmentKind::tls].VirtualAddr;
  llvm::support::little32_t::ref{Out} = S + A - P;
}

} // end anonymous namespace

template <SectionKind SKind,
          typename SectionType = typename pstore::repo::enum_to_section<
              ToPstoreSectionKind<SKind>::value>::type>
static void applyInternalFixups(uint8_t *Dest, Context &Ctxt,
                                const SectionType &Section,
                                const Contribution &Src, const Layout &Lout) {
  const ContributionSpArray *const IfxContributions = Src.IfxContributions;
  assert(IfxContributions != nullptr);
  if (IfxContributions == nullptr) {
    return;
  }
  for (InternalFixup const &IFixup : Section.ifixups()) {
    llvmDebug(DebugType, Ctxt.IOMut, [&] {
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
    applier<llvm::ELF::Name>::applyInternal(Dest + IFixup.offset, Ctxt, Src,   \
                                            Lout, Target, IFixup);             \
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
    llvmDebug(DebugType, Ctxt.IOMut, [&] {
      llvm::dbgs() << "  xfx type:" << static_cast<unsigned>(XFixup.type)
                   << " symbol:" << loadStdString(Ctxt.Db, Symbol->name())
                   << " offset:" << XFixup.offset << '\n';
    });

    switch (XFixup.type) {
#define ELF_RELOC(Name, Value)                                                 \
  case llvm::ELF::Name:                                                        \
    applier<llvm::ELF::Name>::applyExternal(Dest + XFixup.offset, Ctxt, C,     \
                                            Lout, *Symbol, XFixup);            \
    break;
#include "llvm/BinaryFormat/ELFRelocs/x86_64.def"
#undef ELF_RELOC
    }
    ++XfxSymbols;
  }
}

template <SectionKind SKind>
void copyContribution(uint8_t *Dest, Context &Ctxt, const Contribution &C,
                      const Layout &Lout) {
  llvmDebug(DebugType, Ctxt.IOMut, [&] {
    llvm::dbgs() << "copy to "
                 << format_hex(reinterpret_cast<std::uintptr_t>(Dest))
                 << " for " << loadStdString(Ctxt.Db, C.Name) << '\n';
  });

  using SectionType = typename pstore::repo::enum_to_section<
      ToPstoreSectionKind<SKind>::value>::type;
  auto *const Section = reinterpret_cast<SectionType const *>(C.Section);
  if (Section == nullptr) {
    return;
  }
  const auto &D = Section->payload();
  const auto Size = D.size();
  std::memcpy(Dest, D.begin(), Size);
  applyExternalFixups<SectionType>(Dest, Ctxt, *Section, C, Lout);
  applyInternalFixups<SKind>(Dest, Ctxt, *Section, C, Lout);
}

template <>
void copyContribution<SectionKind::bss>(uint8_t *Dest, Context &Ctxt,
                                        const Contribution &, const Layout &) {
  llvmDebug(DebugType, Ctxt.IOMut, [Dest] {
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
void copyContribution<SectionKind::linked_definitions>(uint8_t *, Context &,
                                                       const Contribution &,
                                                       const Layout &) {
  // discard
}

// Represents a job in the queue of blocks to be copied to the output.
struct WorkItem {
  using ContributionIterator = OutputSection::ContributionVector::chunk::const_iterator;
  using HandlerFn = void (*)(uint8_t *, Context &, const Layout &,
                             const GOTPLTContainer &, ContributionIterator,
                             ContributionIterator);

  WorkItem() = default;
  WorkItem(uint8_t *Out, HandlerFn Handler) : Out{Out}, Handler{Handler} {}
  WorkItem(uint8_t *Out, HandlerFn Handler, ContributionIterator First,
           ContributionIterator Last)
      : Out{Out}, Handler{Handler}, First{First}, Last{Last} {}

  // Where output should be written. If this value is null, the consumer job
  // exits.
  uint8_t *Out = nullptr;
  // Function responsible for performing the copy.
  HandlerFn Handler = nullptr;
  // The first contribution to be copied.
  ContributionIterator First {static_cast<const Contribution *> (nullptr)};
  // The end of the range of contributions to be copied.
  ContributionIterator Last {static_cast<const Contribution *> (nullptr)};
};

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

template <SectionKind SectionK>
static std::string sectionTimerName(const bool TimersEnabled) {
  std::string Name;
  if (TimersEnabled) {
    static std::atomic<unsigned> Count{0U};
    const auto OldCount = Count.fetch_add(1U, std::memory_order_relaxed);
    llvm::raw_string_ostream OS{Name};
    OS << jobName(SectionK) << '#' << OldCount;
    OS.flush();
  }
  return Name;
}

template <SectionKind SectionK>
static void copySection(uint8_t *const Data, Context &Context, const Layout &Layout,
                 const GOTPLTContainer & /*GOTPLTs*/, WorkItem::ContributionIterator First,
                 WorkItem::ContributionIterator Last) {
  llvm::NamedRegionTimer _{sectionTimerName<SectionK>(Context.TimersEnabled),
                           "Copy section", rld::TimerGroupName,
                           rld::TimerGroupDescription, Context.TimersEnabled};

  std::for_each(First, Last, [&](const Contribution &Contribution) {
    llvmDebug(DebugType, Context.IOMut, [] { llvm::dbgs() << SectionK << ": "; });

    auto *const Dest = Data + alignTo(Contribution.Offset, Contribution.Align);
    switch (Contribution.SectionK) {
#define X(a)                                                                   \
  case SectionKind::a:                                                         \
    copyContribution<SectionKind::a>(Dest, Context, Contribution, Layout);     \
    break;
      PSTORE_MCREPO_SECTION_KINDS
#undef X

    case SectionKind::init_array:
      copyContribution<SectionKind::read_only>(Dest, Context, Contribution,
                                               Layout);
      break;
    default:
      llvm_unreachable("Bad section kind");
      break;
    }
  });
}

template <SectionKind SectionK>
static void scheduleCopySection(uint8_t *const Data, MPMCQueue<WorkItem> &Q,
                                Context &Ctxt, const Layout &Lout,
                                const GOTPLTContainer & /*GOTPLTs*/) {
  auto const &Contributions = Lout.Sections[SectionK].Contributions;
  std::for_each(Contributions.chunks_begin(), Contributions.chunks_end(),
                [&](const OutputSection::ContributionVector::chunk &Chunk) {
                  if (Chunk.empty()) {
                    return;
                  }
                  Q.emplace(Data, &copySection<SectionK>, Chunk.begin(),
                            Chunk.end());
                });
}

template <>
void scheduleCopySection<SectionKind::shstrtab>(
    uint8_t *const /*Data*/, MPMCQueue<WorkItem> & /*Q*/, Context & /*Ctxt*/,
    const Layout & /*Lout*/, const GOTPLTContainer & /*GOTPLTs*/) {}

template <>
void scheduleCopySection<SectionKind::strtab>(
    uint8_t *const /*Data*/, MPMCQueue<WorkItem> & /*Q*/, Context & /*Ctxt*/,
    const Layout & /*Lout*/, const GOTPLTContainer & /*GOTPLTs*/) {}

template <>
void scheduleCopySection<SectionKind::symtab>(
    uint8_t *const /*Data*/, MPMCQueue<WorkItem> & /*Q*/, Context & /*Ctxt*/,
    const Layout & /*Lout*/, const GOTPLTContainer & /*GOTPLTs*/) {}

template <>
void scheduleCopySection<SectionKind::gotplt>(
    uint8_t *const /*Data*/, MPMCQueue<WorkItem> & /*Q*/, Context & /*Ctxt*/,
    const Layout & /*Lout*/, const GOTPLTContainer & /*GOTPLTs*/) {}

template <>
void scheduleCopySection<SectionKind::plt>(
    uint8_t *const Data, MPMCQueue<WorkItem> &Q, Context &Ctxt,
    const Layout &Lout, const GOTPLTContainer & /*GOTPLTs*/) {
  // TODO: Implement it. Obviously.
}

#if 0
template <>
void copySection<SectionKind::plt>(uint8_t *Dest, Context &Ctxt,
                                   const Layout &Lout,
                                   const GOTPLTContainer &GOTPLTs,
                                   WorkItem::ContributionIterator /*First*/,
                                   WorkItem::ContributionIterator /*Last*/) {
  llvm::NamedRegionTimer CopyTimer("PLT write", "Write the PLT section",
                                   rld::TimerGroupName,
                                   rld::TimerGroupDescription, Context.TimersEnabled);
  const auto &PLTSymbols = GOTPLTs.PLT;
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
}
#endif

template <>
void copySection<SectionKind::got>(uint8_t *Dest, Context &Context,
                                   const Layout &Lout,
                                   const GOTPLTContainer &GOTPLTs,
                                   WorkItem::ContributionIterator /*First*/,
                                   WorkItem::ContributionIterator /*Last*/) {
  llvm::NamedRegionTimer _{"GOT write", "Write the GOT section",
                           rld::TimerGroupName, rld::TimerGroupDescription,
                           Context.TimersEnabled};
  const auto &GOTSymbols = GOTPLTs.GOT;
  assert(!GOTSymbols.empty());
#ifndef NDEBUG
  {
    // Verify that the values returned by gotIndex() and the order in the
    // GOTSymbols array are consistent.
    auto Count = 1U;
    for (const Symbol *const Sym : GOTSymbols) {
      assert(Sym->gotIndex() + 1U == Count);
      ++Count;
    }
  }
#endif
  auto Write = [&Dest](std::uint64_t V) {
    llvm::support::endian::write64le(Dest, V);
    Dest += 8;
  };
  Write(0); // FIXME: the first entry must point to _DYNAMIC.
  for (const Symbol *const Sym : GOTSymbols) {
    Write(Sym->value());
  }
}

template <>
void scheduleCopySection<SectionKind::got>(
    uint8_t *const Data, MPMCQueue<WorkItem> &Q, Context &Ctxt,
    const Layout &Lout, const GOTPLTContainer & /*GOTPLTs*/) {
  Q.emplace(Data, &copySection<SectionKind::got>);
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
  case SectionKind::got:
    return Ctxt.GOTEntries.load() > 0U;

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

static void consumer(MPMCQueue<WorkItem> &Q, Context &Context,
                     const Layout &Layout, const GOTPLTContainer &GOTPLTs) {
  for (;;) {
    WorkItem W = Q.pop();
    if (W.Out == nullptr) {
      break;
    }
    (*W.Handler)(W.Out, Context, Layout, GOTPLTs, W.First, W.Last);
  }
}

namespace rld {

void copyToOutput(
    Context &Context, llvm::ThreadPool &Workers, uint8_t *const Data,
    const Layout &Layout, const GOTPLTContainer &GOTPLTs,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    uint64_t TargetDataOffset) {

  MPMCQueue<WorkItem> Q{std::size_t{32768}};

  const auto NumConsumers = std::max(Workers.getThreadCount(), 1U);

  assert(llvm::llvm_is_multithreaded());

  std::thread Producer{[&] {
    llvm::NamedRegionTimer _{"Producer", "Schedule copy jobs",
                             rld::TimerGroupName, rld::TimerGroupDescription,
                             Context.TimersEnabled};
    forEachSectionKindInFileOrder([&](const SectionKind SectionK) {
      if (!hasDataToCopy(SectionK, Context, Layout)) {
        return;
      }
      assert(SectionFileOffsets[SectionK].hasValue() &&
             "No layout position for a section with data to copy");
      uint8_t *const Out =
          Data + TargetDataOffset + *SectionFileOffsets[SectionK];
      switch (SectionK) {
#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    scheduleCopySection<SectionKind::x>(Out, Q, Context, Layout, GOTPLTs);     \
    break;
#define RLD_X(x) X(x)
        RLD_ALL_SECTION_KINDS
#undef RLD_X
#undef X
      case SectionKind::last:
        break;
      }
    });

    // Queue one job per consumer which tells it to exit.
    for (auto Ctr = 0U; Ctr < NumConsumers; ++Ctr) {
      Q.emplace();
    }
  }};
  for (auto Ctr = 0U; Ctr < NumConsumers; ++Ctr) {
    Workers.async(consumer, std::ref(Q), std::ref(Context), std::cref(Layout),
                  std::cref(GOTPLTs));
  }
  Workers.wait();
  Producer.join();
}

} // end namespace rld
