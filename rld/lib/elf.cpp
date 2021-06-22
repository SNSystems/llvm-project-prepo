//===- lib/elf.cpp --------------------------------------------------------===//
//*       _  __  *
//*   ___| |/ _| *
//*  / _ \ | |_  *
//* |  __/ |  _| *
//*  \___|_|_|   *
//*              *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/elf.h"

#include "rld/Algorithm.h"
#include "rld/MathExtras.h"
#include "llvm/Support/AlignOf.h"

namespace {

constexpr auto DebugType = "rld-ELF";

} // end anonymous namespace

#define ELF_SECTION_NAME(K, N)                                                 \
  constexpr char rld::elf::ElfSectionName<rld::SectionKind::K>::name[];        \
  constexpr std::size_t rld::elf::ElfSectionName<rld::SectionKind::K>::length;
RLD_ELF_SECTION_NAMES
#undef ELF_SECTION_NAME

llvm::ErrorOr<unsigned> rld::elf::details::machineFromTriple(
    llvm::Optional<llvm::Triple> const &Triple) {
  if (!Triple) {
    return std::errc::not_enough_memory; // FIXME: no triple was found
  }
  switch (Triple->getArch()) {
  case llvm::Triple::x86:
    return llvm::ELF::EM_386;
  case llvm::Triple::x86_64:
    return llvm::ELF::EM_X86_64;
  default:
    return std::errc::not_enough_memory; // FIXME: unknown machine type in
                                         // triple
  }
}

// FIXME: copied from repo2obj WriteHelpers.h
template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty const &T) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), sizeof(T));
  return sizeof(T);
}

namespace {

template <typename ELFT> struct ElfSegmentType {
  typename llvm::object::ELFFile<ELFT>::Elf_Word p_type;
};
template <typename OStream, typename ELFT>
OStream &operator<<(OStream &OS, ElfSegmentType<ELFT> const &ST) {
  switch (ST.p_type) {
  case llvm::ELF::PT_NULL:
    OS << "PT_NULL";
    break;
  case llvm::ELF::PT_LOAD:
    OS << "PT_LOAD";
    break;
  case llvm::ELF::PT_DYNAMIC:
    OS << "PT_DYNAMIC";
    break;
  case llvm::ELF::PT_INTERP:
    OS << "PT_INTERP";
    break;
  case llvm::ELF::PT_NOTE:
    OS << "PT_NOTE";
    break;
  case llvm::ELF::PT_SHLIB:
    OS << "PT_SHLIB";
    break;
  case llvm::ELF::PT_PHDR:
    OS << "PT_PHDR";
    break;
  case llvm::ELF::PT_TLS:
    OS << "PT_TLS";
    break;
  case llvm::ELF::PT_GNU_STACK:
    OS << "PT_GNU_STACK";
    break;
  default:
    OS << ST.p_type;
    break;
  }
  return OS;
}

template <typename ELFT> struct ElfSegmentFlags {
  typename llvm::object::ELFFile<ELFT>::Elf_Word p_flags;
};
template <typename OStream, typename ELFT>
OStream &operator<<(OStream &OS, ElfSegmentFlags<ELFT> const &Flags) {
  auto WriteFlag = [&](unsigned F, char C) {
    if ((Flags.p_flags & F) != 0U) {
      OS << C;
    }
  };
  WriteFlag(llvm::ELF::PF_R, 'R');
  WriteFlag(llvm::ELF::PF_W, 'W');
  WriteFlag(llvm::ELF::PF_X, 'E');
  return OS;
}

} // end anonymous namespace

template <typename ELFT>
auto rld::elf::emitProgramHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr, rld::Context &Ctxt,
    const rld::FileRegion &TargetDataRegion,
    const rld::FileRegion &SegmentTableRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets)
    -> typename llvm::object::ELFFile<ELFT>::Elf_Phdr * {

  auto DebugHeading =
      makeOnce([](llvm::raw_ostream &OS) { OS << "ELF Program Headers\n"; });
  auto *const Start = Phdr;
  Lout.forEachSegment([&](const SegmentKind Kind, const Segment &Segment) {
    if (!Segment.shouldEmit()) {
      return;
    }

    int64_t SegmentDataOffset = SegmentDataOffsets[Kind].getValueOr(0);
    if (Segment.VirtualSize > 0) {
      SegmentDataOffset += TargetDataRegion.offset();
    }

    // FIXME: rather than special-case code here, fold this into
    // SegmentDataOffsets.
    if (Kind == SegmentKind::rodata) {
      SegmentDataOffset = 0;
    }

    assert(SegmentDataOffset >= 0);
    Phdr->p_type = elfSegmentKind<ELFT>(Kind);
    Phdr->p_flags = elfSegmentFlags<ELFT>(Kind);
    Phdr->p_vaddr = hasPhysicalAddress(Kind) ? Segment.VirtualAddr : 0;
    Phdr->p_paddr = Phdr->p_vaddr;
    Phdr->p_offset = SegmentDataOffset;
    Phdr->p_filesz = Segment.FileSize;
    Phdr->p_memsz = hasPhysicalAddress(Kind) ? Segment.VirtualSize : 0;

    // “Values 0 and 1 mean no alignment is required. Otherwise, p_align
    // should be a positive, integral power of 2”
    const unsigned MaxAlign = Segment.MaxAlign;
    assert(MaxAlign == 0U || llvm::countPopulation(MaxAlign) == 1U);
    Phdr->p_align = hasPhysicalAddress(Kind) ? MaxAlign : 0;

    llvmDebug(DebugType, Ctxt.IOMut, [&]() {
      auto &OS = llvm::dbgs();
      DebugHeading(OS);
      OS << "Segment: " << Kind << '\n';
      OS << "  type=" << ElfSegmentType<ELFT>{Phdr->p_type} << '\n';
      OS << "  vaddr=" << format_hex(Phdr->p_vaddr) << '\n';
      OS << "  paddr=" << format_hex(Phdr->p_paddr) << '\n';
      OS << "  offset=" << format_hex(Phdr->p_offset) << '\n';
      OS << "  memsz=" << format_hex(Phdr->p_memsz) << '\n';
      OS << "  filesz=" << format_hex(Phdr->p_filesz) << '\n';
      OS << "  flags=" << ElfSegmentFlags<ELFT>{Phdr->p_flags} << '\n';
      OS << "  align=" << format_hex(Phdr->p_align) << '\n';
    });

    // “The file size may not be larger than the memory size.”
    assert(Phdr->p_filesz <= Phdr->p_memsz);

    // “loadable process segments must have congruent values for p_vaddr
    // and p_offset, modulo the page size”. That is, p_vaddr ≡
    // p_offset(mod p_align).
    assert(Phdr->p_type != llvm::ELF::PT_LOAD ||
           (Phdr->p_vaddr % Phdr->p_align) == (Phdr->p_offset % Phdr->p_align));

    ++Phdr;
  });
  assert(Phdr >= Start &&
         (Phdr - Start) * sizeof(*Phdr) == SegmentTableRegion.size() &&
         "The segment table did not fill its allocated space");
  return Phdr;
}

/// \param Lout The layout.
/// \returns A table which contains the index of section-header record for each
///   of the various section-kinds.
static rld::SectionIndexedArray<unsigned>
getLinkSections(const rld::Layout &Lout) {
  using namespace rld;

  SectionIndexedArray<unsigned> Links;
  auto Index = 1U;
  forEachSectionKind([&](SectionKind SectionK) {
    const OutputSection &OScn = Lout.Sections[SectionK];
    if (OScn.shouldEmit()) {
      Links[SectionK] = Index;
      ++Index;
    } else {
      Links[SectionK] = 0;
    }
  });

  return Links;
}

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::FileRegion &SegmentTableRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets)
    -> typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::FileRegion &SegmentTableRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets)
    -> typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::FileRegion &SegmentTableRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets)
    -> typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::FileRegion &SegmentTableRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets)
    -> typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *;

namespace {

template <typename ELFT, rld::SectionKind SKind> struct ShInfoValue {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;

  Elf_Word operator()(
      const rld::Layout &Lout, const rld::SectionIndexedArray<unsigned> &Links,
      const std::vector<const rld::Symbol *> & /*OrderedGlobals*/) const {
    return Elf_Word{Links[Lout.Sections[SKind].Info]};
  }
};

template <typename ELFT> struct ShInfoValue<ELFT, rld::SectionKind::symtab> {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;

  Elf_Word
  operator()(const rld::Layout & /*Lout*/,
             const rld::SectionIndexedArray<unsigned> & /*Links*/,
             const std::vector<const rld::Symbol *> &OrderedGlobals) const {
    if (OrderedGlobals.empty()) {
      return Elf_Word{0};
    }
    auto const Pos = std::partition_point(
        std::begin(OrderedGlobals), std::end(OrderedGlobals),
        [](const rld::Symbol *const S) { return S->hasLocalLinkage(); });
    // (plus one to allow for the initial null symbol).
    return static_cast<Elf_Word>(
        std::distance(std::begin(OrderedGlobals), Pos) + 1);
  }
};

} // end anonymous namespace

template <typename ELFT>
static typename llvm::object::ELFFile<ELFT>::Elf_Word
getShInfoValue(rld::SectionKind SectionK, const rld::Layout &Lout,
               const rld::SectionIndexedArray<unsigned> &Links,
               const std::vector<const rld::Symbol *> &OrderedGlobals) {
  switch (SectionK) {
#define X(a)                                                                   \
  case rld::SectionKind::a:                                                    \
    return ShInfoValue<ELFT, rld::SectionKind::a>{}(Lout, Links,               \
                                                    OrderedGlobals);
#define RLD_X(a) X(a)
    PSTORE_MCREPO_SECTION_KINDS
    RLD_SECTION_KINDS
#undef RLD_X
#undef X
  case rld::SectionKind::last:
    llvm_unreachable("We should not process the last value");
  }
}

template <typename ELFT>
auto rld::elf::emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr, const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    const std::vector<const Symbol *> &OrderedGlobals,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr * {

  // A table which contains the index of section-header record for each of the
  // various section-kinds.
  const SectionIndexedArray<unsigned> Links = getLinkSections(Lout);
  // The Null section.
  std::memset(Shdr, 0, sizeof(*Shdr));
  ++Shdr;

  forEachSectionKind([&](SectionKind SectionK) {
    const OutputSection &OScn = Lout.Sections[SectionK];
    if (OScn.shouldEmit()) {
      const auto linkSection = [&Links](SectionKind L) {
        return L == SectionKind::last ? 0 : Links[L];
      };
      std::memset(Shdr, 0, sizeof(*Shdr));
      Shdr->sh_name = NameOffsets[SectionK];
      Shdr->sh_type = elfSectionType<ELFT>(SectionK);
      Shdr->sh_flags = elfSectionFlags<ELFT>(SectionK);
      Shdr->sh_addr = OScn.VirtualAddr;
      assert(SectionFileOffsets[SectionK].hasValue() &&
             "Emitting a section for which we have not computed a file offset");
      Shdr->sh_offset =
          *SectionFileOffsets[SectionK] +
          TargetDataOffset; // File offset of section data, in bytes
      Shdr->sh_size = OScn.FileSize;
      Shdr->sh_link = linkSection(OScn.Link);
      Shdr->sh_info =
          getShInfoValue<ELFT>(SectionK, Lout, Links, OrderedGlobals);
      Shdr->sh_addralign = OScn.MaxAlign;
      Shdr->sh_entsize = elfSectionEntSize<ELFT>(SectionK);

      ++Shdr;
    }
  });

  return Shdr;
}

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    const std::vector<const Symbol *> &OrderedGlobals,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    const std::vector<const Symbol *> &OrderedGlobals,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    const std::vector<const Symbol *> &OrderedGlobals,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    const std::vector<const Symbol *> &OrderedGlobals,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *;
