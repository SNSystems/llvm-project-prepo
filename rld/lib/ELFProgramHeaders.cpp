#include "rld/ELFProgramHeaders.h"

#include "rld/Algorithm.h"

using namespace rld;

namespace {

constexpr auto DebugType = "rld-ELF";

template <typename ELFT>
constexpr auto elfSegmentKind(const SegmentKind Kind) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Word {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SegmentKind::phdr:
    return Elf_Word{llvm::ELF::PT_PHDR};

  case SegmentKind::data:
  case SegmentKind::rodata:
  case SegmentKind::text:
    return Elf_Word{llvm::ELF::PT_LOAD};

  case SegmentKind::tls:
    return Elf_Word{llvm::ELF::PT_TLS};

  case SegmentKind::gnu_stack:
    return Elf_Word{llvm::ELF::PT_GNU_STACK};

  case SegmentKind::discard:
  case SegmentKind::last:
    assert(false); // Never appears in the layout.
    return Elf_Word{};
  }
  llvm_unreachable("Invalid rld SegmentKind");
}

constexpr bool hasPhysicalAddress(const SegmentKind Kind) {
  switch (Kind) {
  case SegmentKind::phdr:
  case SegmentKind::data:
  case SegmentKind::rodata:
  case SegmentKind::text:
  case SegmentKind::tls:
    return true;

  case SegmentKind::gnu_stack:
  case SegmentKind::discard:
  case SegmentKind::last:
    return false;
  }
  llvm_unreachable("Invalid rld SegmentKind");
}

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
typename llvm::object::ELFFile<ELFT>::Elf_Phdr *rld::elf::emitProgramHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr, Context &Ctxt,
    const FileRegion &TargetDataRegion, const FileRegion &SegmentTableRegion,
    const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets) {

  auto DebugHeading =
      makeOnce([](llvm::raw_ostream &OS) { OS << "ELF Program Headers\n"; });
#ifndef NDEBUG
  auto *const Start = Phdr;
#endif
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

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *Phdr,
    Context &Ctxt, const FileRegion &TargetDataRegion,
    const FileRegion &SegmentTableRegion, const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *Phdr,
    Context &Ctxt, const FileRegion &TargetDataRegion,
    const FileRegion &SegmentTableRegion, const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *Phdr,
    Context &Ctxt, const FileRegion &TargetDataRegion,
    const FileRegion &SegmentTableRegion, const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *Phdr,
    Context &Ctxt, const FileRegion &TargetDataRegion,
    const FileRegion &SegmentTableRegion, const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *;
