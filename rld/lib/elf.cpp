//*       _  __  *
//*   ___| |/ _| *
//*  / _ \ | |_  *
//* |  __/ |  _| *
//*  \___|_|_|   *
//*              *
//===- lib/elf.cpp --------------------------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
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
  constexpr std::size_t rld::elf::ElfSectionName<rld::SectionKind::K>::length

ELF_SECTION_NAME(text, ".text");
ELF_SECTION_NAME(data, ".data");
ELF_SECTION_NAME(bss, ".bss");
ELF_SECTION_NAME(rel_ro, ".data.rel");
ELF_SECTION_NAME(mergeable_1_byte_c_string, ".rodata.str1.1");
ELF_SECTION_NAME(mergeable_2_byte_c_string, ".rodata.str2.2");
ELF_SECTION_NAME(mergeable_4_byte_c_string, ".rodata.str4.4");
ELF_SECTION_NAME(mergeable_const_4, ".rodata.cst4");
ELF_SECTION_NAME(mergeable_const_8, ".rodata.cst8");
ELF_SECTION_NAME(mergeable_const_16, ".rodata.cst16");
ELF_SECTION_NAME(mergeable_const_32, ".rodata.cst32");
ELF_SECTION_NAME(read_only, ".rodata");
ELF_SECTION_NAME(thread_data, ".tls");
ELF_SECTION_NAME(thread_bss, ".tbss");
ELF_SECTION_NAME(debug_line, ".debug_line");
ELF_SECTION_NAME(debug_string, ".debug_str");
ELF_SECTION_NAME(debug_ranges, ".debug_ranges");
ELF_SECTION_NAME(interp, ".interp");
ELF_SECTION_NAME(shstrtab, ".shstrtab");
ELF_SECTION_NAME(strtab, ".strtab");
ELF_SECTION_NAME(linked_definitions, "");

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

template <typename ELFT> static void initStandardSections() {
  // null section
  typename ELFT::Elf_Shdr SH;
  zero(SH);
  // assert(SectionHeaders.size() == SectionIndices::Null);
  // SectionHeaders.push_back(SH);

  // string table
  zero(SH);
  SH.sh_name = 0; // Strings.insert(Generated.add(".strtab"));
  SH.sh_type = llvm::ELF::SHT_STRTAB;
  // assert(SectionHeaders.size() == SectionIndices::StringTab);
  // SectionHeaders.push_back(SH);

  // Symbol table
  zero(SH);
  SH.sh_name = 0; // Strings.insert(Generated.add(".symtab"));
  SH.sh_type = llvm::ELF::SHT_SYMTAB;
  SH.sh_link = 0; // SectionIndices::StringTab;
  SH.sh_entsize = sizeof(typename ELFT::Elf_Sym);
  SH.sh_addralign = alignof(typename ELFT::Elf_Sym);
  // assert(SectionHeaders.size() == SectionIndices::SymTab);
  // SectionHeaders.push_back(SH);
}

// FIXME: copied from repo2obj WriteHelpers.h
template <typename Ty>
static void writeAlignmentPadding(llvm::raw_ostream &OS) {
  constexpr auto Alignment = alignof(Ty);
  uint8_t Padding[Alignment] = {0};
  OS.write(reinterpret_cast<char const *>(Padding),
           Alignment - (OS.tell() % Alignment));
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
    const rld::FileRegion &TargetDataRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr * {

  auto DebugHeading =
      makeOnce([](llvm::raw_ostream &OS) { OS << "ELF Program Headers\n"; });

  Lout.forEachSegment([&](const SegmentKind Kind, const Segment &Segment) {
    if (!Segment.shouldEmit()) {
      return;
    }

    uint64_t SegmentDataOffset = SegmentDataOffsets[Kind].getValueOr(0U);
    if (Segment.HasOutputSections) {
        SegmentDataOffset += TargetDataRegion.offset();
        assert(SegmentDataOffset >= TargetDataRegion.offset() &&
            SegmentDataOffset + Segment.FileSize < TargetDataRegion.end());
    }

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
  return Phdr;
}

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *Phdr,
    Context &Ctxt, const rld::FileRegion &TargetDataRegion,
    const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *;

template <typename ELFT>
auto rld::elf::emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr, const Layout &Lout,
    const SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr * {

  // The Null section.
  std::memset(Shdr, 0, sizeof(*Shdr));
  ++Shdr;

  for (SectionKind SectionK = firstSectionKind(); SectionK != SectionKind::last;
       ++SectionK) {
    const OutputSection &OScn = Lout.Sections[SectionK];
    if (OScn.shouldEmit()) {
      std::memset(Shdr, 0, sizeof(*Shdr));
      Shdr->sh_name = NameOffsets[SectionK];
      Shdr->sh_type = elfSectionType<ELFT>(SectionK);
      Shdr->sh_flags = elfSectionFlags<ELFT>(SectionK);
      Shdr->sh_addr = OScn.VirtualAddr;
      assert(SectionFileOffsets[SectionK].hasValue() &&
             "Emitting a section for which we have no computed a file offset");
      Shdr->sh_offset =
          *SectionFileOffsets[SectionK] +
          TargetDataOffset; // File offset of section data, in bytes
      Shdr->sh_size = OScn.FileSize;
      //  Elf_Word sh_link;      // Section type-specific header table index
      //  link Elf_Word sh_info;      // Section type-specific extra information
      Shdr->sh_addralign = OScn.MaxAlign;
      Shdr->sh_entsize = elfSectionEntSize<ELFT>(SectionK);

      ++Shdr;
    }
  }

  return Shdr;
}

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<uint64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *;
