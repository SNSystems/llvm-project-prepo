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

#include "llvm/Support/AlignOf.h"

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
ELF_SECTION_NAME(dependent, "");

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
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr,
    const rld::FileRegion &TargetDataRegion, const rld::LayoutOutput &Layout,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr * {

  auto &OS = llvm::dbgs();
  OS << "ELF Program Headers\n";

  SegmentIndexedArray<uint64_t> SegmentDataOffsets;
  std::fill(std::begin(SegmentDataOffsets), std::end(SegmentDataOffsets),
            std::numeric_limits<uint64_t>::max());

  for (auto SegmentK = firstSegmentKind(); SegmentK != SegmentKind::last;
       ++SegmentK) {
    if (SegmentK != rld::SegmentKind::discard) {
      for (auto SectionK = firstSectionKind(); SectionK != SectionKind::last;
           ++SectionK) {
        if (!Layout[SegmentK].Sections[SectionK].empty()) {
          SegmentDataOffsets[SegmentK] = std::min(SegmentDataOffsets[SegmentK],
                                                  ElfSections[SectionK].Offset);
        }
      }
    }
  }

  for_each_segment(Layout, [&OS, &Phdr, &SegmentDataOffsets, &TargetDataRegion](
                               const SegmentKind Kind, const Segment &Segment) {
    OS << "Segment: " << Kind << '\n';

    if (Segment.shouldEmit()) {
      auto SegmentDataOffset = SegmentDataOffsets[Kind];
      if (Segment.FileSize == 0) {
        SegmentDataOffset = 0;
      } else {
        assert(SegmentDataOffset != std::numeric_limits<uint64_t>::max());
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
      const size_t MaxAlign = Segment.MaxAlign.load();
      assert(MaxAlign == 0 || llvm::countPopulation(MaxAlign) == 1);
      Phdr->p_align = hasPhysicalAddress(Kind) ? MaxAlign : 0;

      OS << "  type=" << ElfSegmentType<ELFT>{Phdr->p_type} << '\n';
      OS << "  vaddr=" << format_hex(Phdr->p_vaddr) << '\n';
      OS << "  paddr=" << format_hex(Phdr->p_paddr) << '\n';
      OS << "  offset=" << format_hex(Phdr->p_offset) << '\n';
      OS << "  memsz=" << format_hex(Phdr->p_memsz) << '\n';
      OS << "  filesz=" << format_hex(Phdr->p_filesz) << '\n';
      OS << "  flags=" << ElfSegmentFlags<ELFT>{Phdr->p_flags} << '\n';
      OS << "  align=" << format_hex(Phdr->p_align) << '\n';
#if 0
          // “The file size may not be larger than the memory size.”
          assert(Phdr->p_filesz <= Phdr->p_memsz);

          // “loadable process segments must have congruent values for p_vaddr
          // and p_offset, modulo the page size”. That is, p_vaddr ≡
          // p_offset(mod p_align).
          assert(Phdr->p_type != llvm::ELF::PT_LOAD ||
                 (Phdr->p_vaddr % Phdr->p_align) ==
                     (Phdr->p_offset % Phdr->p_align));
#endif
      ++Phdr;
      //      TargetDataOffset += Segment.FileSize;
    }
  });
  return Phdr;
}

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *Phdr,
    const rld::FileRegion &TargetDataRegion, const rld::LayoutOutput &Layout,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *Phdr,
    const rld::FileRegion &TargetDataRegion, const rld::LayoutOutput &Layout,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *Phdr,
    const rld::FileRegion &TargetDataRegion, const rld::LayoutOutput &Layout,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Phdr *;

template auto rld::elf::emitProgramHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *Phdr,
    const rld::FileRegion &TargetDataRegion, const rld::LayoutOutput &Layout,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Phdr *;

template <typename ELFT>
auto rld::elf::emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr * {

  // The Null section.
  std::memset(Shdr, 0, sizeof(*Shdr));
  ++Shdr;

  for (rld::SectionKind Kind = rld::firstSectionKind();
       Kind != rld::SectionKind::last; ++Kind) {
    ElfSectionInfo const &Section = ElfSections[Kind];
    if (!Section.Emit) {
      continue;
    }

    std::memset(Shdr, 0, sizeof(*Shdr));
    Shdr->sh_name = Section.NameOffset;
    Shdr->sh_type = elfSectionType<ELFT>(Kind);
    Shdr->sh_flags = elfSectionFlags<ELFT>(Kind);
    Shdr->sh_addr = Section.Address;    // Sec->Address;
    Shdr->sh_offset = Section.Offset;   // File offset of section data, in bytes
    Shdr->sh_size = Section.Size;
    //  Elf_Word sh_link;      // Section type-specific header table index link
    //  Elf_Word sh_info;      // Section type-specific extra information
    Shdr->sh_addralign = Section.Align; // Sec->AddressAlign;
    Shdr->sh_entsize = elf_section_entsize<ELFT>(Kind);

    ++Shdr;
  }
  return Shdr;
}

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64LE>(
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *Shdr,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *Shdr,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *Shdr,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *Shdr,
    const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *;

char *rld::elf::emitSectionHeaderStringTable(
    char *SectionNamePtr, const SectionArray<ElfSectionInfo> &ElfSections) {
  // First entry is the empty string.
  *(SectionNamePtr++) = '\0';
  for (rld::SectionKind Kind = rld::firstSectionKind();
       Kind != rld::SectionKind::last; ++Kind) {
    ElfSectionInfo const &Section = ElfSections[Kind];

    if (Section.Emit) {
      std::pair<char const *, std::size_t> const NameAndLength =
          elfSectionNameAndLength(Kind);
      std::memcpy(SectionNamePtr, NameAndLength.first, NameAndLength.second);
      assert(NameAndLength.second == std::strlen(NameAndLength.first));
      SectionNamePtr += NameAndLength.second;
      *(SectionNamePtr++) = '\0';
    }
  }
  return SectionNamePtr;
}
