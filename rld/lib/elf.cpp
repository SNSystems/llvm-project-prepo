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
    Shdr->sh_type = elf_section_type<ELFT>(Kind);
    Shdr->sh_flags = elf_section_flags<ELFT>(Kind);
    Shdr->sh_addr = 0;                  // Sec->Address;
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
