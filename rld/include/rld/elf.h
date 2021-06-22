//===- include/rld/elf.h ----------------------------------*- mode: C++ -*-===//
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
#ifndef RLD_ELF_H
#define RLD_ELF_H

#include "llvm/ADT/Optional.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/raw_ostream.h"

#include "rld/LayoutBuilder.h"
#include "rld/SectionArray.h"

#include <memory>
#include <type_traits>

namespace llvm {
class raw_pwrite_stream;
class Triple;
} // namespace llvm

namespace rld {
namespace elf {

namespace details {

llvm::ErrorOr<unsigned>
machineFromTriple(llvm::Optional<llvm::Triple> const &Triple);

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty const &T) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), sizeof(T));
  return sizeof(T);
}

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty *T, std::size_t Size) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), Size);
  return Size;
}

} // end namespace details

template <class ELFT>
auto initELFHeader(const unsigned Machine) -> typename ELFT::Ehdr {
  typename ELFT::Ehdr Header;
  Header.e_ident[llvm::ELF::EI_MAG0] = 0x7f;
  Header.e_ident[llvm::ELF::EI_MAG1] = 'E';
  Header.e_ident[llvm::ELF::EI_MAG2] = 'L';
  Header.e_ident[llvm::ELF::EI_MAG3] = 'F';
  Header.e_ident[llvm::ELF::EI_CLASS] =
      ELFT::Is64Bits ? llvm::ELF::ELFCLASS64 : llvm::ELF::ELFCLASS32;
  Header.e_ident[llvm::ELF::EI_DATA] =
      (ELFT::TargetEndianness == llvm::support::little)
          ? llvm::ELF::ELFDATA2LSB
          : llvm::ELF::ELFDATA2MSB;
  Header.e_ident[llvm::ELF::EI_VERSION] = llvm::ELF::EV_CURRENT;
  Header.e_ident[llvm::ELF::EI_OSABI] = llvm::ELF::ELFOSABI_NONE;
  Header.e_ident[llvm::ELF::EI_ABIVERSION] = 0;
  Header.e_type = llvm::ELF::ET_REL;
  Header.e_machine = Machine;
  Header.e_version = llvm::ELF::EV_CURRENT;
  Header.e_entry = 0;
  Header.e_phoff = 0; // patched up later
  Header.e_shoff = 0; // patched up later
  Header.e_flags = 0;
  Header.e_ehsize = sizeof(typename ELFT::Ehdr);
  Header.e_phentsize = sizeof(typename ELFT::Phdr);
  Header.e_phnum = 0; // patched up later
  Header.e_shentsize = sizeof(typename ELFT::Shdr);
  Header.e_shnum = 0;    // patched up later.
  Header.e_shstrndx = 0; // patched up later.
  return Header;
}

#if 0
template <typename ELFT>
std::error_code writeELF(llvm::raw_pwrite_stream &OS, Context const &Ctxt,
                         std::unique_ptr<rld::LayoutOutput> Layout) {
  llvm::ErrorOr<unsigned> const Machine =
      details::machineFromTriple(Ctxt.triple());
  if (!Machine) {
    return Machine.getError();
  }

  typename ELFT::Ehdr Header = details::initELFHeader<ELFT>(*Machine);
  details::writeRaw(OS, Header);
  return std::error_code{};
}
#endif

template <typename ELFT>
constexpr auto elfSegmentKind(rld::SegmentKind Kind) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Word {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case rld::SegmentKind::phdr:
    return Elf_Word{llvm::ELF::PT_PHDR};

  case rld::SegmentKind::interp:
    return Elf_Word{llvm::ELF::PT_INTERP};

  case rld::SegmentKind::data:
  case rld::SegmentKind::rodata:
  case rld::SegmentKind::text:
    return Elf_Word{llvm::ELF::PT_LOAD};

  case rld::SegmentKind::tls:
    return Elf_Word{llvm::ELF::PT_TLS};

  case rld::SegmentKind::gnu_stack:
    return Elf_Word{llvm::ELF::PT_GNU_STACK};

  case rld::SegmentKind::discard:
  case rld::SegmentKind::last:
    assert(false); // Never appears in the layout.
    return Elf_Word{};
  }
  llvm_unreachable("Invalid rld SegmentKind");
}

template <typename ELFT>
constexpr auto elfSegmentFlags(rld::SegmentKind const Kind) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Word {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case rld::SegmentKind::data:
  case rld::SegmentKind::gnu_stack:
    return Elf_Word{llvm::ELF::PF_R | llvm::ELF::PF_W};

  case rld::SegmentKind::phdr:
  case rld::SegmentKind::interp:
  case rld::SegmentKind::rodata:
  case rld::SegmentKind::tls:
    return Elf_Word{llvm::ELF::PF_R};

  case rld::SegmentKind::text:
    return Elf_Word{llvm::ELF::PF_X | llvm::ELF::PF_R};
  default:
    break;
  }
  llvm_unreachable("Invalid segment kind");
}

inline constexpr bool hasPhysicalAddress(rld::SegmentKind const Kind) {
  switch (Kind) {
  case rld::SegmentKind::phdr:
  case rld::SegmentKind::data:
  case rld::SegmentKind::rodata:
  case rld::SegmentKind::text:
  case rld::SegmentKind::tls:
    return true;

  case rld::SegmentKind::gnu_stack:
  case rld::SegmentKind::interp:
  case rld::SegmentKind::discard:
  case rld::SegmentKind::last:
    return false;
  }
  llvm_unreachable("Invalid rld SegmentKind");
}

template <typename ELFT>
constexpr unsigned char elfVisibility(pstore::repo::visibility SV) {
  switch (SV) {
  case pstore::repo::visibility::default_vis:
    return llvm::ELF::STV_DEFAULT;
  case pstore::repo::visibility::hidden_vis:
    return llvm::ELF::STV_HIDDEN;
  case pstore::repo::visibility::protected_vis:
    return llvm::ELF::STV_PROTECTED;
  }
  llvm_unreachable("Unsupported visibility type");
}

template <typename ELFT> constexpr auto elfSectionType(rld::SectionKind Kind) {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SectionKind::data:
  case SectionKind::debug_line:
  case SectionKind::debug_ranges:
  case SectionKind::debug_string:
  case SectionKind::interp:
  case SectionKind::mergeable_1_byte_c_string:
  case SectionKind::mergeable_2_byte_c_string:
  case SectionKind::mergeable_4_byte_c_string:
  case SectionKind::mergeable_const_16:
  case SectionKind::mergeable_const_32:
  case SectionKind::mergeable_const_4:
  case SectionKind::mergeable_const_8:
  case SectionKind::gotplt:
  case SectionKind::plt:
  case SectionKind::read_only:
  case SectionKind::rel_ro:
  case SectionKind::text:
  case SectionKind::thread_data:
    return Elf_Word{llvm::ELF::SHT_PROGBITS};

  case SectionKind::bss:
  case SectionKind::thread_bss:
    return Elf_Word{llvm::ELF::SHT_NOBITS};

  case SectionKind::strtab:
  case SectionKind::shstrtab:
    return Elf_Word{llvm::ELF::SHT_STRTAB};

  case SectionKind::symtab:
    return Elf_Word{llvm::ELF::SHT_SYMTAB};

  case SectionKind::rela_plt:
    return Elf_Word{llvm::ELF::SHT_RELA};

  case SectionKind::linked_definitions:
  case SectionKind::last:
    assert(false); //
    return Elf_Word{};
  }
  llvm_unreachable("Bad section kind");
}

template <typename ELFT> constexpr auto elfSectionFlags(SectionKind Kind) {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;

  switch (Kind) {
  case SectionKind::text:
  case SectionKind::plt:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_EXECINSTR};

  case SectionKind::bss:
  case SectionKind::data:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_WRITE};

  case SectionKind::gotplt:
  case SectionKind::rel_ro:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_WRITE};

  case SectionKind::mergeable_1_byte_c_string:
  case SectionKind::mergeable_2_byte_c_string:
  case SectionKind::mergeable_4_byte_c_string:
  case SectionKind::mergeable_const_4:
  case SectionKind::mergeable_const_8:
  case SectionKind::mergeable_const_16:
  case SectionKind::mergeable_const_32:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_MERGE};

  case SectionKind::read_only:
    return Elf_Word{llvm::ELF::SHF_ALLOC};

  case SectionKind::thread_data:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_TLS};

  case SectionKind::thread_bss:
    return Elf_Word{llvm::ELF::SHF_TLS};

  case SectionKind::rela_plt:
    return Elf_Word{llvm::ELF::SHF_ALLOC};

  case SectionKind::debug_line:
  case SectionKind::debug_string:
  case SectionKind::debug_ranges:
  case SectionKind::interp:
  case SectionKind::strtab:
  case SectionKind::shstrtab:
  case SectionKind::symtab:
    return Elf_Word{};

  case SectionKind::linked_definitions:
  case SectionKind::last:
    assert(false); //
    return Elf_Word{};
  }
  llvm_unreachable("Bad section kind");
}

template <typename ELFT> constexpr auto elfSectionEntSize(SectionKind Kind) {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SectionKind::mergeable_1_byte_c_string:
    return Elf_Word{1};
  case SectionKind::mergeable_2_byte_c_string:
    return Elf_Word{2};
  case SectionKind::mergeable_4_byte_c_string:
    return Elf_Word{4};
  case SectionKind::mergeable_const_4:
    return Elf_Word{4};
  case SectionKind::mergeable_const_8:
    return Elf_Word{8};
  case SectionKind::mergeable_const_16:
    return Elf_Word{16};
  case SectionKind::mergeable_const_32:
    return Elf_Word{32};
  case SectionKind::symtab:
    return Elf_Word{sizeof(typename llvm::object::ELFFile<ELFT>::Elf_Sym)};
  case SectionKind::gotplt:
  case SectionKind::plt:
    return Elf_Word{8};
  case SectionKind::rela_plt:
    return Elf_Word{sizeof(typename llvm::object::ELFFile<ELFT>::Elf_Rela)};
  case SectionKind::bss:
  case SectionKind::data:
  case SectionKind::debug_line:
  case SectionKind::debug_ranges:
  case SectionKind::debug_string:
  case SectionKind::interp:
  case SectionKind::linked_definitions:
  case SectionKind::read_only:
  case SectionKind::rel_ro:
  case SectionKind::shstrtab:
  case SectionKind::strtab:
  case SectionKind::text:
  case SectionKind::thread_bss:
  case SectionKind::thread_data:
    break;
  case SectionKind::last:
    llvm_unreachable("an impossible section kind");
  }
  return Elf_Word{0};
}

#define RLD_ELF_SECTION_NAMES                                                  \
  ELF_SECTION_NAME(bss, ".bss")                                                \
  ELF_SECTION_NAME(data, ".data")                                              \
  ELF_SECTION_NAME(debug_line, ".debug_line")                                  \
  ELF_SECTION_NAME(debug_ranges, ".debug_ranges")                              \
  ELF_SECTION_NAME(debug_string, ".debug_str")                                 \
  ELF_SECTION_NAME(linked_definitions, "")                                     \
  ELF_SECTION_NAME(interp, ".interp")                                          \
  ELF_SECTION_NAME(mergeable_1_byte_c_string, ".rodata.str1.1")                \
  ELF_SECTION_NAME(mergeable_2_byte_c_string, ".rodata.str2.2")                \
  ELF_SECTION_NAME(mergeable_4_byte_c_string, ".rodata.str4.4")                \
  ELF_SECTION_NAME(mergeable_const_16, ".rodata.cst16")                        \
  ELF_SECTION_NAME(mergeable_const_32, ".rodata.cst32")                        \
  ELF_SECTION_NAME(mergeable_const_4, ".rodata.cst4")                          \
  ELF_SECTION_NAME(mergeable_const_8, ".rodata.cst8")                          \
  ELF_SECTION_NAME(gotplt, ".got.plt")                                         \
  ELF_SECTION_NAME(rela_plt, ".rela.plt")                                      \
  ELF_SECTION_NAME(plt, ".plt")                                                \
  ELF_SECTION_NAME(read_only, ".rodata")                                       \
  ELF_SECTION_NAME(rel_ro, ".data.rel")                                        \
  ELF_SECTION_NAME(shstrtab, ".shstrtab")                                      \
  ELF_SECTION_NAME(strtab, ".strtab")                                          \
  ELF_SECTION_NAME(symtab, ".symtab")                                          \
  ELF_SECTION_NAME(text, ".text")                                              \
  ELF_SECTION_NAME(thread_bss, ".tbss")                                        \
  ELF_SECTION_NAME(thread_data, ".tls")

template <SectionKind SKind> struct ElfSectionName {};
#define ELF_SECTION_NAME(K, N)                                                 \
  template <> struct ElfSectionName<SectionKind::K> {                          \
    static constexpr char name[] = N;                                          \
    static constexpr std::size_t length = pstore::array_elements(name) - 1U;   \
  };
RLD_ELF_SECTION_NAMES
#undef ELF_SECTION_NAME

#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    return {ElfSectionName<SectionKind::x>::name,                              \
            ElfSectionName<SectionKind::x>::length};
#define RLD_X(x) X(x)
inline constexpr std::pair<char const *, std::size_t>
elfSectionNameAndLength(rld::SectionKind SKind) {
  switch (SKind) {
    PSTORE_MCREPO_SECTION_KINDS
    RLD_SECTION_KINDS
  case rld::SectionKind::last:
    break;
  }
  llvm_unreachable("Unknown rld section-kind");
}
#undef RLD_X
#undef X

template <typename ELFT>
auto emitProgramHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr, Context &Ctxt,
    const rld::FileRegion &TargetDataRegion,
    const rld::FileRegion &SegmentTableRegion, const rld::Layout &Lout,
    const rld::SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets)
    -> typename llvm::object::ELFFile<ELFT>::Elf_Phdr *;

template <typename ELFT>
auto emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr, const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    const std::vector<const Symbol *> &OrderedGlobals,
    uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *;

} // end namespace elf
} // end namespace rld

#endif // RLD_ELF_H
