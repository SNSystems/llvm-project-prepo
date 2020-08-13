//*       _  __  *
//*   ___| |/ _| *
//*  / _ \ | |_  *
//* |  __/ |  _| *
//*  \___|_|_|   *
//*              *
//===- include/rld/elf.h --------------------------------------------------===//
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

inline bool hasPhysicalAddress(rld::SegmentKind const Kind) {
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

  case SectionKind::dependent:
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
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_EXECINSTR};

  case SectionKind::bss:
  case SectionKind::data:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_WRITE};

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

  case SectionKind::debug_line:
  case SectionKind::debug_string:
  case SectionKind::debug_ranges:
  case SectionKind::interp:
  case SectionKind::strtab:
  case SectionKind::shstrtab:
    return Elf_Word{};

  case SectionKind::dependent:
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
  case SectionKind::text:
  case SectionKind::data:
  case SectionKind::bss:
  case SectionKind::rel_ro:
  case SectionKind::read_only:
  case SectionKind::thread_data:
  case SectionKind::thread_bss:
  case SectionKind::debug_line:
  case SectionKind::debug_string:
  case SectionKind::debug_ranges:
  case SectionKind::interp:
  case SectionKind::dependent:
  case SectionKind::shstrtab:
  case SectionKind::strtab:
    break;
  case SectionKind::last:
    llvm_unreachable("an impossible section kind");
  }
  return Elf_Word{0};
}

template <rld::SectionKind SKind> struct ElfSectionName {};
#define ELF_SECTION_NAME(K, N)                                                 \
  template <> struct ElfSectionName<rld::SectionKind::K> {                     \
    static constexpr char name[] = N;                                          \
    static constexpr std::size_t length = pstore::array_elements(name) - 1U;   \
  }

ELF_SECTION_NAME(bss, ".bss");
ELF_SECTION_NAME(data, ".data");
ELF_SECTION_NAME(debug_line, ".debug_line");
ELF_SECTION_NAME(debug_ranges, ".debug_ranges");
ELF_SECTION_NAME(debug_string, ".debug_str");
ELF_SECTION_NAME(dependent, "");
ELF_SECTION_NAME(interp, ".interp");
ELF_SECTION_NAME(mergeable_1_byte_c_string, ".rodata.str1.1");
ELF_SECTION_NAME(mergeable_2_byte_c_string, ".rodata.str2.2");
ELF_SECTION_NAME(mergeable_4_byte_c_string, ".rodata.str4.4");
ELF_SECTION_NAME(mergeable_const_16, ".rodata.cst16");
ELF_SECTION_NAME(mergeable_const_32, ".rodata.cst32");
ELF_SECTION_NAME(mergeable_const_4, ".rodata.cst4");
ELF_SECTION_NAME(mergeable_const_8, ".rodata.cst8");
ELF_SECTION_NAME(read_only, ".rodata");
ELF_SECTION_NAME(rel_ro, ".data.rel");
ELF_SECTION_NAME(shstrtab, ".shstrtab");
ELF_SECTION_NAME(strtab, ".strtab");
ELF_SECTION_NAME(text, ".text");
ELF_SECTION_NAME(thread_bss, ".tbss");
ELF_SECTION_NAME(thread_data, ".tls");

#undef ELF_SECTION_NAME

#define X(x)                                                                   \
  case rld::SectionKind::x:                                                    \
    return {ElfSectionName<rld::SectionKind::x>::name,                         \
            ElfSectionName<rld::SectionKind::x>::length};

inline std::pair<char const *, std::size_t>
elfSectionNameAndLength(rld::SectionKind SKind) {
  switch (SKind) {
    PSTORE_MCREPO_SECTION_KINDS
  case rld::SectionKind::shstrtab:
    return {ElfSectionName<rld::SectionKind::shstrtab>::name,
            ElfSectionName<rld::SectionKind::shstrtab>::length};
  case rld::SectionKind::strtab:
    return {ElfSectionName<rld::SectionKind::strtab>::name,
            ElfSectionName<rld::SectionKind::strtab>::length};
  case rld::SectionKind::last:
    break;
  }
  llvm_unreachable("Unknown rld section-kind");
}

#undef X

struct ElfSectionInfo {
  /// If the section will appear in the memory image of a process, this member
  /// gives the address at which the section's first byte should reside.
  /// Otherwise, the member contains 0. Provides the sh_addr value for the
  /// Elf_Shdr structure.
  uint64_t Address = 0;
  uint64_t Offset = 0;
  uint64_t Size = 0;
  uint64_t NameOffset = 0;
  bool Emit = false;
  /// The alignment of the section data. Always a power of 2.
  unsigned Align = 1U;
};

template <typename ELFT>
auto emitProgramHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr,
    const rld::FileRegion &TargetDataRegion, const rld::LayoutOutput &Layout,
    const rld::SegmentIndexedArray<uint64_t> &SegmentDataOffsets) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *;

template <typename ELFT>
auto emitSectionHeaders(typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr,
                        const SectionArray<ElfSectionInfo> &ElfSections) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *;

// Produce the section names string table.
char *
emitSectionHeaderStringTable(char *SectionNamePtr,
                             const SectionArray<ElfSectionInfo> &ElfSections);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELF_H
