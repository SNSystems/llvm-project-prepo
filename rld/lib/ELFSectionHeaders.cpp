//===- lib/ELFSectionHeaders.cpp ------------------------------------------===//
//*  _____ _     _____ ____            _   _              *
//* | ____| |   |  ___/ ___|  ___  ___| |_(_) ___  _ __   *
//* |  _| | |   | |_  \___ \ / _ \/ __| __| |/ _ \| '_ \  *
//* | |___| |___|  _|  ___) |  __/ (__| |_| | (_) | | | | *
//* |_____|_____|_|   |____/ \___|\___|\__|_|\___/|_| |_| *
//*                                                       *
//*  _   _                _                *
//* | | | | ___  __ _  __| | ___ _ __ ___  *
//* | |_| |/ _ \/ _` |/ _` |/ _ \ '__/ __| *
//* |  _  |  __/ (_| | (_| |  __/ |  \__ \ *
//* |_| |_|\___|\__,_|\__,_|\___|_|  |___/ *
//*                                        *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELFSectionHeaders.h"

#include "rld/LayoutBuilder.h"

using namespace rld;

/// \param Lout The layout.
/// \returns A table which contains the index of section-header record for each
///   of the various section-kinds.
static SectionIndexedArray<unsigned> getLinkSections(const Layout &Lout) {
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

template <typename ELFT>
static constexpr auto elfSectionType(const SectionKind Kind) {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SectionKind::data:
  case SectionKind::debug_line:
  case SectionKind::debug_ranges:
  case SectionKind::debug_string:
  case SectionKind::mergeable_1_byte_c_string:
  case SectionKind::mergeable_2_byte_c_string:
  case SectionKind::mergeable_4_byte_c_string:
  case SectionKind::mergeable_const_16:
  case SectionKind::mergeable_const_32:
  case SectionKind::mergeable_const_4:
  case SectionKind::mergeable_const_8:
  case SectionKind::got:
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
  case SectionKind::init_array:
    return Elf_Word{llvm::ELF::SHT_INIT_ARRAY};
  case SectionKind::fini_array:
    return Elf_Word{llvm::ELF::SHT_FINI_ARRAY};

  case SectionKind::linked_definitions:
  case SectionKind::last:
    assert(false); //
    return Elf_Word{};
  }
  llvm_unreachable("Bad section kind");
}

template <typename ELFT>
static constexpr auto elfSectionFlags(const SectionKind Kind) {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SectionKind::text:
  case SectionKind::plt:
    return Elf_Word{llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_EXECINSTR};

  case SectionKind::bss:
  case SectionKind::data:
  case SectionKind::fini_array:
  case SectionKind::got:
  case SectionKind::gotplt:
  case SectionKind::init_array:
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

template <typename ELFT>
static constexpr auto elfSectionEntSize(const SectionKind Kind) {
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
  case SectionKind::got:
  case SectionKind::gotplt:
  case SectionKind::plt:
  case SectionKind::init_array:
  case SectionKind::fini_array:
    return Elf_Word{8};
  case SectionKind::rela_plt:
    return Elf_Word{sizeof(typename llvm::object::ELFFile<ELFT>::Elf_Rela)};
  case SectionKind::bss:
  case SectionKind::data:
  case SectionKind::debug_line:
  case SectionKind::debug_ranges:
  case SectionKind::debug_string:
  case SectionKind::linked_definitions:
  case SectionKind::read_only:
  case SectionKind::rel_ro:
  case SectionKind::shstrtab:
  case SectionKind::strtab:
  case SectionKind::text:
  case SectionKind::thread_bss:
  case SectionKind::thread_data:
    return Elf_Word{0};
  case SectionKind::last:
    break;
  }
  llvm_unreachable("an impossible section kind");
}

namespace {

template <typename ELFT, rld::SectionKind SKind> struct ShInfoValue {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;

  Elf_Word operator()(const rld::Layout &Lout,
                      const rld::SectionIndexedArray<unsigned> &Links,
                      size_t /*LocalsSize*/) const {
    const rld::SectionKind L = Lout.Sections[SKind].Info;
    return Elf_Word{L == rld::SectionKind::last ? 0 : Links[L]};
  }
};

template <typename ELFT> struct ShInfoValue<ELFT, rld::SectionKind::symtab> {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;

  Elf_Word operator()(const rld::Layout & /*Lout*/,
                      const rld::SectionIndexedArray<unsigned> & /*Links*/,
                      size_t LocalsSize) const {
    // (plus one to allow for the initial null symbol).
    return static_cast<Elf_Word>(LocalsSize + 1);
  }
};

} // end anonymous namespace

template <typename ELFT>
static typename llvm::object::ELFFile<ELFT>::Elf_Word
getShInfoValue(rld::SectionKind SectionK, const rld::Layout &Lout,
               const rld::SectionIndexedArray<unsigned> &Links,
               size_t LocalsSize) {
  switch (SectionK) {
#define X(a)                                                                   \
  case rld::SectionKind::a:                                                    \
    return ShInfoValue<ELFT, SectionKind::a>{}(Lout, Links, LocalsSize);
#define RLD_X(a) X(a)
    RLD_ALL_SECTION_KINDS
#undef RLD_X
#undef X
  case rld::SectionKind::last:
    llvm_unreachable("We should not process the last value");
  }
  llvm_unreachable("Unknown section kind");
}

template <typename ELFT>
auto rld::elf::emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr, const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    size_t LocalsSize, uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr * {

  // A table which contains the index of section-header record for each of the
  // various section-kinds.
  const SectionIndexedArray<unsigned> Links = getLinkSections(Lout);
  // The Null section.
  std::memset(Shdr, 0, sizeof(*Shdr));
  ++Shdr;

  forEachSectionKind([&](const SectionKind SectionK) {
    const OutputSection &OScn = Lout.Sections[SectionK];
    if (OScn.shouldEmit()) {
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
      Shdr->sh_link = OScn.Link == SectionKind::last ? 0 : Links[OScn.Link];
      Shdr->sh_info = getShInfoValue<ELFT>(SectionK, Lout, Links, LocalsSize);
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
    size_t LocalsSize, uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF64LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF64BE>(
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    size_t LocalsSize, uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF64BE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32LE>(
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    size_t LocalsSize, uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF32LE>::Elf_Shdr *;

template auto rld::elf::emitSectionHeaders<llvm::object::ELF32BE>(
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *Shdr,
    const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    size_t LocalsSize, uint64_t TargetDataOffset) ->
    typename llvm::object::ELFFile<llvm::object::ELF32BE>::Elf_Shdr *;
