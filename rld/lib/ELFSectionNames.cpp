//===- lib/ELFSectionNames.cpp --------------------------------------------===//
//*  _____ _     _____ ____            _   _              *
//* | ____| |   |  ___/ ___|  ___  ___| |_(_) ___  _ __   *
//* |  _| | |   | |_  \___ \ / _ \/ __| __| |/ _ \| '_ \  *
//* | |___| |___|  _|  ___) |  __/ (__| |_| | (_) | | | | *
//* |_____|_____|_|   |____/ \___|\___|\__|_|\___/|_| |_| *
//*                                                       *
//*  _   _                            *
//* | \ | | __ _ _ __ ___   ___  ___  *
//* |  \| |/ _` | '_ ` _ \ / _ \/ __| *
//* | |\  | (_| | | | | | |  __/\__ \ *
//* |_| \_|\__,_|_| |_| |_|\___||___/ *
//*                                   *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELFSectionNames.h"

#include "rld/ELF.h"

using namespace rld;

#define RLD_ELF_SECTION_NAMES                                                  \
  ELF_SECTION_NAME(bss, ".bss")                                                \
  ELF_SECTION_NAME(data, ".data")                                              \
  ELF_SECTION_NAME(debug_line, ".debug_line")                                  \
  ELF_SECTION_NAME(debug_loc, ".debug_loc")                                    \
  ELF_SECTION_NAME(debug_ranges, ".debug_ranges")                              \
  ELF_SECTION_NAME(debug_string, ".debug_str")                                 \
  ELF_SECTION_NAME(linked_definitions, "")                                     \
  ELF_SECTION_NAME(mergeable_1_byte_c_string, ".rodata.str1.1")                \
  ELF_SECTION_NAME(mergeable_2_byte_c_string, ".rodata.str2.2")                \
  ELF_SECTION_NAME(mergeable_4_byte_c_string, ".rodata.str4.4")                \
  ELF_SECTION_NAME(mergeable_const_16, ".rodata.cst16")                        \
  ELF_SECTION_NAME(mergeable_const_32, ".rodata.cst32")                        \
  ELF_SECTION_NAME(mergeable_const_4, ".rodata.cst4")                          \
  ELF_SECTION_NAME(mergeable_const_8, ".rodata.cst8")                          \
  ELF_SECTION_NAME(got, ".got")                                                \
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
  ELF_SECTION_NAME(thread_data, ".tls")                                        \
  ELF_SECTION_NAME(init_array, ".init_array")                                  \
  ELF_SECTION_NAME(fini_array, ".fini_array")

namespace {

template <SectionKind SKind> struct ElfSectionName {};
#define ELF_SECTION_NAME(K, N)                                                 \
  template <> struct ElfSectionName<SectionKind::K> {                          \
    static constexpr char name[] = N;                                          \
    static constexpr std::size_t length = pstore::array_elements(name) - 1U;   \
  };
RLD_ELF_SECTION_NAMES
#undef ELF_SECTION_NAME

#define ELF_SECTION_NAME(K, N)                                                 \
  constexpr char ElfSectionName<rld::SectionKind::K>::name[];                  \
  constexpr std::size_t ElfSectionName<rld::SectionKind::K>::length;
RLD_ELF_SECTION_NAMES
#undef ELF_SECTION_NAME

} // end anonymous namespace

#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    return {ElfSectionName<SectionKind::x>::name,                              \
            ElfSectionName<SectionKind::x>::length};
#define RLD_X(x) X(x)
static constexpr std::pair<char const *, std::size_t>
elfSectionNameAndLength(rld::SectionKind SKind) {
  switch (SKind) {
    RLD_ALL_SECTION_KINDS
  case rld::SectionKind::last:
    break;
  }
  llvm_unreachable("Unknown rld section-kind");
}
#undef RLD_X
#undef X

// build section name string table
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rld::SectionIndexedArray<uint64_t>
rld::elf::buildSectionNameStringTable(Layout *const Lout) {
  OutputSection &ShStrTab = Lout->Sections[SectionKind::shstrtab];
  ShStrTab.AlwaysEmit = true;
  ShStrTab.MaxAlign = 1U;

  SectionIndexedArray<uint64_t> NameOffsets{{uint64_t{0}}};

  auto SectionNameSize = uint64_t{1}; // Initial null.
  forEachSectionKind([&](const SectionKind SectionK) {
    if (Lout->Sections[SectionK].shouldEmit()) {
      NameOffsets[SectionK] = SectionNameSize;
      SectionNameSize += elfSectionNameAndLength(SectionK).second + 1U;
    }
  });

  ShStrTab.FileSize = SectionNameSize;
  return NameOffsets;
}

// section name table writer
// ~~~~~~~~~~~~~~~~~~~~~~~~~
// Produce the section names string table.
uint8_t *rld::elf::sectionNameTableWriter(uint8_t *Data, const Layout &Lout) {
  // First entry is the empty string.
  *(Data++) = '\0';
  forEachSectionKind([&](const SectionKind SectionK) {
    if (Lout.Sections[SectionK].shouldEmit()) {
      std::pair<char const *, std::size_t> const NameAndLength =
          elfSectionNameAndLength(SectionK);
      std::memcpy(Data, NameAndLength.first, NameAndLength.second);
      assert(NameAndLength.second == std::strlen(NameAndLength.first));
      Data += NameAndLength.second;
      *(Data++) = '\0';
    }
  });

  return Data;
}
