//===- R2OELFOutputSection.cpp --------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "R2OELFOutputSection.h"

using namespace llvm;
using namespace pstore::repo;

namespace details {

SectionMap const SectionAttributes{
    {ELFSectionType::bss,
     {".bss", ELF::SHT_NOBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
    {ELFSectionType::data,
     {".data", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
    {ELFSectionType::rel_ro,
     {".data.rel.ro", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
    {ELFSectionType::text,
     {".text", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_EXECINSTR}},
    {ELFSectionType::mergeable_1_byte_c_string,
     {".rodata.str1", ELF::SHT_PROGBITS,
      ELF::SHF_ALLOC | ELF::SHF_MERGE | ELF::SHF_STRINGS}},
    {ELFSectionType::mergeable_2_byte_c_string,
     {".rodata.str2", ELF::SHT_PROGBITS,
      ELF::SHF_ALLOC | ELF::SHF_MERGE | ELF::SHF_STRINGS}},
    {ELFSectionType::mergeable_4_byte_c_string,
     {".rodata.str4", ELF::SHT_PROGBITS,
      ELF::SHF_ALLOC | ELF::SHF_MERGE | ELF::SHF_STRINGS}},
    {ELFSectionType::mergeable_const_4,
     {".rodata.cst4", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
    {ELFSectionType::mergeable_const_8,
     {".rodata.cst8", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
    {ELFSectionType::mergeable_const_16,
     {".rodata.cst16", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
    {ELFSectionType::mergeable_const_32,
     {".rodata.cst32", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
    {ELFSectionType::read_only, {".rodata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC}},
    {ELFSectionType::thread_bss,
     {".tbss", ELF::SHT_NOBITS,
      ELF::SHF_ALLOC | ELF::SHF_WRITE | ELF::SHF_TLS}},
    {ELFSectionType::thread_data,
     {".tdata", ELF::SHT_PROGBITS,
      ELF::SHF_ALLOC | ELF::SHF_WRITE | ELF::SHF_TLS}},

    // X (Metadata)
    {ELFSectionType::debug_line, {".debug_line", ELF::SHT_PROGBITS, 0}},

    {ELFSectionType::init_array,
     {".init_array", ELF::SHT_INIT_ARRAY, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
    {ELFSectionType::fini_array,
     {".fini_array", ELF::SHT_FINI_ARRAY, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
};

} // namespace details
