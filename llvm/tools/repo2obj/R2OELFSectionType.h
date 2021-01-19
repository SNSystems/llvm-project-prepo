//===- R2OELFSectionType.h ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
#define LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H

#include "pstore/mcrepo/fragment.hpp"

#define LLVM_REPO2OBJ_ELF_SECTION_TYPE                                         \
  X(bss)                                                                       \
  X(data)                                                                      \
  X(debug_line)                                                                \
  X(debug_ranges)                                                              \
  X(debug_string)                                                              \
  X(fini_array)                                                                \
  X(init_array)                                                                \
  X(interp)                                                                    \
  X(mergeable_1_byte_c_string)                                                 \
  X(mergeable_2_byte_c_string)                                                 \
  X(mergeable_4_byte_c_string)                                                 \
  X(mergeable_const_16)                                                        \
  X(mergeable_const_32)                                                        \
  X(mergeable_const_4)                                                         \
  X(mergeable_const_8)                                                         \
  X(read_only)                                                                 \
  X(rel_ro)                                                                    \
  X(text)                                                                      \
  X(thread_bss)                                                                \
  X(thread_data)

#define X(t) t,
enum class ELFSectionType { LLVM_REPO2OBJ_ELF_SECTION_TYPE };
#undef X

namespace llvm {
class raw_ostream;
}
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST);

#endif // LLVM_TOOLS_REPO2OBJ_ELFSECTIONTYPE_H
