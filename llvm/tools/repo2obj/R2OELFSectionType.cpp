//===- R2OELFSectionType.cpp ----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "R2OELFSectionType.h"
#include "llvm/Support/raw_ostream.h"

#define X(k)                                                                   \
  case ELFSectionType::k:                                                      \
    OS << #k;                                                                  \
    break;

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ELFSectionType ST) {
  switch (ST) { LLVM_REPO2OBJ_ELF_SECTION_TYPE }
  return OS;
}
#undef X
