//===- MCRepoObjectTargetWriter.cpp - Repository Target Writer Subclass ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoObjectWriter.h"

using namespace llvm;

MCRepoObjectTargetWriter::MCRepoObjectTargetWriter() {}

bool MCRepoObjectTargetWriter::needsRelocateWithSymbol(const MCSymbol &Sym,
                                                       unsigned Type) const {
  return false;
}

void MCRepoObjectTargetWriter::sortRelocs(
    const MCAssembler &Asm, std::vector<RepoRelocationEntry> &Relocs) {}
