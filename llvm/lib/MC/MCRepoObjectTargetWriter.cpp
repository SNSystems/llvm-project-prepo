//===-- MCRepoObjectTargetWriter.cpp - Repository Target Writer Subclass---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
