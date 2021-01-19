//===- MCSectionRepo.cpp - Repo Code Section Representation -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the MCSectionRepo class.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCSectionRepo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSymbol.h"

using namespace llvm;

namespace {
unsigned idx = 0;
}

MCSectionRepo::MCSectionRepo(SectionKind K, DebugSectionKind DK,
                             MCSymbol *Begin, StringRef N,
                             repodefinition::DigestType Digest)
    : MCSection(SV_Repo, N, K, Begin), DebugKind{DK},
      Digest{std::move(Digest)}, Index{++idx} {

  assert((K.isMetadata() && DK != DebugSectionKind::None) ||
         (!K.isMetadata() && DK == DebugSectionKind::None));
}

MCSectionRepo::~MCSectionRepo() {}

MCSection *MCSectionRepo::associatedDebugLineSection(MCContext &Ctx) {
  return Ctx.getRepoSection(MCContext::RepoSection::DebugLine, Name, Digest);
}
