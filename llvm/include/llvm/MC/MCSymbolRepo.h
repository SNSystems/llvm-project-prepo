//===- MCSymbolRepo.h -  ----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_MC_MCSYMBOLREPO_H
#define LLVM_MC_MCSYMBOLREPO_H

#include "llvm/MC/MCSymbol.h"

namespace llvm {

class MCSymbolRepo : public MCSymbol {

public:
  MCSymbolRepo(const StringMapEntry<bool> *Name, bool isTemporary)
      : MCSymbol(SymbolKindRepo, Name, isTemporary) {}

  static bool classof(const MCSymbol *S) { return S->isRepo(); }

  // A pointer to the RepoDefinition metadata of the corresponding GlobalObject.
  const RepoDefinition *CorrespondingRepoDefinition = nullptr;

  // Get the symbol full name.
  static const std::string
  getFullName(const MCContext &Ctx, const StringRef InitialName,
              const repodefinition::DigestType &Digest) {
    std::string FullName;
    StringRef PrivateGlobalPrefix = Ctx.getAsmInfo()->getPrivateGlobalPrefix();
    FullName.reserve(PrivateGlobalPrefix.size() + InitialName.size() +
                     repodefinition::DigestSize + 1);
    FullName = PrivateGlobalPrefix.str();
    FullName += InitialName;
    if (Digest != repodefinition::NullDigest) {
      FullName += ".";
      FullName += Digest.digest().str();
    }
    return FullName;
  }
};

} // end namespace llvm

#endif
