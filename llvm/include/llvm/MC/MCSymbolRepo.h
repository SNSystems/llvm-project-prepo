//===- MCSymbolRepo.h -  ----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_MC_MCSYMBOLREPO_H
#define LLVM_MC_MCSYMBOLREPO_H

#include "llvm/IR/RepoDefinition.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSymbol.h"

namespace llvm {

class MCSymbolRepo : public MCSymbol {

  enum SymbolFlags : uint16_t {
    SF_ExternalWeak = 0x0001,
  };

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

  bool isExternalWeak() const {
      return getFlags() & SF_ExternalWeak;
  }
  void setExternalWeak() const {
      modifyFlags(SF_ExternalWeak, SF_ExternalWeak);
  }

};

} // end namespace llvm

#endif
