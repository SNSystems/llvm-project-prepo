//===- llvm/MC/MCRepoObjectWriter.h - Repository Writer ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCREPOOBJECTWRITER_H
#define LLVM_MC_MCREPOOBJECTWRITER_H

#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

namespace llvm {
class MCAssembler;
class MCContext;
class MCFixup;
class MCObjectWriter;
class MCSymbol;
class MCSymbolRepo;
class MCValue;

struct RepoRelocationEntry {
  uint64_t Offset;            // Where is the relocation.
  const MCSymbolRepo *Symbol; // The symbol to relocate with.
  unsigned Type;              // The type of the relocation.
  uint64_t Addend;            // The addend to use.
  const MCSymbolRepo
      *OriginalSymbol;     // The original value of Symbol if we changed it.
  uint64_t OriginalAddend; // The original value of addend.
  StringRef UsedSymbolName; // The Symbol name is used in the relocations.

  RepoRelocationEntry(uint64_t Offset, const MCSymbolRepo *Symbol,
                      unsigned Type, uint64_t Addend,
                      const MCSymbolRepo *OriginalSymbol,
                      uint64_t OriginalAddend, const StringRef UsedSymbolName)
      : Offset(Offset), Symbol(Symbol), Type(Type), Addend(Addend),
        OriginalSymbol(OriginalSymbol), OriginalAddend(OriginalAddend),
        UsedSymbolName(UsedSymbolName) {}

  void print(raw_ostream &Out) const {
    Out << "Off=" << Offset << ", Sym=" << Symbol << ", Type=" << Type
        << ", Addend=" << Addend << ", OriginalSymbol=" << OriginalSymbol
        << ", OriginalAddend=" << OriginalAddend
        << ", UsedSymbolName=" << UsedSymbolName;
  }

  void dump() const { print(errs()); }
};

class MCRepoObjectTargetWriter : public MCObjectTargetWriter {

protected:
  MCRepoObjectTargetWriter();

public:
  virtual ~MCRepoObjectTargetWriter() = default;

  Triple::ObjectFormatType getFormat() const override { return Triple::Repo; }
  static bool classof(const MCObjectTargetWriter *W) {
	  return W->getFormat() == Triple::Repo;
  }

  virtual unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                                const MCFixup &Fixup, bool IsPCRel) const = 0;

  virtual bool needsRelocateWithSymbol(const MCSymbol &Sym,
                                       unsigned Type) const;

  virtual void sortRelocs(const MCAssembler &Asm,
                          std::vector<RepoRelocationEntry> &Relocs);
};

/// \brief Construct a new Repository writer instance.
///
/// \param MOTW - The target specific writer subclass.
/// \param OS - The stream to write to.
/// \returns The constructed object writer.
std::unique_ptr<MCObjectWriter>
createRepoObjectWriter(std::unique_ptr<MCRepoObjectTargetWriter> MOTW,
                       raw_pwrite_stream &OS, bool IsLittleEndian);
} // namespace llvm

#endif // LLVM_MC_MCREPOOBJECTWRITER_H
