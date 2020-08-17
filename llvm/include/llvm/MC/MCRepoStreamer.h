//===- MCRepoStreamer.h - MCStreamer Repo Object File Interface -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCREPOSTREAMER_H
#define LLVM_MC_MCREPOSTREAMER_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCObjectStreamer.h"
#include "llvm/MC/SectionKind.h"
#include "llvm/Support/DataTypes.h"

namespace llvm {

class MCAsmBackend;
class MCCodeEmitter;
class MCExpr;
class MCInst;

class MCRepoStreamer : public MCObjectStreamer {
public:
  MCRepoStreamer(MCContext &Context, std::unique_ptr<MCAsmBackend> TAB,
                 std::unique_ptr<MCObjectWriter> OW,
                 std::unique_ptr<MCCodeEmitter> Emitter);

  ~MCRepoStreamer() override;

  /// state management
  void reset() override {}

  /// \name MCStreamer Interface
  /// @{

  void changeSection(MCSection *Section, const MCExpr *Subsection) override;
  bool emitSymbolAttribute(MCSymbol *Symbol, MCSymbolAttr Attribute) override;
  void emitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                        unsigned ByteAlignment) override;

  void emitZerofill(MCSection *Section, MCSymbol *Symbol = nullptr,
                    uint64_t Size = 0, unsigned ByteAlignment = 0,
                    SMLoc L = SMLoc()) override;

private:
  void emitInstToData(const MCInst &Inst, const MCSubtargetInfo &) override;
};

} // end namespace llvm

#endif
