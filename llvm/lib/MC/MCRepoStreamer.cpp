//===- lib/MC/MCRepoStreamer.cpp - Repo Object Output ------------------- -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file assembles .s files and emits ELF .o object files.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoStreamer.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSymbolRepo.h"
#include "llvm/Support/TargetRegistry.h"
#include <iostream>

using namespace llvm;

MCRepoStreamer::MCRepoStreamer(MCContext &Context,
                               std::unique_ptr<MCAsmBackend> TAB,
                               std::unique_ptr<MCObjectWriter> OW,
                               std::unique_ptr<MCCodeEmitter> Emitter)
    : MCObjectStreamer(Context, std::move(TAB), std::move(OW),
                       std::move(Emitter)) {}

MCRepoStreamer::~MCRepoStreamer() {}

void MCRepoStreamer::changeSection(MCSection *Section,
                                   const MCExpr *Subsection) {
  this->MCObjectStreamer::changeSection(Section, Subsection);
}

bool MCRepoStreamer::emitSymbolAttribute(MCSymbol *Sym,
                                         MCSymbolAttr Attribute) {
  MCSymbolRepo *Symbol = cast<MCSymbolRepo>(Sym);
  if (Attribute == MCSA_Weak || Attribute == MCSA_WeakReference)
    Symbol->setExternalWeak();
  return true; // success
}

void MCRepoStreamer::emitCommonSymbol(MCSymbol *Symbol, uint64_t Size,
                                      unsigned ByteAlignment) {
  report_fatal_error("Emit Common Symbol not yet implemented");
}

void MCRepoStreamer::emitZerofill(MCSection *Section, MCSymbol *Symbol,
                                  uint64_t Size, unsigned ByteAlignment,
                                  SMLoc Loc) {
  report_fatal_error("Emit Zero File not yet implemented");
}

void MCRepoStreamer::emitInstToData(const MCInst &Inst,
                                    const MCSubtargetInfo &STI) {
  MCDataFragment *const DF = this->getOrCreateDataFragment();

  SmallVector<MCFixup, 4> Fixups;
  SmallString<256> Code;
  raw_svector_ostream VecOS(Code);
  getAssembler().getEmitter().encodeInstruction(Inst, VecOS, Fixups, STI);

  // Add the fixups and data.
  for (MCFixup &Fixup : Fixups) {
    Fixup.setOffset(Fixup.getOffset() + DF->getContents().size());
    DF->getFixups().push_back(Fixup);
  }
  DF->getContents().append(Code.begin(), Code.end());
}

MCStreamer *llvm::createRepoStreamer(MCContext &Context,
                                     std::unique_ptr<MCAsmBackend> &&MAB,
                                     std::unique_ptr<MCObjectWriter> &&OW,
                                     std::unique_ptr<MCCodeEmitter> &&CE,
                                     bool RelaxAll) {
  MCRepoStreamer *S =
      new MCRepoStreamer(Context, std::move(MAB), std::move(OW), std::move(CE));

  if (RelaxAll)
    S->getAssembler().setRelaxAll(true);
  return S;
}
