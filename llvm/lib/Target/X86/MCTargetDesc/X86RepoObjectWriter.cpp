//===- X86RepoObjectWriter.cpp - X86 Program Repository Writer ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/X86FixupKinds.h"
#include "MCTargetDesc/X86MCTargetDesc.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCRepoObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {

class X86RepoObjectWriter : public MCRepoObjectTargetWriter {
public:
  X86RepoObjectWriter() = default;
  ~X86RepoObjectWriter() override = default;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};

} // end anonymous namespace

enum X86_64RelType { RT64_NONE, RT64_64, RT64_32, RT64_32S, RT64_16, RT64_8 };

static X86_64RelType getType64(MCFixupKind Kind,
                               MCSymbolRefExpr::VariantKind &Modifier,
                               bool &IsPCRel) {
  switch (unsigned(Kind)) {
  default:
    llvm_unreachable("Unimplemented");
  case FK_NONE:
    return RT64_NONE;
  case X86::reloc_global_offset_table8:
    Modifier = MCSymbolRefExpr::VK_GOT;
    IsPCRel = true;
    return RT64_64;
  case FK_Data_8:
    return RT64_64;
  case X86::reloc_signed_4byte:
  case X86::reloc_signed_4byte_relax:
    if (Modifier == MCSymbolRefExpr::VK_None && !IsPCRel)
      return RT64_32S;
    return RT64_32;
  case X86::reloc_global_offset_table:
    Modifier = MCSymbolRefExpr::VK_GOT;
    IsPCRel = true;
    return RT64_32;
  case FK_Data_4:
  case FK_PCRel_4:
  case X86::reloc_riprel_4byte:
  case X86::reloc_riprel_4byte_relax:
  case X86::reloc_riprel_4byte_relax_rex:
  case X86::reloc_riprel_4byte_movq_load:
    return RT64_32;
  case X86::reloc_branch_4byte_pcrel:
    Modifier = MCSymbolRefExpr::VK_PLT;
    return RT64_32;
  case FK_PCRel_2:
  case FK_Data_2:
    return RT64_16;
  case FK_PCRel_1:
  case FK_Data_1:
    return RT64_8;
  }
}

static void checkIs32(MCContext &Ctx, SMLoc Loc, X86_64RelType Type) {
  if (Type != RT64_32)
    Ctx.reportError(Loc,
                    "32 bit reloc applied to a field with a different size");
}

static unsigned getRelocType64(MCContext &Ctx, SMLoc Loc,
                               MCSymbolRefExpr::VariantKind Modifier,
                               X86_64RelType Type, bool IsPCRel,
                               MCFixupKind Kind) {
  switch (Modifier) {
  default:
    llvm_unreachable("Unimplemented");
  case MCSymbolRefExpr::VK_None:
  case MCSymbolRefExpr::VK_X86_ABS8:
    switch (Type) {
    case RT64_NONE:
      if (Modifier == MCSymbolRefExpr::VK_None)
        return ELF::R_X86_64_NONE;
      llvm_unreachable("Unimplemented");
    case RT64_64:
      return IsPCRel ? ELF::R_X86_64_PC64 : ELF::R_X86_64_64;
    case RT64_32:
      return IsPCRel ? ELF::R_X86_64_PC32 : ELF::R_X86_64_32;
    case RT64_32S:
      return ELF::R_X86_64_32S;
    case RT64_16:
      return IsPCRel ? ELF::R_X86_64_PC16 : ELF::R_X86_64_16;
    case RT64_8:
      return IsPCRel ? ELF::R_X86_64_PC8 : ELF::R_X86_64_8;
    }
    llvm_unreachable("unexpected relocation type!");
  case MCSymbolRefExpr::VK_GOT:
    switch (Type) {
    case RT64_64:
      return IsPCRel ? ELF::R_X86_64_GOTPC64 : ELF::R_X86_64_GOT64;
    case RT64_32:
      return IsPCRel ? ELF::R_X86_64_GOTPC32 : ELF::R_X86_64_GOT32;
    case RT64_32S:
    case RT64_16:
    case RT64_8:
    case RT64_NONE:
      llvm_unreachable("Unimplemented");
    }
    llvm_unreachable("unexpected relocation type!");
  case MCSymbolRefExpr::VK_GOTOFF:
    assert(Type == RT64_64);
    assert(!IsPCRel);
    return ELF::R_X86_64_GOTOFF64;
  case MCSymbolRefExpr::VK_TPOFF:
    assert(!IsPCRel);
    switch (Type) {
    case RT64_64:
      return ELF::R_X86_64_TPOFF64;
    case RT64_32:
      return ELF::R_X86_64_TPOFF32;
    case RT64_32S:
    case RT64_16:
    case RT64_8:
    case RT64_NONE:
      llvm_unreachable("Unimplemented");
    }
    llvm_unreachable("unexpected relocation type!");
  case MCSymbolRefExpr::VK_DTPOFF:
    assert(!IsPCRel);
    switch (Type) {
    case RT64_64:
      return ELF::R_X86_64_DTPOFF64;
    case RT64_32:
      return ELF::R_X86_64_DTPOFF32;
    case RT64_32S:
    case RT64_16:
    case RT64_8:
    case RT64_NONE:
      llvm_unreachable("Unimplemented");
    }
    llvm_unreachable("unexpected relocation type!");
  case MCSymbolRefExpr::VK_SIZE:
    assert(!IsPCRel);
    switch (Type) {
    case RT64_64:
      return ELF::R_X86_64_SIZE64;
    case RT64_32:
      return ELF::R_X86_64_SIZE32;
    case RT64_32S:
    case RT64_16:
    case RT64_8:
    case RT64_NONE:
      llvm_unreachable("Unimplemented");
    }
    llvm_unreachable("unexpected relocation type!");
  case MCSymbolRefExpr::VK_TLSCALL:
    return ELF::R_X86_64_TLSDESC_CALL;
  case MCSymbolRefExpr::VK_TLSDESC:
    return ELF::R_X86_64_GOTPC32_TLSDESC;
  case MCSymbolRefExpr::VK_TLSGD:
    checkIs32(Ctx, Loc, Type);
    return ELF::R_X86_64_TLSGD;
  case MCSymbolRefExpr::VK_GOTTPOFF:
    checkIs32(Ctx, Loc, Type);
    return ELF::R_X86_64_GOTTPOFF;
  case MCSymbolRefExpr::VK_TLSLD:
    checkIs32(Ctx, Loc, Type);
    return ELF::R_X86_64_TLSLD;
  case MCSymbolRefExpr::VK_PLT:
    checkIs32(Ctx, Loc, Type);
    return ELF::R_X86_64_PLT32;
  case MCSymbolRefExpr::VK_GOTPCREL:
    checkIs32(Ctx, Loc, Type);
    // Older versions of ld.bfd/ld.gold/lld
    // do not support GOTPCRELX/REX_GOTPCRELX,
    // and we want to keep back-compatibility.
    if (!Ctx.getAsmInfo()->canRelaxRelocations())
      return ELF::R_X86_64_GOTPCREL;
    switch (unsigned(Kind)) {
    default:
      return ELF::R_X86_64_GOTPCREL;
    case X86::reloc_riprel_4byte_relax:
      return ELF::R_X86_64_GOTPCRELX;
    case X86::reloc_riprel_4byte_relax_rex:
    case X86::reloc_riprel_4byte_movq_load:
      return ELF::R_X86_64_REX_GOTPCRELX;
    }
    llvm_unreachable("unexpected relocation type!");
  }
}

unsigned X86RepoObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;
  MCSymbolRefExpr::VariantKind Modifier = Target.getAccessVariant();
  X86_64RelType Type = getType64(Kind, Modifier, IsPCRel);
  return getRelocType64(Ctx, Fixup.getLoc(), Modifier, Type, IsPCRel, Kind);
}

std::unique_ptr<MCObjectTargetWriter> llvm::createX86RepoObjectWriter() {
  return std::make_unique<X86RepoObjectWriter>();
}
