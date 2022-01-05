//===- include/rld/ELFProgramHeaders.h --------------------*- mode: C++ -*-===//
//*  _____ _     _____ ____                                       *
//* | ____| |   |  ___|  _ \ _ __ ___   __ _ _ __ __ _ _ __ ___   *
//* |  _| | |   | |_  | |_) | '__/ _ \ / _` | '__/ _` | '_ ` _ \  *
//* | |___| |___|  _| |  __/| | | (_) | (_| | | | (_| | | | | | | *
//* |_____|_____|_|   |_|   |_|  \___/ \__, |_|  \__,_|_| |_| |_| *
//*                                    |___/                      *
//*  _   _                _                *
//* | | | | ___  __ _  __| | ___ _ __ ___  *
//* | |_| |/ _ \/ _` |/ _` |/ _ \ '__/ __| *
//* |  _  |  __/ (_| | (_| |  __/ |  \__ \ *
//* |_| |_|\___|\__,_|\__,_|\___|_|  |___/ *
//*                                        *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ELFPROGRAMHEADERS_H
#define RLD_ELFPROGRAMHEADERS_H

#include "rld/LayoutBuilder.h"

#include "llvm/Object/ELF.h"

namespace rld {
namespace elf {

template <typename ELFT>
constexpr auto elfSegmentFlags(const SegmentKind Kind) ->
    typename llvm::object::ELFFile<ELFT>::Elf_Word {
  using Elf_Word = typename llvm::object::ELFFile<ELFT>::Elf_Word;
  switch (Kind) {
  case SegmentKind::data:
  case SegmentKind::gnu_relro:
  case SegmentKind::gnu_stack:
    return Elf_Word{llvm::ELF::PF_R | llvm::ELF::PF_W};

  case SegmentKind::phdr:
  case SegmentKind::rodata:
  case SegmentKind::tls:
    return Elf_Word{llvm::ELF::PF_R};

  case SegmentKind::text:
    return Elf_Word{llvm::ELF::PF_X | llvm::ELF::PF_R};

  case SegmentKind::discard:
  case SegmentKind::last:
    break;
  }
  llvm_unreachable("Invalid segment kind");
}

template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Phdr *emitProgramHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Phdr *Phdr, Context &Ctxt,
    const FileRegion &TargetDataRegion, const FileRegion &SegmentTableRegion,
    const Layout &Lout,
    const SegmentIndexedArray<llvm::Optional<int64_t>> &SegmentDataOffsets);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFPROGRAMHEADERS_H
