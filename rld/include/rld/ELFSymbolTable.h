//===- include/rld/ELFSymbolTable.h -----------------------*- mode: C++ -*-===//
//*  _____ _     _____ ____                  _           _  *
//* | ____| |   |  ___/ ___| _   _ _ __ ___ | |__   ___ | | *
//* |  _| | |   | |_  \___ \| | | | '_ ` _ \| '_ \ / _ \| | *
//* | |___| |___|  _|  ___) | |_| | | | | | | |_) | (_) | | *
//* |_____|_____|_|   |____/ \__, |_| |_| |_|_.__/ \___/|_| *
//*                          |___/                          *
//*  _____     _     _       *
//* |_   _|_ _| |__ | | ___  *
//*   | |/ _` | '_ \| |/ _ \ *
//*   | | (_| | |_) | |  __/ *
//*   |_|\__,_|_.__/|_|\___| *
//*                          *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RLD_ELFSYMBOLTABLE_H
#define RLD_ELFSYMBOLTABLE_H

#include "rld/LayoutBuilder.h"
#include "rld/symbol.h"

#include "llvm/Object/ELF.h"

namespace rld {
namespace elf {

inline uint64_t symbolValue(const Contribution &C) {
  return C.OScn->VirtualAddr + C.Offset;
}

inline uint64_t symbolValue(const Symbol &Sym) {
  const Contribution *const C = Sym.contribution();
  assert(C != nullptr);
  return symbolValue(*C);
}

template <typename ELFT>
uint64_t prepareSymbolTableSection(Layout *const Lout,
                                   const GlobalSymbolsContainer &Globals);

template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Sym *
writeSymbolTable(typename llvm::object::ELFFile<ELFT>::Elf_Sym *SymbolOut,
                 const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
                 const SectionIndexedArray<unsigned> &SectionToIndex);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFSYMBOLTABLE_H
