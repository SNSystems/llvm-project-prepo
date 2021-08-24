//===- include/rld/ELFSymbolTable.h -----------------------*- mode: C++ -*-===//
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

constexpr uint64_t symbolValue(const Contribution &C) {
  return C.OScn->VirtualAddr + C.Offset;
}

constexpr uint64_t symbolValue(const Symbol &Sym) {
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
