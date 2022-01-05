//===- include/rld/ELFStringTable.h -----------------------*- mode: C++ -*-===//
//*  _____ _     _____ ____  _        _               _____     _     _       *
//* | ____| |   |  ___/ ___|| |_ _ __(_)_ __   __ _  |_   _|_ _| |__ | | ___  *
//* |  _| | |   | |_  \___ \| __| '__| | '_ \ / _` |   | |/ _` | '_ \| |/ _ \ *
//* | |___| |___|  _|  ___) | |_| |  | | | | | (_| |   | | (_| | |_) | |  __/ *
//* |_____|_____|_|   |____/ \__|_|  |_|_| |_|\__, |   |_|\__,_|_.__/|_|\___| *
//*                                           |___/                           *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ELFSTRINGTABLE_H
#define RLD_ELFSTRINGTABLE_H

#include "rld/symbol.h"

#include <cstdint>

namespace rld {

class Layout;
struct SymbolOrder;

namespace elf {

uint64_t prepareStringTable(Layout *const Lout, const Context &Ctxt,
                            const GlobalSymbolsContainer &Globals);

uint8_t *writeStrings(uint8_t *StringOut, const Context &Context,
                      const SymbolOrder &SymOrder,
                      const UndefsContainer &Undefs);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELF_STRING_TABLE_H
