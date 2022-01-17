//===- include/rld/ElfOutput.h ----------------------------*- mode: C++ -*-===//
//*  _____ _  __    ___        _               _    *
//* | ____| |/ _|  / _ \ _   _| |_ _ __  _   _| |_  *
//* |  _| | | |_  | | | | | | | __| '_ \| | | | __| *
//* | |___| |  _| | |_| | |_| | |_| |_) | |_| | |_  *
//* |_____|_|_|    \___/ \__,_|\__| .__/ \__,_|\__| *
//*                               |_|               *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ELFOUTPUT_H
#define RLD_ELFOUTPUT_H

#include "rld/Symbol.h"

#include "llvm/Support/Error.h"
#include "llvm/Support/ThreadPool.h"

namespace llvm {
class StringRef;
}

namespace rld {

class Context;
class Layout;
class UndefsContainer;
struct SymbolOrder;

template <typename ELFT>
llvm::Error elfOutput(const llvm::StringRef &OutputFileName, Context &Ctxt,
                      const GlobalSymbolsContainer &Globals,
                      const SymbolOrder &SymOrder,
                      const UndefsContainer &Undefs, llvm::ThreadPool &WorkPool,
                      Layout *const Lout, const GOTPLTContainer &GOTPLTs);

} // end namespace rld

#endif // RLD_ELFOUTPUT_H
