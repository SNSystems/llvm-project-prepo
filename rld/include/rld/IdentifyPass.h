//===- include/rld/IdentifyPass.h -------------------------*- mode: C++ -*-===//
//*  ___    _            _   _  __         ____                *
//* |_ _|__| | ___ _ __ | |_(_)/ _|_   _  |  _ \ __ _ ___ ___  *
//*  | |/ _` |/ _ \ '_ \| __| | |_| | | | | |_) / _` / __/ __| *
//*  | | (_| |  __/ | | | |_| |  _| |_| | |  __/ (_| \__ \__ \ *
//* |___\__,_|\___|_| |_|\__|_|_|  \__, | |_|   \__,_|___/___/ *
//*                                |___/                       *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_IDENTIFYPASS_H
#define RLD_IDENTIFYPASS_H

#include "rld/Identify.h"
#include "rld/context.h"
#include "rld/types.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorOr.h"

#include <string>

namespace llvm {

class ThreadPool;

} // end namespace llvm

namespace rld {

struct IdentifyResult {
  IdentifyResult(ArchiveSymbolMap &&AS,
                 Identifier::CompilationVector &&C)
      : ArchSymbols{std::move(AS)}, Compilations{std::move(C)} {}

  ArchiveSymbolMap ArchSymbols;
  Identifier::CompilationVector Compilations;
};

llvm::ErrorOr<IdentifyResult>
identifyPass(Context &Ctxt, llvm::ThreadPool &IdentifyPool,
             const CompilationIndexPtr &CompilationIndex,
             const llvm::ArrayRef<std::string> &InputPaths);

} // end namespace rld

#endif // RLD_IDENTIFYPASS_H
