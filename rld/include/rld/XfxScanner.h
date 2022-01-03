//===- include/rld/XfxScanner.h ---------------------------*- mode: C++ -*-===//
//* __  __ __        ____                                   *
//* \ \/ // _|_  __ / ___|  ___ __ _ _ __  _ __   ___ _ __  *
//*  \  /| |_\ \/ / \___ \ / __/ _` | '_ \| '_ \ / _ \ '__| *
//*  /  \|  _|>  <   ___) | (_| (_| | | | | | | |  __/ |    *
//* /_/\_\_| /_/\_\ |____/ \___\__,_|_| |_|_| |_|\___|_|    *
//*                                                         *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_XFX_SCANNER_H
#define RLD_XFX_SCANNER_H

#include "rld/CSAlloc.h"
#include "rld/LayoutBuilder.h"
#include "rld/symbol.h"

#include <cassert>

namespace rld {

class GroupSet;

/// \param Context  The rld context.
/// \param Locals  The definitions provided by a compilation after symbol
///   resolution.
/// \param Globals The global symbol table.
/// \param Storage  A container for the results of resolving external and
///   internal fixups.

GOTPLTContainer resolveFixups(Context &Context,
                              const NotNull<CompilationSymbolsView *> Locals,
                              const NotNull<GlobalSymbolsContainer *> Globals,
                              const NotNull<UndefsContainer *> Undefs,
                              uint32_t InputOrdinal,
                              const NotNull<FixupStorage::Container *> Storage,
                              const NotNull<GroupSet *> NextGroup);

} // end namespace rld

#endif // RLD_XFX_SCANNER_H
