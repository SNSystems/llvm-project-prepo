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

#include "rld/LayoutBuilder.h"
#include "rld/symbol.h"

namespace rld {

bool resolveXfixups(Context &Context, LocalSymbolsContainer const &Locals,
                    NotNull<rld::GlobalSymbolsContainer *> const Globals,
                    NotNull<UndefsContainer *> const Undefs,
                    uint32_t InputOrdinal);

} // end namespace rld

#endif // RLD_XFX_SCANNER_H
