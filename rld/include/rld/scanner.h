//*                                       *
//*  ___  ___ __ _ _ __  _ __   ___ _ __  *
//* / __|/ __/ _` | '_ \| '_ \ / _ \ '__| *
//* \__ \ (_| (_| | | | | | | |  __/ |    *
//* |___/\___\__,_|_| |_|_| |_|\___|_|    *
//*                                       *
//===- include/rld/scanner.h ----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_SCANNER_H
#define RLD_SCANNER_H

#include "pstore/core/address.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"

#include "rld/context.h"
#include "rld/symbol.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"

#include <atomic>
#include <memory>

namespace rld {

class LayoutBuilder;

class Scanner {
public:
  Scanner(Context &Ctx, LayoutBuilder &Layout,
          NotNull<UndefsContainer *> const Undefs)
      : Context_{Ctx}, Layout_{Layout}, Undefs_{Undefs} {}
  Scanner(Scanner const &) = delete;
  Scanner &operator=(Scanner const &) = delete;

  void run(std::string const &Path,
           NotNull<rld::GlobalSymbolsContainer *> const GlobalSymbols,
           pstore::extent<pstore::repo::compilation> const &CompilationExtent,
           uint32_t InputOrdinal);

private:
  Context &Context_;
  LayoutBuilder &Layout_;
  NotNull<UndefsContainer *> const Undefs_;
};

} // namespace rld

#endif // RLD_SCANNER_H
