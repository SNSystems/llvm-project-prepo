//===- include/rld/Scanner.h ------------------------------*- mode: C++ -*-===//
//*  ____                                   *
//* / ___|  ___ __ _ _ __  _ __   ___ _ __  *
//* \___ \ / __/ _` | '_ \| '_ \ / _ \ '__| *
//*  ___) | (_| (_| | | | | | | |  __/ |    *
//* |____/ \___\__,_|_| |_|_| |_|\___|_|    *
//*                                         *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_SCANNER_H
#define RLD_SCANNER_H

#include "rld/Context.h"
#include "rld/Symbol.h"
#include "rld/XfxScanner.h"

#include "pstore/core/address.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"

#include <atomic>
#include <memory>

namespace rld {

class LayoutBuilder;
class GroupSet;

class Scanner {
public:
  Scanner(Context &Ctx, LayoutBuilder &Layout,
          NotNull<UndefsContainer *> const Undefs)
      : Context_{Ctx}, Layout_{Layout}, Undefs_{Undefs} {}
  Scanner(Scanner const &) = delete;
  Scanner &operator=(Scanner const &) = delete;

  bool run(const llvm::StringRef &Path,
           const NotNull<GlobalSymbolsContainer *> GlobalSymbols,
           const NotNull<FixupStorage::Container *> FixupStorage,
           const NotNull<GroupSet *> NextGroup,
           const pstore::extent<pstore::repo::compilation> &CompilationExtent,
           uint32_t InputOrdinal);

private:
  Context &Context_;
  LayoutBuilder &Layout_;
  NotNull<UndefsContainer *> const Undefs_;
};

} // namespace rld

#endif // RLD_SCANNER_H
