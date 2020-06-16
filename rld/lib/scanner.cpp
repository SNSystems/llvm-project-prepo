//*                                       *
//*  ___  ___ __ _ _ __  _ __   ___ _ __  *
//* / __|/ __/ _` | '_ \| '_ \ / _ \ '__| *
//* \__ \ (_| (_| | | | | | | |  __/ |    *
//* |___/\___\__,_|_| |_|_| |_|\___|_|    *
//*                                       *
//===- lib/scanner.cpp ----------------------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
//===----------------------------------------------------------------------===//
#include "rld/scanner.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/Magic.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/Archive.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"

#include "rld/LayoutBuilder.h"
#include "rld/XfxScanner.h"

namespace {

constexpr auto DebugType = "rld-scanner";

#if 0
    llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                  pstore::index::digest const &Digest) {
      return OS << Digest.to_hex_string();
    }
#endif

} // end anonymous namespace


namespace rld {

// run
// ~~~
void Scanner::run(
    std::string const &Path,
    NotNull<rld::GlobalSymbolsContainer *> const GlobalSymbols,
    pstore::extent<pstore::repo::compilation> const &CompilationExtent,
    uint32_t InputOrdinal) {

  llvmDebug(DebugType, Context_.IOMut, [&]() {
    llvm::dbgs() << "Scanning ticket file \"" << Path << "\" (index "
                 << InputOrdinal << ")\n";
  });

  auto const Compilation =
      pstore::repo::compilation::load(Context_.Db, CompilationExtent);

  llvm::ErrorOr<llvm::Triple> const EOT = Context_.mergeTriple(*Compilation);
  if (!EOT) {
    // FIXME: handle the error somehow
  }

  auto const ErrorFn = [this, &Path](StringAddress Name) {
    std::lock_guard<decltype(Context_.IOMut)> const Lock{Context_.IOMut};
    llvm::errs() << "Error: cannot define symbol ("
                 << loadStdString(Context_.Db, Name) << ") in \"" << Path
                 << "\"\n";
  };

  SymbolResolver Resolver{Context_};
  llvm::Optional<LocalSymbolsContainer> Locals = Resolver.defineSymbols(
      GlobalSymbols, Undefs_, *Compilation, InputOrdinal, ErrorFn);

  bool Error = !Locals.hasValue();
  if (!Error) {
    assert(Locals->size() == Compilation->size());
    //    XfxScanner FixupScanner{Context_, Undefs_};
    Error =
        resolveXfixups(Context_, *Locals, GlobalSymbols, Undefs_, InputOrdinal);
  }

  if (Error) {
    // FIXME: handle the error somehow
  }

  // Notify layout that we've completed work on the item at 'index' and tell it
  // about its definitions.
  auto &&L = Locals.getValue();
  Layout_.visited(InputOrdinal, std::move(L));
}

} // namespace rld
