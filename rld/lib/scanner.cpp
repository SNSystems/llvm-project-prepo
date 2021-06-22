//===- lib/scanner.cpp ----------------------------------------------------===//
//*                                       *
//*  ___  ___ __ _ _ __  _ __   ___ _ __  *
//* / __|/ __/ _` | '_ \| '_ \ / _ \ '__| *
//* \__ \ (_| (_| | | | | | | |  __/ |    *
//* |___/\___\__,_|_| |_|_| |_|\___|_|    *
//*                                       *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
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

// get thread storage
// ~~~~~~~~~~~~~~~~~~
auto XfxStorage::getThreadStorage() -> NotNull<StorageType *> {
  static thread_local char tls = 0;
  auto *const ptr = &tls;
  const std::lock_guard<std::mutex> _{Mut_};
  return &S_.try_emplace(ptr).first->second;
}

// run
// ~~~
void Scanner::run(
    const llvm::StringRef &Path,
    const NotNull<rld::GlobalSymbolsContainer *> GlobalSymbols,
    const NotNull<XfxStorage::StorageType *> ResolvedXfxs,
    const pstore::extent<pstore::repo::compilation> &CompilationExtent,
    uint32_t InputOrdinal) {

  llvmDebug(DebugType, Context_.IOMut, [&]() {
    llvm::dbgs() << "Scanning ticket file \"" << Path << "\" (index "
                 << InputOrdinal << ")\n";
  });

  auto const &Compilation = Context_.recordCompilation(CompilationExtent);

  llvm::ErrorOr<llvm::Triple> const EOT = Context_.mergeTriple(Compilation);
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
      GlobalSymbols, Undefs_, Compilation, InputOrdinal, ErrorFn);

  bool Error = !Locals.hasValue();
  if (Error) {
    // FIXME: handle the error somehow
  }

  assert(Locals->size() == Compilation.size());
  LocalPLTsContainer PLTSymbols =
      resolveXfixups(Context_, *Locals, GlobalSymbols, Undefs_,
                     ResolvedXfxs.get(), InputOrdinal);

  // Notify layout that we've completed work on the item at 'index' and tell it
  // about its definitions.
  auto &&L = Locals.getValue();
  Layout_.visited(InputOrdinal,
                  std::make_tuple(std::move(L), std::move(PLTSymbols)));
}

} // namespace rld
