//===- lib/Scanner.cpp ----------------------------------------------------===//
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
#include "rld/Scanner.h"

#include "rld/LayoutBuilder.h"
#include "rld/XfxScanner.h"

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"

#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Demangle/Demangle.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/Archive.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/raw_ostream.h"

namespace {

constexpr auto DebugType = "rld-scanner";

} // end anonymous namespace

static std::string timerName(const bool Enabled, const uint32_t InputOrdinal) {
  std::string TimerName;
  if (Enabled) {
    llvm::raw_string_ostream OS{TimerName};
    OS << "Resolution" << InputOrdinal;
    OS.flush();
  }
  return TimerName;
}

namespace rld {

// run
// ~~~
bool Scanner::run(
    const llvm::StringRef &Path,
    const NotNull<GlobalSymbolsContainer *> GlobalSymbols,
    const NotNull<FixupStorage::Container *> FixupStorage,
    const NotNull<GroupSet *> NextGroup,
    const pstore::extent<pstore::repo::compilation> &CompilationExtent,
    uint32_t InputOrdinal) {
  llvm::NamedRegionTimer _{timerName(Context_.TimersEnabled, InputOrdinal),
                           "Input file scanning", rld::TimerGroupName,
                           rld::TimerGroupDescription, Context_.TimersEnabled};

  llvmDebug(DebugType, Context_.IOMut, [&]() {
    llvm::dbgs() << "Scanning ticket file \"" << Path << "\" (index "
                 << InputOrdinal << ")\n";
  });

  auto const &Compilation = Context_.recordCompilation(CompilationExtent);

  llvm::ErrorOr<llvm::Triple> const EOT = Context_.mergeTriple(Compilation);
  if (!EOT) {
    // FIXME: handle the error somehow
  }

  auto const ErrorFn = [this, &Path](Symbol const *const Sym) {
    // TODO: make demangling optional.
    const std::string Name =
        llvm::demangle(loadStdString(Context_.Db, Sym->name()));
    {
      auto Def = Sym->definition();
      if (const auto &Bodies = std::get<const Symbol::OptionalBodies &>(Def)) {
        const std::string Previous =
            Context_.ordinalName((*Bodies)[0].inputOrdinal());
        std::lock_guard<decltype(Context_.IOMut)> const Lock{Context_.IOMut};
        llvm::errs() << "Error: Duplicate symbol '" << Name << "' in \"" << Path
                     << "\" previously defined by \"" << Previous << "\"\n";
        return;
      }
    }

    std::lock_guard<decltype(Context_.IOMut)> const Lock{Context_.IOMut};
    llvm::errs() << "Error: Duplicate symbol '" << Name << "' in \"" << Path
                 << "\"\n";
  };

  SymbolResolver Resolver{Context_};
  llvm::Optional<CompilationSymbolsView> Locals = Resolver.defineSymbols(
      GlobalSymbols, Undefs_, Compilation, InputOrdinal, ErrorFn);

  if (!Locals.hasValue()) {
    // There was an error found in symbol resolution.
    return false;
  }

  assert(Locals->Map.size() == Compilation.size());
  GOTPLTContainer PLTGOTSymbols =
      resolveFixups(Context_, &Locals.getValue(), GlobalSymbols, Undefs_,
                    InputOrdinal, FixupStorage.get(), NextGroup);

  // Notify layout that we've completed work on the item at 'index' and tell it
  // about its definitions.
  auto &&LocalsValue = Locals.getValue();
  Layout_.visited(InputOrdinal, std::make_tuple(std::move(LocalsValue),
                                                std::move(PLTGOTSymbols)));
  return true;
}

} // namespace rld
