//===- lib/IdentifyPass.cpp -----------------------------------------------===//
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
#include "rld/IdentifyPass.h"

#include "rld/Algorithm.h"
#include "rld/Identify.h"

#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/raw_ostream.h"

#include <atomic>

namespace {

const char *DebugType = "rld-Identify";

} // end anonymous namespace

static void dump(pstore::database const &Db, llvm::raw_ostream &OS,
                 rld::IdentifyResult const &R, size_t MaxArchiveMembers) {
  constexpr auto Indent = "  ";
  OS << "Identify pass complete. MaxArchMembers=" << MaxArchiveMembers << '\n'
     << "Compilations:\n";

  for (auto &C : R.Compilations) {
    OS << Indent << std::get<0>(C) << " @" << std::get<2>(C) << '\n';
  }
  OS << "Archive symbols:\n";
  for (auto &KV : R.ArchSymbols) {
    auto const Ordinal = std::get<0>(KV.second.Ordinal) * MaxArchiveMembers +
                         std::get<1>(KV.second.Ordinal) + R.Compilations.size();
    OS << Indent << rld::loadStdString(Db, KV.first) << ": @" << Ordinal << "\n";
  }
  OS << '\n';
}

auto rld::identifyPass(rld::Context &Context, llvm::ThreadPool &IdentifyPool,
                       const rld::CompilationIndexPtr &CompilationIndex,
                       const llvm::ArrayRef<std::string> &InputPaths)
    -> llvm::ErrorOr<IdentifyResult> {

  llvm::NamedRegionTimer ScanTimer("Identify", "Identify pass", TimerGroupName,
                                   TimerGroupDescription,
                                   Context.TimersEnabled);

  ArchiveSymbolMap ArchSymbols;
  Identifier::CompilationVector Compilations;
  std::atomic<bool> Okay{true};
  std::atomic<size_t> MaxArchiveMembers{0};

  auto InputOrdinal = uint32_t{0};
  rld::Identifier Ident{Context.Db, CompilationIndex, Context.IOMut};
  for (auto const &InputPath : InputPaths) {
    IdentifyPool.async(
        [&](std::string const &Path, uint32_t Ordinal) {
          bool Success;
          size_t Members;
          std::tie(Success, Members) = Ident.add(Path, Ordinal);
          if (!Success) {
            Okay = false;
          }
          updateMaximum(MaxArchiveMembers, Members);
        },
        std::cref(InputPath), InputOrdinal++);
  }
  IdentifyPool.wait();
  if (!Okay) {
    // FIXME: this is utter nonsense. We didn't run out of memory.
    return make_error_code(std::errc::not_enough_memory);
  }

  auto R = IdentifyResult(std::move(Ident.getArchiveSymbols()),
                          std::move(Ident.getCompilationVector()));
  llvmDebug(DebugType, Context.IOMut,
            [&]() { dump(Context.Db, llvm::dbgs(), R, MaxArchiveMembers); });
  return R;
}
