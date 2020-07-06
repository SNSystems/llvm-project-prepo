//*  ___    _            _   _  __       ____                *
//* |_ _|__| | ___ _ __ | |_(_)/ _|_   _|  _ \ __ _ ___ ___  *
//*  | |/ _` |/ _ \ '_ \| __| | |_| | | | |_) / _` / __/ __| *
//*  | | (_| |  __/ | | | |_| |  _| |_| |  __/ (_| \__ \__ \ *
//* |___\__,_|\___|_| |_|\__|_|_|  \__, |_|   \__,_|___/___/ *
//*                                |___/                     *
//===- lib/IdentifyPass.cpp -----------------------------------------------===//
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

auto rld::identifyPass(rld::Context &Ctxt, llvm::ThreadPool &IdentifyPool,
                       const rld::CompilationIndexPtr &CompilationIndex,
                       const llvm::ArrayRef<std::string> &InputPaths)
    -> llvm::ErrorOr<IdentifyResult> {

  llvm::NamedRegionTimer ScanTimer("Identify", "Identify pass", TimerGroupName,
                                   TimerGroupDescription);

  ArchiveSymbolMap ArchSymbols;
  Identifier::CompilationVector Compilations;
  std::atomic<bool> Okay{true};
  std::atomic<size_t> MaxArchiveMembers{0};

  auto InputOrdinal = uint32_t{0};
  rld::Identifier Ident{Ctxt.Db, CompilationIndex, Ctxt.IOMut};
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
  llvmDebug(DebugType, Ctxt.IOMut,
            [&]() { dump(Ctxt.Db, llvm::dbgs(), R, MaxArchiveMembers); });
  return R;
}
