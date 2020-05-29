#include "rld/IdentifyPass.h"
#include "rld/Identify.h"

#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/raw_ostream.h"
#include <atomic>

namespace {
const char *DebugType = "rld-Identify";
} // end anonymous namespace

template <typename T>
static void updateMaximum(std::atomic<T> &MaxValue, const T &Value) {
  T PrevMax = MaxValue;
  while (PrevMax < Value && !MaxValue.compare_exchange_weak(PrevMax, Value)) {
  }
}

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
                       const llvm::ArrayRef<std::string> &InputPaths,
                       unsigned NumWorkers) -> llvm::ErrorOr<IdentifyResult> {
  ArchiveSymbolMap ArchSymbols;
  Identifier::CompilationVector Compilations;
  std::atomic<bool> Okay{true};
  std::atomic<size_t> MaxArchiveMembers{0};

  auto InputOrdinal = std::size_t{0};
  rld::Identifier Ident{Ctxt.Db, CompilationIndex, Ctxt.IOMut};
  for (auto const &InputPath : InputPaths) {
    IdentifyPool.async(
        [&](std::string const &Path, unsigned Ordinal) {
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
