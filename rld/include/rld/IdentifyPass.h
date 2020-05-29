#ifndef RLD_IDENTIFYPASS_H
#define RLD_IDENTIFYPASS_H

#include "rld/Identify.h"
#include "rld/context.h"
#include "rld/types.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorOr.h"

#include <string>

namespace llvm {

class ThreadPool;

} // end namespace llvm

namespace rld {

struct IdentifyResult {
  IdentifyResult(ArchiveSymbolMap &&AS,
                 Identifier::CompilationVector &&C)
      : ArchSymbols{std::move(AS)}, Compilations{std::move(C)} {}

  ArchiveSymbolMap ArchSymbols;
  Identifier::CompilationVector Compilations;
};

llvm::ErrorOr<IdentifyResult>
identifyPass(Context &Ctxt, llvm::ThreadPool &IdentifyPool,
             const CompilationIndexPtr &CompilationIndex,
             const llvm::ArrayRef<std::string> &InputPaths,
             unsigned NumWorkers);

} // end namespace rld

#endif // RLD_IDENTIFYPASS_H
