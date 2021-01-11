#ifndef RLD_ELF_OUTPUT_H
#define RLD_ELF_OUTPUT_H

#include "llvm/Support/Error.h"
#include "llvm/Support/ThreadPool.h"

#include "rld/symbol.h"

namespace llvm {
class StringRef;
}

namespace rld {

class Context;
class Layout;

llvm::Error elfOutput(const llvm::StringRef &OutputFileName, Context &Ctxt,
                      const GlobalSymbolsContainer &Globals,
                      llvm::ThreadPool &WorkPool, Layout *const Lout);

} // end namespace rld

#endif // RLD_ELF_OUTPUT_H
