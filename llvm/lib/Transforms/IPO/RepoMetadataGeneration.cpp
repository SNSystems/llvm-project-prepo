//===----    RepoMetadataGeneration.cpp - Create a program repository   -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/IR/RepoTicket.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Transforms/IPO.h"

using namespace llvm;

#define DEBUG_TYPE "prepo"

STATISTIC(NumFunctions, "Number of functions hashed");
STATISTIC(NumVariables, "Number of variables hashed");
#if 0
// TODO: enable the code once support alias.
STATISTIC(NumAliases, "Number of aliases hashed");
#endif

namespace {

/// RepoMetadataGeneration finds functions, global variables and calculate the
/// hash values.
class RepoMetadataGeneration : public ModulePass {
public:
  static char ID;
  RepoMetadataGeneration() : ModulePass(ID) {
    initializeRepoMetadataGenerationPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override { return "RepoMetadataGenerationPass"; }

  bool runOnModule(Module &M) override;

private:
  bool isObjFormatRepo(Module &M) const {
    return Triple(M.getTargetTriple()).isOSBinFormatRepo();
  }
};

} // end anonymous namespace

char RepoMetadataGeneration::ID = 0;
INITIALIZE_PASS(RepoMetadataGeneration, "prepo",
                "Generate Program Repository Tickets", false, false)

ModulePass *llvm::createRepoMetadataGenerationPass() {
  return new RepoMetadataGeneration();
}

bool RepoMetadataGeneration::runOnModule(Module &M) {
  if (skipModule(M) || !isObjFormatRepo(M))
    return false;

  auto Result = ticketmd::generateTicketMDs(M);
  NumVariables += std::get<1>(Result);
  NumFunctions += std::get<2>(Result);
  LLVM_DEBUG(dbgs() << "Size of module: " << M.size() << '\n');
  LLVM_DEBUG(dbgs() << "Number of hashed functions: " << NumFunctions << '\n');
  LLVM_DEBUG(dbgs() << "Number of hashed variables: " << NumVariables << '\n');

#if 0
  // TODO: enable the code once support alias.
  std::map<const GlobalValue *, ticketmd::DigestType> DigestMap;
  for (GlobalAlias &GA : M.aliases()) {
    auto GAAliasee = dyn_cast<GlobalValue>(ticketmd::getAliasee(&GA));
    auto GADigest = DigestMap[GAAliasee];
    DigestMap.emplace(&GA, GADigest);
    Changed = true;
    ++NumAliases;
  }
  DEBUG(dbgs() << "size of hashed aliases: " << NumAliases << '\n');
#endif
  return std::get<0>(Result);
}
