//===----    RepoMetadataGeneration.cpp - Create a program repository -----===//
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
#include "llvm/IR/RepoDefinition.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/InitializePasses.h"
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
STATISTIC(NumAliases, "Number of aliases hashed");

namespace {

/// RepoMetadataGeneration finds functions, global variables and calculate the
/// hash values.
class RepoMetadataGeneration : public ModulePass {
public:
  static char ID;
  RepoMetadataGeneration() : ModulePass(ID) {
    initializeRepoMetadataGenerationPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override {
    return "RepoMetadataGenerationPass";
  }

  bool runOnModule(Module &M) override;
};

} // end anonymous namespace

char RepoMetadataGeneration::ID = 0;
INITIALIZE_PASS(RepoMetadataGeneration, "prepo",
                "Generate Program Repository Definitions", false, false)

ModulePass *llvm::createRepoMetadataGenerationPass() {
  return new RepoMetadataGeneration();
}

bool RepoMetadataGeneration::runOnModule(Module &M) {
  assert(Triple(M.getTargetTriple()).isOSBinFormatRepo() &&
         "This pass should be only run on the Repo target");

  if (skipModule(M))
    return false;

  auto Result = repodefinition::generateRepoDefinitions(M);
  NumVariables += std::get<1>(Result);
  NumFunctions += std::get<2>(Result);

  // Enable the program repo support for alias.
  for (GlobalAlias &GA : M.aliases()) {
    if (auto *GO = GA.getBaseObject()) {
      LLVM_DEBUG(dbgs() << "GA: " << GA.getName()
                        << ", its base object: " << GO->getName() << '\n');
      RepoDefinition *RD = dyn_cast<RepoDefinition>(
          GO->getMetadata(LLVMContext::MD_repo_definition));
      assert(RD && "RD should not be NULL!");
      auto GARD =
          RepoDefinition::get(M.getContext(), GA.getName(), RD->getDigest(),
                              GA.getLinkage(), GA.getVisibility(), true);
      NamedMDNode *NMD = M.getOrInsertNamedMetadata("repo.definitions");
      assert(NMD && "NamedMDNode cannot be NULL!");
      NMD->addOperand(GARD);
      ++NumAliases;
    }
  }

  LLVM_DEBUG(dbgs() << "Size of module: " << M.size() << '\n');
  LLVM_DEBUG(dbgs() << "Number of hashed functions: " << NumFunctions << '\n');
  LLVM_DEBUG(dbgs() << "Number of hashed variables: " << NumVariables << '\n');
  LLVM_DEBUG(dbgs() << "Number of hashed aliases: " << NumAliases << '\n');

  return std::get<0>(Result);
}
