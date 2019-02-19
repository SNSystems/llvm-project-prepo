//===- llvm/unittest/IR/RepoModuleHashTest.cpp - PassManager tests --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Transforms/IPO.h"
#include "gtest/gtest.h"

using namespace llvm;

namespace {

class TestModuleHash : public ::testing::Test {
protected:
  LLVMContext Context;
  SMDiagnostic Error;
  std::unique_ptr<Module> M0;
  std::unique_ptr<Module> M1;
  void calculateModeulHash() {
    legacy::PassManager PM;
    PM.add(createRepoMetadataGenerationPass());
    PM.run(*M0);
    PM.run(*M1);
  }

public:
  TestModuleHash()
      : M0(parseAssemblyString("define i32 @foo() { ret i32 1 }\n", Error,
                               Context)),
        M1(parseAssemblyString("define i32 @foo() { ret i32 1 }\n", Error,
                               Context)) {
    assert(M0 && M1 && "Failed to build the module!");
  }
};
} // namespace

TEST_F(TestModuleHash, SameTriple) {
  M0->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M1->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  calculateModeulHash();
  EXPECT_EQ(M0->getModuleHash().getValue().Bytes,
            M1->getModuleHash().getValue().Bytes);
}

TEST_F(TestModuleHash, DiffTriple) {
  M0->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M1->setTargetTriple("arm-unknown-linux-gnu-repo");
  calculateModeulHash();
  EXPECT_NE(M0->getModuleHash().getValue().Bytes,
            M1->getModuleHash().getValue().Bytes);
}

TEST_F(TestModuleHash, SameDataLayout) {
  M0->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M1->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M0->setDataLayout("e-n32");
  M1->setDataLayout("e-n32");
  calculateModeulHash();
  EXPECT_EQ(M0->getModuleHash().getValue().Bytes,
            M1->getModuleHash().getValue().Bytes);
}

TEST_F(TestModuleHash, DiffDataLayout) {
  M0->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M1->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M0->setDataLayout("e-n32");
  M1->setDataLayout("e");
  calculateModeulHash();
  EXPECT_NE(M0->getModuleHash().getValue().Bytes,
            M1->getModuleHash().getValue().Bytes);
}

TEST_F(TestModuleHash, DiffPasses) {
  M0->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  M1->setTargetTriple("x86_64-unknown-linux-gnu-repo");
  legacy::PassManager PM;
  PM.run(*M0);
  PM.add(createRepoMetadataGenerationPass());
  PM.run(*M1);
  EXPECT_NE(M0->getModuleHash().getValue().Bytes,
            M1->getModuleHash().getValue().Bytes);
}
// eof: unittests/IR/RepoModuleHashTest.cpp
