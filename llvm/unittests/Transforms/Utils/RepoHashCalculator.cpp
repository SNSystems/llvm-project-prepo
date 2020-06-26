//===- RepoHashCalculator.cpp - Unit tests for RepoHashCalculator ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoDefinition.h"
#include "gtest/gtest.h"

using namespace llvm;

/// Generates a simple test function.
struct TestFunction {
  Function *F;
  BasicBlock *BB;
  Constant *C;
  Instruction *I;
  Type *T;

  TestFunction(LLVMContext &Ctx, Module &M, int addVal) {
    IRBuilder<> B(Ctx);
    T = B.getInt8Ty();
    F = Function::Create(FunctionType::get(T, {B.getInt8PtrTy()}, false),
                         GlobalValue::ExternalLinkage, "F", &M);
    BB = BasicBlock::Create(Ctx, "", F);
    B.SetInsertPoint(BB);
    Argument *PointerArg = &*F->arg_begin();
    LoadInst *LoadInst = B.CreateLoad(PointerArg);
    C = B.getInt8(addVal);
    I = cast<Instruction>(B.CreateAdd(LoadInst, C));
    B.CreateRet(I);
  }
};

/// A class for testing the RepoHashCalculator API.
///
/// The main purpose is to test if the required protected functions are
/// accessible from a derived class of FunctionHashCalculator.
class TestHash : public FunctionHashCalculator {
public:
  TestHash(const Function *F1) : FunctionHashCalculator(F1) {}

  bool testFunctionAccess(const Function *F1) { return F1 == function(); }

  repodefinition::DigestType getHash() { return getHashResult(); }

  repodefinition::DigestType testCalculate() {
    calculateHash();
    return getHash();
  }

  repodefinition::DigestType testHashSignature() {
    hasher().beginCalculate(*function()->getParent());
    hashSignature(function());
    return getHash();
  }

  repodefinition::DigestType testHashBasicBlock(const BasicBlock *BB) {
    hasher().beginCalculate(*function()->getParent());
    DIFileRecord DIFMap;
    hashBasicBlock(BB, DIFMap);
    return getHash();
  }

  repodefinition::DigestType testHashConstant(const Constant *V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashConstant(V);
    return getHash();
  }

  repodefinition::DigestType testHashGlobalValue(const GlobalValue *V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashGlobalValue(V);
    return getHash();
  }

  repodefinition::DigestType testHashValue(const Value *V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashValue(V);
    return getHash();
  }

  repodefinition::DigestType testHashInstruction(const Instruction *V) {
    hasher().beginCalculate(*function()->getParent());
    DIFileRecord DIFMap;
    hashInstruction(V, DIFMap);
    return getHash();
  }

  repodefinition::DigestType testHashType(Type *Ty) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashType(Ty);
    return getHash();
  }

  repodefinition::DigestType testHashNumber(uint64_t V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashNumber(V);
    return getHash();
  }

  repodefinition::DigestType testHashAPInt(const APInt &V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashAPInt(V);
    return getHash();
  }

  repodefinition::DigestType testHashAPFloat(const APFloat &V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashAPFloat(V);
    return getHash();
  }

  repodefinition::DigestType testHashMem(StringRef V) {
    hasher().beginCalculate(*function()->getParent());
    hasher().hashMem(V);
    return getHash();
  }
};

/// A test for the FunctionHashCalculator.
TEST(HashCalculatorTest, TestFunction) {
  LLVMContext C;
  Module M("test", C);
  MD5::MD5Result Hash = {};
  M.setModuleHash(Hash);
  TestFunction F1(C, M, 27);
  TestFunction F2(C, M, 28);
  TestFunction F3(C, M, 27);

  TestHash F1H(F1.F);
  TestHash F2H(F2.F);
  TestHash F3H(F3.F);

  EXPECT_TRUE(F1H.testFunctionAccess(F1.F));
  EXPECT_NE(F1H.testCalculate(), F2H.testCalculate());
  EXPECT_EQ(F1H.testCalculate(), F3H.testCalculate());
  EXPECT_EQ(F1H.testHashSignature(), F2H.testHashSignature());
  EXPECT_NE(F1H.testHashBasicBlock(F1.BB), F2H.testHashBasicBlock(F2.BB));
  EXPECT_EQ(F1H.testHashBasicBlock(F1.BB), F3H.testHashBasicBlock(F3.BB));
  EXPECT_NE(F1H.testHashConstant(F1.C), F2H.testHashConstant(F2.C));

  EXPECT_NE(F1H.testHashGlobalValue(F1.F), F2H.testHashGlobalValue(F2.F));
  EXPECT_NE(F1H.testHashInstruction(F1.I), F2H.testHashInstruction(F2.I));
  EXPECT_EQ(F1H.testHashType(F1.T), F2H.testHashType(F2.T));

  EXPECT_EQ(F1H.testHashNumber(5), F2H.testHashNumber(5));
  EXPECT_NE(F1H.testHashNumber(5), F2H.testHashNumber(6));
  EXPECT_EQ(F1H.testHashMem("Hello World"), F2H.testHashMem("Hello World"));
  EXPECT_NE(F1H.testHashMem("Hello World"), F2H.testHashMem("Hello-World"));
  EXPECT_EQ(F1H.testHashAPInt(APInt(32, 2)), F2H.testHashAPInt(APInt(32, 2)));
  EXPECT_NE(F1H.testHashAPInt(APInt(32, 2)), F2H.testHashAPInt(APInt(32, 5)));
  EXPECT_EQ(F1H.testHashAPFloat(APFloat(2.0)),
            F2H.testHashAPFloat(APFloat(2.0)));
  EXPECT_NE(F1H.testHashAPFloat(APFloat(2.0)),
            F2H.testHashAPFloat(APFloat(5.0)));
}

/// Generates a simple test CallInst.
struct TestCallInvoke {
  Function *F;
  TestCallInvoke(LLVMContext &Ctx, Module &M, Type *ArgType) {
    FunctionType *FTy = FunctionType::get(Type::getVoidTy(Ctx), ArgType, false);
    F = Function::Create(FTy, Function::ExternalLinkage, "", &M);
  }
};

// A test for the CallInst with the same argument type and argument value.
TEST(HashCalculatorTest, CallInstWithSameTypeValue) {
  LLVMContext C;
  Module M("test", C);
  MD5::MD5Result Hash = {};
  M.setModuleHash(Hash);

  Type *Int8Ty = Type::getInt8Ty(C);
  const int Val = 20;
  TestCallInvoke F1(C, M, Int8Ty);

  Value *Const = ConstantInt::get(Int8Ty, Val);
  TestHash F1H(F1.F);
  std::unique_ptr<CallInst> Call1(CallInst::Create(F1.F, Const));
  std::unique_ptr<CallInst> Call2(CallInst::Create(F1.F, Const));
  EXPECT_EQ(F1H.testHashInstruction(Call1.get()),
            F1H.testHashInstruction(Call2.get()))
      << "Instructions of Call1 and Call2 should have the same hash since "
         "they have the same argument type and value";
}

// The CallInst with the same argument type and different argument value.
TEST(HashCalculatorTest, CallInstWithSameTypeDiffValue) {
  LLVMContext C;
  Module M("test", C);
  MD5::MD5Result Hash = {};
  M.setModuleHash(Hash);

  Type *Int8Ty = Type::getInt8Ty(C);
  const int Val1 = 20;
  const int Val2 = 30;
  TestCallInvoke F1(C, M, Int8Ty);
  TestHash F1H(F1.F);
  Value *Const1 = ConstantInt::get(Int8Ty, Val1);
  Value *Const2 = ConstantInt::get(Int8Ty, Val2);
  std::unique_ptr<CallInst> Call1(CallInst::Create(F1.F, Const1));
  std::unique_ptr<CallInst> Call2(CallInst::Create(F1.F, Const2));
  EXPECT_NE(F1H.testHashInstruction(Call1.get()),
            F1H.testHashInstruction(Call2.get()))
      << "Instructions of Call1 and Call2 should have the different hash "
         "since they have the different argument value.";
}

// The CallInst with the different argument type and same argument value.
TEST(HashCalculatorTest, CallInstWithDiffTypeSameValue) {
  LLVMContext C;
  Module M("test", C);
  MD5::MD5Result Hash = {};
  M.setModuleHash(Hash);

  Type *Int8Ty = Type::getInt8Ty(C);
  Type *Int32Ty = Type::getInt32Ty(C);
  TestCallInvoke F1(C, M, Int8Ty);
  TestCallInvoke F2(C, M, Int32Ty);

  TestHash F1H(F1.F);
  const int Val = 20;
  Value *Const1 = ConstantInt::get(Int8Ty, Val);
  Value *Const2 = ConstantInt::get(Int32Ty, Val);
  std::unique_ptr<CallInst> Call1(CallInst::Create(F1.F, Const1));
  std::unique_ptr<CallInst> Call2(CallInst::Create(F2.F, Const2));
  EXPECT_NE(F1H.testHashInstruction(Call1.get()),
            F1H.testHashInstruction(Call2.get()))
      << "Instructions of Call1 and Call2 should have the different hash "
         "since they have the different argument value.";
}

class VariableHash : public testing::Test {
protected:
  VariableHash() {
    M0.reset(new Module("Module", Ctx));
    M1.reset(new Module("Module1", Ctx));
    MD5::MD5Result Hash = {};
    M0->setModuleHash(Hash);
    M1->setModuleHash(Hash);
    GV0 = new GlobalVariable(*M0, Type::getInt8Ty(Ctx), true,
                             GlobalValue::ExternalLinkage, nullptr, "GV0");
    GV1 = new GlobalVariable(*M1, Type::getInt8Ty(Ctx), true,
                             GlobalValue::ExternalLinkage, nullptr, "GV1");
  }

  ~VariableHash() {}

  bool isEqualHash(GlobalVariable *G0, GlobalVariable *G1) {
    VariableHashCalculator GVH0{G0}, GVH1{G1};
    GVH0.calculateHash();
    GVH1.calculateHash();
    return GVH0.getHashResult() == GVH1.getHashResult();
  }

  bool isEqualHash() { return isEqualHash(GV0, GV1); }

  LLVMContext Ctx;
  std::unique_ptr<Module> M0;
  std::unique_ptr<Module> M1;
  GlobalVariable *GV0;
  GlobalVariable *GV1;
};

// The module hash will affect the hash value.
TEST_F(VariableHash, SameModuleHash) {
  MD5 Hash;
  Hash.update("foo");
  MD5::MD5Result Result;
  Hash.final(Result);
  M0->setModuleHash(Result);
  M1->setModuleHash(Result);
  EXPECT_TRUE(isEqualHash());
}

TEST_F(VariableHash, DiffModuleHash) {
  MD5 Hash;
  Hash.update("foo");
  MD5::MD5Result Result;
  Hash.final(Result);
  M0->setModuleHash(Result);
  Hash.update("bar");
  Hash.final(Result);
  M1->setModuleHash(Result);
  EXPECT_FALSE(isEqualHash());
}

// The source filename will not affect the hash value.
TEST_F(VariableHash, CheckSourceFile) {
  M0->setSourceFileName("file.cpp");
  M1->setSourceFileName("file1.cpp");
  EXPECT_TRUE(isEqualHash());
}

// The name and linkage type will affect the hash value.
TEST_F(VariableHash, CheckLinkage) {
  GV0->setLinkage(GlobalValue::ExternalLinkage);
  GV1->setLinkage(GlobalValue::InternalLinkage);
  EXPECT_TRUE(isEqualHash());
}

// The alignment will affect the hash value.
TEST_F(VariableHash, CheckAlignment) {
  GV0->setAlignment(MaybeAlign(1));
  GV1->setAlignment(MaybeAlign(4));
  EXPECT_FALSE(isEqualHash());
}

// The constant type will affect the hash value.
TEST_F(VariableHash, CheckConstant) {
  GV0->setConstant(false);
  GV1->setConstant(true);
  EXPECT_FALSE(isEqualHash());
}

// The initializr will affect the hash value.
TEST_F(VariableHash, CheckInitializer) {
  Type *Int8 = Type::getInt8Ty(Ctx);

  Constant *Zero = ConstantInt::get(Int8, 0);
  GV0->setInitializer(Zero);
  EXPECT_TRUE(isEqualHash());

  GV1->setInitializer(Zero);
  EXPECT_TRUE(isEqualHash());

  Constant *One = ConstantInt::get(Int8, 1);
  GV1->setInitializer(One);
  EXPECT_FALSE(isEqualHash());
}

// The value type will affect the hash value.
TEST_F(VariableHash, CheckType) {
  GV1 = new GlobalVariable(*M1, Type::getInt32Ty(Ctx), true,
                           GlobalValue::ExternalLinkage, nullptr);
  EXPECT_FALSE(isEqualHash());
}

// int a = 1;
// int vp_a = &a;
// If 'a' hash value modifies, the 'vp_a' value will change as well.
TEST_F(VariableHash, ReferenceToGlobalVariable) {
  Type *Int8Ty = Type::getInt8Ty(Ctx);
  Constant *Zero = ConstantInt::get(Int8Ty, 0);
  // The name of global variable 'a' contributed to the vp_a hash calculation.
  GV0->setInitializer(Zero);
  Constant *PtrGV0 = ConstantExpr::getPtrToInt(GV0, Int8Ty);
  GlobalVariable *Ref_GV0 = new GlobalVariable(
      *M0, Int8Ty, false, GlobalValue::ExternalLinkage, PtrGV0);
  GV1->setInitializer(Zero);
  Constant *PtrGV1 = ConstantExpr::getPtrToInt(GV1, Int8Ty);
  GlobalVariable *Ref_GV1 = new GlobalVariable(
      *M1, Int8Ty, false, GlobalValue::ExternalLinkage, PtrGV1);
  EXPECT_TRUE(isEqualHash(GV0, GV1))
      << "GV0 and GV1 should have the same hash value.";
  EXPECT_FALSE(isEqualHash(Ref_GV0, Ref_GV1))
      << " Ref_GV0 and Ref_GV1 should have the different hash value since GV0 "
         "and GV1 have different name";

  // The hash of the global variable 'a' contributed to the vp_a hash
  // calculation.
  GlobalVariable *GV0_M1 =
      new GlobalVariable(*M1, Type::getInt8Ty(Ctx), true,
                         GlobalValue::ExternalLinkage, nullptr, "GV0");
  GV0_M1->setInitializer(Zero);
  EXPECT_TRUE(isEqualHash(GV0, GV0_M1))
      << "GV0 and GV0_M1 should have the same hash value.";
  Constant *PtrGV0_M1 = ConstantExpr::getPtrToInt(GV0_M1, Int8Ty);
  GlobalVariable *Ref_GV0_M1 = new GlobalVariable(
      *M1, Int8Ty, false, GlobalValue::ExternalLinkage, PtrGV0_M1);
  EXPECT_TRUE(isEqualHash(Ref_GV0, Ref_GV0_M1))
      << " Ref_GV0 and Ref_GV0_M1 should have same hashes since GV0 "
         "and GV0_M1 have same name and same hash value";

  Constant *One = ConstantInt::get(Int8Ty, 1);
  GV0_M1->setInitializer(One);
  EXPECT_FALSE(isEqualHash(GV0, GV0_M1))
      << "GV0 and GV0_M1 should have the different hash value.";
  EXPECT_FALSE(isEqualHash(Ref_GV0, Ref_GV0_M1))
      << " Ref_GV0 and Ref_GV0_M1 should have different hashes since GV0 "
         "and GV0_M1 have different hash value";
}
