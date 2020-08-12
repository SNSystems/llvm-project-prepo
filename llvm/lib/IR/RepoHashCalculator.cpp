//===- RepoHashCalculator.cpp - Implement Hash Calculation ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the FunctionHash and VariableHash Calculator which are
// used as 'definition' item by the RepoMetadataGeneration passes.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/RepoDefinition.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "repo-hash-calculator"

/// \brief Adds \param V to the hash.

void HashCalculator::hashMem(StringRef V) {
  Hash.update(ArrayRef<uint8_t>(HashKind::TAG_StringRef));
  hashNumber(V.size());
  Hash.update(V);
}

void HashCalculator::hashAPInt(const APInt &V) {
  Hash.update(HashKind::TAG_APInt);
  const uint64_t *Words = V.getRawData();
  for (unsigned I = 0, E = V.getNumWords(); I != E; ++I) {
    hashNumber(Words[I]);
  }
}

void HashCalculator::hashAPFloat(const APFloat &V) {
  Hash.update(HashKind::TAG_APFloat);
  // Floats are ordered first by semantics (i.e. float, double, half, etc.),
  // then by value interpreted as a bitstring (aka APInt).
  const fltSemantics &SV = V.getSemantics();
  hashNumber(APFloat::semanticsPrecision(SV));
  signed short SMax = APFloat::semanticsMaxExponent(SV);
  Hash.update(ArrayRef<uint8_t>((uint8_t *)&SMax, sizeof(signed short)));
  signed short SMin = APFloat::semanticsMinExponent(SV);
  Hash.update(ArrayRef<uint8_t>((uint8_t *)&SMin, sizeof(signed short)));
  hashNumber(APFloat::semanticsSizeInBits(SV));
  hashAPInt(V.bitcastToAPInt());
}

void HashCalculator::hashOrdering(AtomicOrdering V) {
  Hash.update(HashKind::TAG_AtomicOrdering);
  Hash.update(static_cast<uint8_t>(V));
}

void HashCalculator::hashAttribute(const Attribute &V) {
  if (V.isEnumAttribute()) {
    // Enum attribute uses the attribute kind to calculate the hash.
    Hash.update(HashKind::TAG_AttributeEnum);
    auto EnunKind = V.getKindAsEnum();
    Hash.update(
        ArrayRef<uint8_t>((uint8_t *)&EnunKind, sizeof(Attribute::AttrKind)));
  } else if (V.isIntAttribute()) {
    // Int attribute uses the attribute kind and int value to calculate the
    // hash.
    Hash.update(HashKind::TAG_AttributeInt);
    auto EnunKind = V.getKindAsEnum();
    Hash.update(
        ArrayRef<uint8_t>((uint8_t *)&EnunKind, sizeof(Attribute::AttrKind)));
    hashNumber(V.getValueAsInt());
  } else if (V.isStringAttribute()) {
    // String attribute uses the attribute kind and string value to calculate
    // the hash.
    Hash.update(HashKind::TAG_AttributeString);
    hashMem(V.getKindAsString());
    hashMem(V.getValueAsString());
  } else if (V.isTypeAttribute()) {
    // Type attribute uses the attribute kind and TypeID value to calculate the
    // hash.
    Hash.update(HashKind::TAG_AttributeType);
    auto EnunKind = V.getKindAsEnum();
    Hash.update(
        ArrayRef<uint8_t>((uint8_t *)&EnunKind, sizeof(Attribute::AttrKind)));
    hashNumber(V.getValueAsType()->getTypeID());
  } else {
    llvm_unreachable("Unrecognised Attribute type");
  }
}

void HashCalculator::hashAttributeList(const AttributeList &V) {
  Hash.update(HashKind::TAG_AttributeList);
  for (unsigned I = 0, E = V.getNumAttrSets(); I != E; ++I) {
    for (unsigned I = V.index_begin(), E = V.index_end(); I != E; ++I) {
      AttributeSet AS = V.getAttributes(I);
      for (AttributeSet::iterator VI = AS.begin(), VE = AS.end(); VI != VE;
           ++VI) {
        hashAttribute(*VI);
      }
    }
  }
}

void HashCalculator::hashInlineAsm(const InlineAsm *V) {
  Hash.update(HashKind::TAG_InlineAsm);
  hashType(V->getFunctionType());
  hashMem(V->getAsmString());
  hashMem(V->getConstraintString());
  Hash.update(HashKind::TAG_InlineAsm_SideEffects);
  Hash.update(V->hasSideEffects());
  Hash.update(HashKind::TAG_InlineAsm_AlignStack);
  Hash.update(V->isAlignStack());
  Hash.update(HashKind::TAG_InlineAsm_Dialect);
  Hash.update(V->getDialect());
}

void HashCalculator::hashRangeMetadata(const MDNode *V) {
  if (!V)
    return;
  Hash.update(HashKind::TAG_RangeMetadata);
  // Range metadata is a sequence of numbers.
  for (size_t I = 0, E = V->getNumOperands(); I < E; ++I) {
    ConstantInt *VLow = mdconst::extract<ConstantInt>(V->getOperand(I));
    hashAPInt(VLow->getValue());
  }
}

/// hashType - calculate a type hash.
void HashCalculator::hashType(Type *Ty) {
  Hash.update(HashKind::TAG_Type);
  Hash.update(Ty->getTypeID());

  DenseMap<const Type *, unsigned>::iterator TyItr = TypeNumbers.find(Ty);
  if (TyItr != TypeNumbers.end()) {
    hashNumber(TyItr->second);
    return;
  }
  TypeNumbers.insert(std::make_pair(Ty, TypeNumbers.size()));

  switch (Ty->getTypeID()) {
  // PrimitiveTypes
  case Type::VoidTyID:
  case Type::HalfTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
  case Type::LabelTyID:
  case Type::MetadataTyID:
  case Type::X86_MMXTyID:
  case Type::TokenTyID:
    break;

  // Derived types
  case Type::IntegerTyID:
    hashNumber(cast<IntegerType>(Ty)->getBitWidth());
    break;
  case Type::FunctionTyID: {
    FunctionType *FTy = cast<FunctionType>(Ty);
    for (Type *ParamTy : FTy->params()) {
      hashType(ParamTy);
    }
    Hash.update(FTy->isVarArg());
    hashType(FTy->getReturnType());
    break;
  }
  case Type::PointerTyID: {
    PointerType *PTy = dyn_cast<PointerType>(Ty);
    assert(PTy && "Ty type must be pointers here.");
    hashNumber(PTy->getAddressSpace());
    hashType(PTy->getElementType());
    break;
  }
  case Type::StructTyID: {
    StructType *STy = cast<StructType>(Ty);
    if (!STy->isLiteral() && !STy->getName().empty()) {
      hashMem(STy->getName());
    }
    for (Type *ElemTy : STy->elements()) {
      hashType(ElemTy);
    }
    if (STy->isPacked())
      Hash.update(STy->isPacked());
    break;
  }
  case Type::ArrayTyID:
  case Type::VectorTyID: {
    auto *STy = cast<SequentialType>(Ty);
    hashNumber(STy->getNumElements());
    hashType(STy->getElementType());
    break;
  }
  }
}

/// Accumulate the constants hash.
void HashCalculator::hashConstant(const Constant *V) {
  LLVM_DEBUG(dbgs() << "Constant V name:  " << V->getName() << "\n");

  Hash.update(HashKind::TAG_Constant);
  Type *Ty = V->getType();
  // Calculate type hash.
  hashType(Ty);

  auto GlobalValueV = dyn_cast<GlobalValue>(V);
  if (GlobalValueV) {
    hashGlobalValue(GlobalValueV);
    return;
  }

  unsigned int VID = V->getValueID();
  hashNumber(VID);

  if (const auto *SeqV = dyn_cast<ConstantDataSequential>(V)) {
    // This handles ConstantDataArray and ConstantDataVector.
    hashMem(SeqV->getRawDataValues());
    return;
  }

  switch (VID) {
  case Value::UndefValueVal:
  case Value::ConstantTokenNoneVal:
  case Value::ConstantAggregateZeroVal:
  case Value::ConstantPointerNullVal:
    break;
  case Value::ConstantIntVal: {
    hashAPInt(cast<ConstantInt>(V)->getValue());
    break;
  }
  case Value::ConstantFPVal: {
    hashAPFloat(cast<ConstantFP>(V)->getValueAPF());
    break;
  }
  case Value::ConstantArrayVal: {
    const ConstantArray *VA = cast<ConstantArray>(V);
    for (uint64_t I = 0, E = cast<ArrayType>(Ty)->getNumElements(); I < E;
         ++I) {
      hashConstant(cast<Constant>(VA->getOperand(I)));
    }
    break;
  }
  case Value::ConstantStructVal: {
    const ConstantStruct *VS = cast<ConstantStruct>(V);
    for (unsigned I = 0, E = cast<StructType>(Ty)->getNumElements(); I < E;
         ++I) {
      hashConstant(cast<Constant>(VS->getOperand(I)));
    }
    break;
  }
  case Value::ConstantVectorVal: {
    const ConstantVector *VV = cast<ConstantVector>(V);
    for (uint64_t I = 0, E = cast<VectorType>(Ty)->getNumElements(); I < E;
         ++I) {
      hashConstant(cast<Constant>(VV->getOperand(I)));
    }
    break;
  }
  case Value::ConstantExprVal: {
    const ConstantExpr *VE = cast<ConstantExpr>(V);
    for (unsigned I = 0, E = VE->getNumOperands(); I < E; ++I) {
      hashConstant(cast<Constant>(VE->getOperand(I)));
    }
    break;
  }
  case Value::BlockAddressVal: {
    const BlockAddress *BA = cast<BlockAddress>(V);
    hashValue(BA->getFunction());
    // hashValue will tell us if these are equivalent BasicBlocks, in the
    // context of their respective functions.
    hashValue(BA->getBasicBlock());
    break;
  }
  default: // Unknown constant, abort.
    LLVM_DEBUG(dbgs() << "Looking at valueID " << V->getValueID() << "\n");
    llvm_unreachable("Constant ValueID not recognized.");
  }
}

/// Calculate the value hash under pair-wise comparison. If this is the first
/// time the values are seen, they're added to the mapping so that we will
/// detect mismatches on next use. See comments in declaration for more details.
void HashCalculator::hashValue(const Value *V) {
  Hash.update(HashKind::TAG_Value);
  if (const Constant *ConstV = dyn_cast<Constant>(V)) {
    hashConstant(ConstV);
    return;
  }

  if (const InlineAsm *InlineAsmV = dyn_cast<InlineAsm>(V)) {
    hashInlineAsm(InlineAsmV);
    return;
  }

  auto *GV = dyn_cast<GlobalVariable>(V);
  if (!GV) {
    auto GA = dyn_cast<GlobalAlias>(V);
    if (GA)
      GV = dyn_cast<GlobalVariable>(GA->getAliasee()->stripPointerCasts());
  }
  if (GV && !GV->getName().empty()) {
    hashMem(GV->getName());
    return;
  }

  auto SN = SNMap.insert(std::make_pair(V, SNMap.size()));
  hashNumber(SN.first->second);
}

static bool shouldAddGODependency(const GlobalObject &GO) {
  // Don't push GO into the dependent list if it is a declaration.
  if (GO.isDeclaration())
    return false;

  // Don't push GO into the dependent list if function will not be inlined
  // and not be discarded if it is not used.
  const llvm::Function *const Fn = dyn_cast<const llvm::Function>(&GO);
  if (Fn && Fn->hasFnAttribute(Attribute::NoInline) &&
      !Fn->isDiscardableIfUnused())
    return false;

  return true;
}

void HashCalculator::hashGlobalValue(const GlobalValue *V) {
  hashMem(V->getName());
  DenseMap<const GlobalValue *, unsigned>::iterator GVI = GlobalNumbers.find(V);
  if (GVI != GlobalNumbers.end()) {
    hashNumber(GVI->second);
    return;
  }

  GlobalNumbers.insert(std::make_pair(V, GlobalNumbers.size()));

  if (auto *GV = dyn_cast<GlobalVariable>(V)) {
    // Accumulate the initial value of global variable .
    Hash.update(HashKind::TAG_GlobalVariable);
    if (GV->hasDefinitiveInitializer())
      hashConstant(GV->getInitializer());
  }

  if (auto *GA = dyn_cast<GlobalAlias>(V)) {
    if (auto GAV = dyn_cast<GlobalValue>(GA->getAliasee()->stripPointerCasts()))
      V = GAV;
  }

  if (auto *GO = dyn_cast<GlobalObject>(V)) {
    if (shouldAddGODependency(*GO)) {
      getDependencies().emplace_back(GO);
    }
  }
}

std::string &HashCalculator::get(MD5::MD5Result &HashRes) {
  SmallString<32> Result;
  MD5::stringifyResult(HashRes, Result);
  TheHash = Result.str();
  return TheHash;
}

void FunctionHashCalculator::hashSignature(const Function *F) {
  update(HashKind::TAG_Signature);
  // TODO: review all the attributes to find the stardard c++ attributes to
  // affect the generated codes.
  FnHash.hashAttributeList(F->getAttributes());
  if (F->hasGC()) {
    update(HashKind::TAG_Signature_GC);
    FnHash.hashMem(F->getGC());
  }
  if (F->hasSection()) {
    update(HashKind::TAG_Signature_Sec);
    FnHash.hashMem(F->getSection());
  }
  update(HashKind::TAG_Signature_VarArg);
  update(F->isVarArg());

  // Calling conventions may differ in where parameters, return values and
  // return addresses are placed (in registers, on the call stack, a mix of
  // both, or in other memory structures). If the function has input
  // paramaters, the generated codes will be different, the calling conventions
  // need to be consided in the hash calculation. If the function return type
  // is not void type, the generated code would be changed. Again, the calling
  // conventions need to be considered.
  if (F->getFunctionType()->getNumParams() != 0 ||
      F->getReturnType()->getTypeID() == Type::VoidTyID) {
    update(HashKind::TAG_Signature_CC);
    CallingConv::ID CC = F->getCallingConv();
    update(ArrayRef<uint8_t>((uint8_t *)&CC, sizeof(CallingConv::ID)));
  }

  FnHash.hashType(F->getFunctionType());
  // Visit the arguments so that they get enumerated in the order they're
  // passed in.
  update(HashKind::TAG_Signature_Arg);
  for (Function::const_arg_iterator ArgI = F->arg_begin(), ArgE = F->arg_end();
       ArgI != ArgE; ++ArgI) {
    FnHash.hashValue(&*ArgI);
  }
}

// Calculate either CallInst or InvokeInst instruction hash.
void FunctionHashCalculator::hashOperandBundles(const Instruction *V) {
  update(HashKind::TAG_OperandBundles);
  ImmutableCallSite VCS(V);
  assert(VCS && "Must not be empty!");
  assert((VCS.isCall() || VCS.isInvoke()) && "Must be calls or invokes!");

  for (unsigned i = 0, e = VCS.getNumOperandBundles(); i != e; ++i) {
    auto VOB = VCS.getOperandBundleAt(i);
    FnHash.hashMem(VOB.getTagName());
    // Since input values have been used to calculate the instruction hash for
    // all instructions, we only consider the input sizes here.
    FnHash.hashNumber(VOB.Inputs.size());
  }
}

// Update the Fnhash value by adding the DIFile's FileName or Directory.
static void hashDIFileString(HashCalculator &Hash, DIFileRecord &DIF,
                             StringRef Str) {
  bool Inserted;
  typename decltype(DIF.FileDirMap)::const_iterator StateIt;
  std::tie(StateIt, Inserted) =
      DIF.FileDirMap.try_emplace(Str, DIF.FileDirMap.size());
  if (!Inserted) {
    // If Dir is visited, use the letter 'R' as the marker and use its
    // state as the value.
    Hash.update(makeArrayRef('R'));
    Hash.hashNumber(StateIt->second);
    return;
  }
  Hash.update(makeArrayRef('T'));
  Hash.hashMem(StateIt->first);
}

void FunctionHashCalculator::hashDILocation(const DILocation *DL,
                                            DIFileRecord &DIF) {
  // Currently the abosolute line number is saved in the debug line section
  // and is stored in the repository. Therefore, the abosolute line number
  // is hashed.
  // TODO: Hash the relative line to the begining of this function.
  FnHash.hashNumber(DL->getLine());
  FnHash.hashNumber(DL->getColumn());
  // Hash the file name and directory, which contribute to the line number
  // program header for both inlined and non-inlined instructions.
  const DISubprogram *Scope = DL->getScope()->getSubprogram();
  const DIFile *File = Scope->getFile();
  if (File != DIF.CurDIFile) {
    // If the DIFile is changed, remember the current DIFile and hash the
    // filename and directory.
    DIF.CurDIFile = File;
    hashDIFileString(FnHash, DIF, Scope->getDirectory());
    hashDIFileString(FnHash, DIF, Scope->getFilename());
  }
  const DILocation *SiteLoc = DL->getInlinedAt();
  update(SiteLoc ? HashKind::TAG_DILocation_InlinedAt
                 : HashKind::TAG_DILocation_Line);
  /// Walk through getInlinedAt() and hash all the DILocation from all levels.
  if (SiteLoc) {
    hashDILocation(SiteLoc, DIF);
  }
}

const GlobalVariable *
FunctionHashCalculator::getAddressFromConstant(const Constant *C) {
  if (auto *GV = dyn_cast<GlobalVariable>(C)) {
    // e.g. store i32 1, i32 * @GV
    if (GV->hasDefinitiveInitializer()) {
      if (auto *MemoryGV = dyn_cast<GlobalVariable>(GV->getInitializer()))
        return getAddressFromConstant(MemoryGV);
    }
    return GV;
  }

  if (auto *GA = dyn_cast<GlobalAlias>(C))
    if (GA->getAliasee() && !GA->isInterposable())
      return getAddressFromConstant(GA->getAliasee());

  // If the loaded value isn't a constant expr, we can't handle it.
  auto *CE = dyn_cast<ConstantExpr>(C);
  if (!CE)
    return nullptr;

  if (CE->getOpcode() == Instruction::GetElementPtr ||
      CE->getOpcode() == Instruction::BitCast) {
    return getAddressFromConstant(CE->getOperand(0));
  }
  return nullptr;
}

const GlobalVariable *
FunctionHashCalculator::getAddressFromValue(const Value *V) {
  if (auto *C = dyn_cast<Constant>(V)) {
    return getAddressFromConstant(C);
  } else if (auto *LI = dyn_cast<LoadInst>(V)) {
    // e.g.
    // @a = dso_local global i32 0, align 4
    // @b = dso_local global i32 * @a, align 8
    // %0 = load i32*, i32** @b, align 8
    // store i32 5, i32* %0, align 4
    auto LoadAddress = LI->getPointerOperand();
    return getAddressFromValue(LoadAddress);
  }
  return nullptr;
}

const GlobalVariable *
FunctionHashCalculator::getStoreAddress(const StoreInst *SI) {
  return getAddressFromValue(SI->getPointerOperand());
}

/// Accumulate the instruction hash. The opcodes, type, operand types, operands
/// value and any other factors affecting the operation must be considered.
void FunctionHashCalculator::hashInstruction(const Instruction *V,
                                             DIFileRecord &DIF) {
  update(HashKind::TAG_Instruction);
  // Accumulate the hash of the instruction opcode.
  FnHash.hashNumber(V->getOpcode());
  // Instruction return type.
  FnHash.hashType(V->getType());
  FnHash.hashNumber(V->getRawSubclassOptionalData());
  // Accumulate the instruction operands type and value.
  for (unsigned I = 0, E = V->getNumOperands(); I != E; ++I) {
    const auto *Operand = V->getOperand(I);
    FnHash.hashType(Operand->getType());
    FnHash.hashValue(Operand);
  }

  update(HashKind::TAG_DILocation);
  if (auto DbgLoc = V->getDebugLoc()) {
    hashDILocation(DbgLoc, DIF);
  }

  if (const CallInst *CI = dyn_cast<CallInst>(V)) {
    update(HashKind::TAG_CallInst);
    update(CI->isTailCall());
    hashCallInvoke(CI);
    addContributionsFromCallInvoke(CI);
    return;
  }
  if (const InvokeInst *II = dyn_cast<InvokeInst>(V)) {
    update(HashKind::TAG_InvokeInst);
    hashCallInvoke(II);
    addContributionsFromCallInvoke(II);
    return;
  }

  // special GetElementPtrInst instruction.
  if (const GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(V)) {
    update(HashKind::TAG_GetElementPtrInst);
    FnHash.hashType(GEP->getSourceElementType());
    return;
  }
  // Check special state that is a part of some instructions.
  if (const AllocaInst *AI = dyn_cast<AllocaInst>(V)) {
    update(HashKind::TAG_AllocaInst);
    FnHash.hashType(AI->getAllocatedType());
    FnHash.hashNumber(AI->getAlignment());
    return;
  }
  if (const LoadInst *LI = dyn_cast<LoadInst>(V)) {
    update(HashKind::TAG_LoadInst);
    update(LI->isVolatile());
    FnHash.hashNumber(LI->getAlignment());
    FnHash.hashOrdering(LI->getOrdering());
    update(LI->getSyncScopeID());
    // FIXME: Is there any other Metadata need to be considered??
    FnHash.hashRangeMetadata(LI->getMetadata(LLVMContext::MD_range));
    return;
  }
  if (const StoreInst *SI = dyn_cast<StoreInst>(V)) {
    update(HashKind::TAG_StoreInst);
    update(SI->isVolatile());
    FnHash.hashNumber(SI->getAlignment());
    FnHash.hashOrdering(SI->getOrdering());
    update(SI->getSyncScopeID());
    if (auto *GV = getStoreAddress(SI)) {
      FnHash.getContributions().emplace_back(GV);
    }
    return;
  }
  if (const CmpInst *CI = dyn_cast<CmpInst>(V)) {
    update(HashKind::TAG_CmpInst);
    update(CI->getPredicate());
    return;
  }
  if (const InsertValueInst *IVI = dyn_cast<InsertValueInst>(V)) {
    update(HashKind::TAG_InsertValueInst);
    ArrayRef<unsigned> Indices = IVI->getIndices();
    update(ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Indices.data()),
                             sizeof(unsigned) * Indices.size()));
    return;
  }
  if (const ExtractValueInst *EVI = dyn_cast<ExtractValueInst>(V)) {
    update(HashKind::TAG_ExtractValueInst);
    ArrayRef<unsigned> Indices = EVI->getIndices();
    update(ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Indices.data()),
                             sizeof(unsigned) * Indices.size()));
    return;
  }
  if (const FenceInst *FI = dyn_cast<FenceInst>(V)) {
    update(HashKind::TAG_FenceInst);
    FnHash.hashOrdering(FI->getOrdering());
    update(FI->getSyncScopeID());
    return;
  }
  if (const AtomicCmpXchgInst *CXI = dyn_cast<AtomicCmpXchgInst>(V)) {
    update(HashKind::TAG_AtomicCmpXchgInst);
    update(CXI->isVolatile());
    update(CXI->isWeak());
    FnHash.hashOrdering(CXI->getSuccessOrdering());
    FnHash.hashOrdering(CXI->getFailureOrdering());
    update(CXI->getSyncScopeID());
    return;
  }
  if (const AtomicRMWInst *RMWI = dyn_cast<AtomicRMWInst>(V)) {
    update(HashKind::TAG_AtomicRMWInst);
    update(RMWI->getOperation());
    update(RMWI->isVolatile());
    FnHash.hashOrdering(RMWI->getOrdering());
    update(RMWI->getSyncScopeID());
    return;
  }
  if (const PHINode *PN = dyn_cast<PHINode>(V)) {
    update(HashKind::TAG_PHINode);
    // Ensure that in addition to the incoming values being identical
    // (checked by the caller of this function), the incoming blocks
    // are also identical.
    for (unsigned I = 0, E = PN->getNumIncomingValues(); I != E; ++I) {
      FnHash.hashValue(PN->getIncomingBlock(I));
    }
  }
}

void FunctionHashCalculator::hashBasicBlock(const BasicBlock *BB,
                                            DIFileRecord &DIF) {
  update(HashKind::TAG_BasicBlock);
  BasicBlock::const_iterator Inst = BB->begin(), InstE = BB->end();
  do {
    hashInstruction(&*Inst, DIF);
    ++Inst;
  } while (Inst != InstE);
}

void FunctionHashCalculator::hashFunction() {
  update(HashKind::TAG_GlobalFunction);
  hashSignature(Fn);

  // We do a CFG-ordered walk since the actual ordering of the blocks in the
  // linked list is immaterial. Our walk starts at the entry block for both
  // functions, then takes each block from each terminator in order. As an
  // artifact, this also means that unreachable blocks are ignored.
  SmallVector<const BasicBlock *, 8> FnBBs;
  SmallPtrSet<const BasicBlock *, 32> VisitedBBs; // in terms of F1.
  DIFileRecord DIFMap;

  FnBBs.push_back(&Fn->getEntryBlock());

  VisitedBBs.insert(FnBBs[0]);
  while (!FnBBs.empty()) {
    const BasicBlock *BB = FnBBs.pop_back_val();
    FnHash.hashValue(BB);
    hashBasicBlock(BB, DIFMap);

    const Instruction *Term = BB->getTerminator();
    for (unsigned I = 0, E = Term->getNumSuccessors(); I != E; ++I) {
      if (!VisitedBBs.insert(Term->getSuccessor(I)).second)
        continue;
      FnBBs.push_back(Term->getSuccessor(I));
    }
  }
}

void FunctionHashCalculator::calculateHash() {
  FnHash.beginCalculate(*Fn->getParent());
  hashFunction();
#ifndef NDEBUG
  LLVM_DEBUG(dbgs() << "\nGO name is:" << Fn->getName()
                    << ". Its Dependencies are: ");
  for (auto D : FnHash.getDependencies()) {
    LLVM_DEBUG(dbgs() << D->getName() << ", ");
  }
  LLVM_DEBUG(dbgs() << ". Its Contributions are: ");
  for (auto C : FnHash.getContributions()) {
    LLVM_DEBUG(dbgs() << C->getName() << ", ");
  }
  LLVM_DEBUG(dbgs() << "\n");
#endif
}

/// Calculate the function hash and return the result as the words.
MD5::MD5Result FunctionHashCalculator::getHashResult() {
  return FnHash.getHashResult();
}

void VariableHashCalculator::hashVariable() {
  update(HashKind::TAG_GlobalVariable);
  GvHash.hashType(Gv->getValueType());
  // If global variable is constant, accumulate the const attribute.
  update(HashKind::TAG_GVIsConstant);
  update(Gv->isConstant());
  // Accumulate the thread local mode.
  update(HashKind::TAG_GVThreadLocalMode);
  update(Gv->getThreadLocalMode());
  // Accumulate the alignment of global variable.
  update(HashKind::TAG_GVAlignment);
  GvHash.hashNumber(Gv->getAlignment());
  // Accumulate an optional unnamed_addr or local_unnamed_addr attribute.
  update(HashKind::TAG_GVUnnamedAddr);
  update(static_cast<uint8_t>(Gv->getUnnamedAddr()));
  // Global variable is constant type. Accumulate the initial value.
  // This accumulation also cover the "llvm.global_ctors",
  // "llvm.global_dtors", "llvm.used" and "llvm.compiler.used" cases.
  // If the weak global varible is initialized to another global variable, the
  // weak data should belong to .data section and has a XFixup to that global
  // data. The hash value should also accumulate the initial value of the weak
  // global value.
  GvHash.hashConstant((Gv->hasInitializer())
                          ? Gv->getInitializer()
                          : Constant::getNullValue(Gv->getValueType()));
}

// Calculate the global Variable hash value.
void VariableHashCalculator::calculateHash() {
  GvHash.beginCalculate(*Gv->getParent());
  hashVariable();
}
