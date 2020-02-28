//===---- RepoTicket.cpp -  Implement digest data structure. ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the digest data structure.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoTicket.h"
#include "LLVMContextImpl.h"
#include "MetadataImpl.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>

using namespace llvm;

namespace llvm {

namespace ticketmd {

void set(GlobalObject *GO, ticketmd::DigestType const &D) {
  auto M = GO->getParent();
  MDBuilder MDB(M->getContext());
  auto MD = MDB.createTicketNode(GO->getName(), D, GO->getLinkage(),
                                 GO->getVisibility());
  assert(MD && "TicketNode cannot be NULL!");
  GO->setMetadata(LLVMContext::MD_repo_ticket, MD);
  NamedMDNode *NMD = M->getOrInsertNamedMetadata("repo.tickets");
  assert(NMD && "NamedMDNode cannot be NULL!");
  // Add GO's TicketNode metadata to the module's repo.tickets metadata if the
  // GO will be emitted to the object file.
  if (!GO->hasAvailableExternallyLinkage())
    NMD->addOperand(MD);
}

const TicketNode *getTicket(const GlobalObject *GO) {
  if (const auto *const T = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *const MD = dyn_cast<TicketNode>(T)) {
      return MD;
    }
  }
  report_fatal_error("Failed to get TicketNode metadata!");
}

auto get(const GlobalObject *GO) -> std::pair<ticketmd::DigestType, bool> {

  if (const auto *const T = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *const MD = dyn_cast<TicketNode>(T)) {
      return std::make_pair(MD->getDigest(), false);
    }
    // If invalid, report the error with report_fatal_error.
    report_fatal_error("Failed to get TicketNode metadata for global object '" +
                       GO->getName() + "'.");
  }

  return {calculateDigest(GO), true};
}

const Constant *getAliasee(const GlobalAlias *GA) {
  auto Aliasee = GA->getAliasee();
  assert(Aliasee && "Aliasee cannot be NULL!");
  auto Target = Aliasee->stripPointerCasts();
  assert(Target && "Target cannot be NULL!");
  // After stripping pointer casts, the target type should be only
  // GlobalValue type.
  assert(isa<GlobalValue>(Target) && "Aliasee should be only GlobalValue");
  return Target;
}

static void calculateGOInfo(const GlobalObject *GO, GOInfoMap &GOIMap) {

  if (const auto GV = dyn_cast<GlobalVariable>(GO)) {
    calculateGOInfo(GV, GOIMap);
  } else if (const auto Fn = dyn_cast<Function>(GO)) {
    calculateGOInfo(Fn, GOIMap);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
}

// Accumulate the GO's initial digest and get its GODigestState.
static const GODigestState
accumulateGOInitialDigestAndGetGODigestState(const GlobalObject *GO,
                                             MD5 &GOHash, GOInfoMap &GOIMap) {
  const GOInfo &GOInformation = GOIMap[GO];
  GOHash.update(GOInformation.InitialDigest.Bytes);
  return GODigestState(GOInformation.Contributions, GOInformation.Dependencies,
                       true);
}

static GODigestState accumulateGOFinalDigestOrInitialDigestAndGetGODigestState(
    const GlobalObject *GO, MD5 &GOHash, GOInfoMap &GOIMap) {
  if (const auto *const GOMD = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *const TN = dyn_cast<TicketNode>(GOMD)) {
      DigestType D = TN->getDigest();
      GOHash.update(D.Bytes);
      const GOInfo &GOInformation =
          GOIMap.try_emplace(GO, std::move(D), GOVec(), GOVec()).first->second;
      return GODigestState(GOInformation.Contributions,
                           GOInformation.Dependencies, true);
    }
    report_fatal_error("Failed to get TicketNode metadata!");
  }
  calculateGOInfo(GO, GOIMap);
  return accumulateGOInitialDigestAndGetGODigestState(GO, GOHash, GOIMap);
}

// Update the GO's hash value by adding the hash of its dependents.
template <typename Function>
static bool updateDigestUseDependenciesAndContributions(
    const GlobalObject *GO, MD5 &GOHash, GOStateMap &Visited, GOInfoMap &GOIMap,
    Function AccumulateGODigest) {
  if (GO->isDeclaration())
    return false;

  bool Inserted;
  typename GOStateMap::const_iterator StateIt;
  std::tie(StateIt, Inserted) = Visited.try_emplace(GO, Visited.size());
  if (!Inserted) {
    // If GO is visited, use the letter 'R' as the marker and use its state as
    // the value.
    GOHash.update('R');
    GOHash.update(StateIt->second);
    return true;
  }

  bool Changed = false;
  GOHash.update('T');
  auto State = AccumulateGODigest(GO, GOHash, GOIMap);
  Changed = Changed || State.Changed;
  // Update the GO's hash using its contributions and dependencies.
  auto &GOContributions = State.Contributions;
  auto &GODependencies = State.Dependencies;
  for (const GlobalObject *const G : llvm::concat<const GlobalObject *const>(
           make_range(GOContributions.begin(), GOContributions.end()),
           make_range(GODependencies.begin(), GODependencies.end()))) {
    const llvm::Function *const Fn = dyn_cast<const llvm::Function>(G);
    // if function will not be inlined and not be discarded if it is not used,
    // skip it.
    if (Fn && Fn->hasFnAttribute(Attribute::NoInline) &&
        !Fn->isDiscardableIfUnused())
      continue;
    Changed = updateDigestUseDependenciesAndContributions(
                  G, GOHash, Visited, GOIMap, AccumulateGODigest) ||
              Changed;
  }

  return Changed;
}

// Calculate the GOs' initial digest, dependencies, contributions and GONumber.
std::pair<GOInfoMap, GONumber> calculateGONumAndGOIMap(Module &M) {
  GOInfoMap GOIMap;
  GONumber GONum;

  for (auto &GV : M.globals()) {
    if (GV.isDeclaration())
      continue;
    calculateGOInfo(&GV, GOIMap);
    ++GONum.VarNum;
  }
  for (auto &Fn : M.functions()) {
    if (Fn.isDeclaration())
      continue;
    calculateGOInfo(&Fn, GOIMap);
    ++GONum.FuncNum;
  }
  return std::make_pair(std::move(GOIMap), std::move(GONum));
}

std::tuple<bool, unsigned, unsigned> generateTicketMDs(Module &M) {
  bool Changed = false;
  GONumber GONum;
  GOStateMap Visited;
  Visited.reserve(M.size() + M.getGlobalList().size());
  GOInfoMap GOIMap;

  // Calculate the GOInfoMap and GONumber.
  std::tie(GOIMap, GONum) = calculateGONumAndGOIMap(M);

#ifndef NDEBUG
  for (auto &GI : GOIMap) {
    LLVM_DEBUG(dbgs() << "\nGO Name:" << GI.first->getName() << "\n");
    LLVM_DEBUG(GI.second.dump());
  }
#endif

  // Update the GO's digest using the dependencies and the contributions.
  for (auto &GO : M.global_objects()) {
    if (GO.isDeclaration())
      continue;
    Visited.clear();
    MD5 Hash = MD5();
    auto Helper = [](const GlobalObject *GO, MD5 &GOHash, GOInfoMap &GOIMap) {
      return accumulateGOInitialDigestAndGetGODigestState(GO, GOHash, GOIMap);
    };
    updateDigestUseDependenciesAndContributions(&GO, Hash, Visited, GOIMap,
                                                Helper);
    MD5::MD5Result Digest;
    Hash.final(Digest);
    set(&GO, Digest);
    Changed = true;
  }

  return std::make_tuple(Changed, GONum.VarNum, GONum.FuncNum);
}

DigestType calculateDigest(const GlobalObject *GO) {
  GOStateMap Visited;
  MD5 Hash = MD5();
  GOInfoMap GOIMap;

  updateDigestUseDependenciesAndContributions(
      GO, Hash, Visited, GOIMap,
      accumulateGOFinalDigestOrInitialDigestAndGetGODigestState);
  MD5::MD5Result Digest;
  Hash.final(Digest);
  return Digest;
}

} // namespace ticketmd
#ifndef NDEBUG
static bool isCanonical(const MDString *S) {
  return !S || !S->getString().empty();
}
#endif

TicketNode *TicketNode::getImpl(LLVMContext &Context, MDString *Name,
                                ConstantAsMetadata *Digest,
                                GlobalValue::LinkageTypes Linkage,
                                GlobalValue::VisibilityTypes Visibility,
                                bool Pruned, StorageType Storage,
                                bool ShouldCreate) {
  if (Storage == Uniqued) {
    if (auto *N = getUniqued(
            Context.pImpl->TicketNodes,
            TicketNodeInfo::KeyTy(Linkage, Visibility, Pruned, Name, Digest)))
      return N;
    if (!ShouldCreate)
      return nullptr;
  } else {
    assert(ShouldCreate && "Expected non-uniqued nodes to always be created");
  }

  assert(isCanonical(Name) && "Expected canonical MDString");
  Metadata *Ops[] = {Name, Digest};
  return storeImpl(new (array_lengthof(Ops)) TicketNode(
                       Context, Storage, Linkage, Visibility, Pruned, Ops),
                   Storage, Context.pImpl->TicketNodes);
}

TicketNode *TicketNode::getImpl(LLVMContext &Context, StringRef Name,
                                ticketmd::DigestType const &Digest,
                                GlobalValue::LinkageTypes Linkage,
                                GlobalValue::VisibilityTypes Visibility,
                                bool Pruned, StorageType Storage,
                                bool ShouldCreate) {
  MDString *MDName = nullptr;
  if (!Name.empty())
    MDName = MDString::get(Context, Name);
  MDBuilder MDB(Context);
  const auto Size = ticketmd::DigestSize;
  llvm::Constant *Field[Size];
  Type *Int8Ty = Type::getInt8Ty(Context);
  for (unsigned Idx = 0; Idx < Size; ++Idx) {
    Field[Idx] = llvm::ConstantInt::get(Int8Ty, Digest[Idx], false);
  }
  // Array implementation that the hash is outputed as char/string.
  ConstantAsMetadata *MDDigest = ConstantAsMetadata::get(
      ConstantArray::get(llvm::ArrayType::get(Int8Ty, Size), Field));
  return getImpl(Context, MDName, MDDigest, Linkage, Visibility, Pruned,
                 Storage, ShouldCreate);
}

ticketmd::DigestType TicketNode::getDigest() const {
  ConstantAsMetadata const *C = getDigestAsMDConstant();
  auto const ArrayType = C->getType();
  auto const Elems = ArrayType->getArrayNumElements();
  ticketmd::DigestType D;

  assert(Elems == D.Bytes.max_size() &&
         "Global object has invalid digest array size.");
  for (unsigned I = 0, E = Elems; I != E; ++I) {
    ConstantInt const *CI =
        dyn_cast<ConstantInt>(C->getValue()->getAggregateElement(I));
    assert(CI);
    D[I] = CI->getValue().getZExtValue();
  }
  return D;
}

} // namespace llvm
