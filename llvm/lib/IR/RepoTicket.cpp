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
  auto MD = MDB.createTicketNode(GO->getName(), D, GO->getLinkage());
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

static const DependenciesType &
updateInitialDigestAndGetDependencies(const GlobalObject *GO, MD5 &GOHash,
                                      GOInfoMap &GOIMap, GONumber &GONum) {
  GOInfoMap::const_iterator Pos;
  if (const auto GV = dyn_cast<GlobalVariable>(GO)) {
    ++GONum.VarNum;
    Pos = calculateInitialDigestAndDependenciesAndContributions(GV, GOIMap);
  } else if (const auto Fn = dyn_cast<Function>(GO)) {
    ++GONum.FuncNum;
    Pos = calculateInitialDigestAndDependenciesAndContributions(Fn, GOIMap);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
  GOHash.update(Pos->second.InitialDigest.Bytes);
  return Pos->second.Dependencies;
}

GODigestState updateDigestGONumAndGetDependencies(const GlobalObject *GO,
                                                  MD5 &GOHash,
                                                  GOInfoMap &GOIMap,
                                                  GONumber &GONum) {
  auto It = GOIMap.find(GO);
  if (It != GOIMap.end()) {
    const GOInfo &GOInformation = It->second;
    GOHash.update(GOInformation.InitialDigest.Bytes);
    return {GOInformation.Dependencies, true};
  }

  return {updateInitialDigestAndGetDependencies(GO, GOHash, GOIMap, GONum),
          true};
}

GODigestState updateGODigestAndGetDependencies(const GlobalObject *GO,
                                               MD5 &GOHash, GOInfoMap &GOIMap) {
  if (auto GOMD = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
    if (const TicketNode *TN = dyn_cast<TicketNode>(GOMD)) {
      DigestType D = TN->getDigest();
      GOHash.update(D.Bytes);
      return {GOIMap
                  .try_emplace(GO, std::move(D), DependenciesType(),
                               ContributionsType())
                  .first->second.Dependencies,
              true};
    }
    report_fatal_error("Failed to get TicketNode metadata!");
  }
  GONumber GONum;
  return {updateInitialDigestAndGetDependencies(GO, GOHash, GOIMap, GONum),
          true};
}

GODigestState updateDigestUseContributions(const GlobalObject *GO, MD5 &GOHash,
                                           GOInfoMap &GOIMap,
                                           GVInfoMap GVIMap) {
  bool Changed = false;
  if (const auto &GV = dyn_cast<GlobalVariable>(GO)) {
    if (GVIMap.count(GV)) {
      GOHash.update(getTicket(GO)->getDigest().Bytes);
      Changed = true;
    }
  }
  return {GOIMap[GO].Dependencies, Changed};
}

// Update the GO's hash value by adding the hash of its dependents.
template <typename Function>
static bool
updateDigestUseCallDependencies(const GlobalObject *GO, MD5 &GOHash,
                                GOStateMap &Visited, GOInfoMap &GOIMap,
                                Function UpdateDigestAndGetDependencies) {
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
  auto State = UpdateDigestAndGetDependencies(GO, GOHash, GOIMap);
  Changed = Changed || State.second;

  // Recursively for all the dependent global objects.
  for (const GlobalObject *D : State.first) {
    const llvm::Function *Fn = dyn_cast<const llvm::Function>(D);
    // if function will not be inlined, skip it
    if (Fn && Fn->hasFnAttribute(Attribute::NoInline))
      continue;
    Changed = updateDigestUseCallDependencies(D, GOHash, Visited, GOIMap,
                                              UpdateDigestAndGetDependencies) ||
              Changed;
  }
  return Changed;
}

// Calculate the final GO hash by adding the initial hashes of its dependents
// and the contributions and create the ticket metadata for GOs.
std::tuple<bool, unsigned, unsigned> generateTicketMDs(Module &M) {
  bool Changed = false;
  GONumber GONum;
  GOStateMap Visited;
  GOInfoMap GOIMap;
  GVInfoMap GVIMap;

  for (auto &GO : M.global_objects()) {
    if (GO.isDeclaration())
      continue;
    Visited.clear();
    MD5 Hash = MD5();
    auto Helper = [&GONum](const GlobalObject *GO, MD5 &GOHash,
                           GOInfoMap &GOIMap) {
      return updateDigestGONumAndGetDependencies(GO, GOHash, GOIMap, GONum);
    };
    updateDigestUseCallDependencies(&GO, Hash, Visited, GOIMap, Helper);
    for (const auto GV : GOIMap[&GO].Contributions) {
      // Record GO's possible contributions.
      GVIMap[GV].insert(&GO);
    }

    MD5::MD5Result Digest;
    Hash.final(Digest);
    set(&GO, Digest);
    Changed = true;
  }

  if (GVIMap.empty()) {
    return std::make_tuple(Changed, GONum.VarNum, GONum.FuncNum);
  }

  // Update the GV's digest using the contributions.
  for (auto &Entry : GVIMap) {
    if (Entry.first->isDeclaration())
      continue;

    MD5 GVHash = MD5();
    GVHash.update(getTicket(Entry.first)->getDigest().Bytes);
    // Update GV's hash using the contributions.
    for (const GlobalObject *GO : Entry.second) {
      GVHash.update(getTicket(GO)->getDigest().Bytes);
    }
    MD5::MD5Result Digest;
    GVHash.final(Digest);
    set(const_cast<GlobalVariable *>(Entry.first), Digest);
  }

  // If GO's dependents include the updated GV, update the GO's digest.
  for (auto &GO : M.global_objects()) {
    if (GO.isDeclaration())
      continue;

    Visited.clear();
    MD5 Hash = MD5();

    Hash.update(getTicket(&GO)->getDigest().Bytes);
    auto Helper = [&GVIMap](const GlobalObject *GO, MD5 &GOHash,
                            GOInfoMap &GOIMap) {
      return updateDigestUseContributions(GO, GOHash, GOIMap, GVIMap);
    };

    if (updateDigestUseCallDependencies(&GO, Hash, Visited, GOIMap, Helper)) {
      MD5::MD5Result Digest;
      Hash.final(Digest);
      set(&GO, Digest);
    }
  }

  return std::make_tuple(Changed, GONum.VarNum, GONum.FuncNum);
}

DigestType calculateDigest(const GlobalObject *GO) {
  GOStateMap Visited;
  MD5 Hash = MD5();
  GOInfoMap GOIMap;

  updateDigestUseCallDependencies(GO, Hash, Visited, GOIMap,
                                  updateGODigestAndGetDependencies);
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
                                GlobalValue::LinkageTypes Linkage, bool Pruned,
                                StorageType Storage, bool ShouldCreate) {
  if (Storage == Uniqued) {
    if (auto *N =
            getUniqued(Context.pImpl->TicketNodes,
                       TicketNodeInfo::KeyTy(Linkage, Pruned, Name, Digest)))
      return N;
    if (!ShouldCreate)
      return nullptr;
  } else {
    assert(ShouldCreate && "Expected non-uniqued nodes to always be created");
  }

  assert(isCanonical(Name) && "Expected canonical MDString");
  Metadata *Ops[] = {Name, Digest};
  return storeImpl(new (array_lengthof(Ops))
                       TicketNode(Context, Storage, Linkage, Pruned, Ops),
                   Storage, Context.pImpl->TicketNodes);
}

TicketNode *TicketNode::getImpl(LLVMContext &Context, StringRef Name,
                                ticketmd::DigestType const &Digest,
                                GlobalValue::LinkageTypes Linkage, bool Pruned,
                                StorageType Storage, bool ShouldCreate) {
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
  return getImpl(Context, MDName, MDDigest, Linkage, Pruned, Storage,
                 ShouldCreate);
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
