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
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "prepo-digest"

STATISTIC(NumFunctions, "Number of functions hashed");
STATISTIC(NumVariables, "Number of variables hashed");
STATISTIC(NumMemoizedHashes, "Number of memoized hashes");
STATISTIC(VisitedUnCachedGOTimes, "Visited times of unmemoized hashes");
STATISTIC(VisitedCachedGOTimes, "Visited times of memoized hashes");

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

template <typename GlobalType>
void HashCache::calculateGOInfo(const GlobalType *G) {
  GOInfo Result = calculateDigestAndDependenciesAndContributedToGVs(G);
  // ContributedToGVs of an object is used to update other objects'
  // contributions. For example, if the ContributedToGVs of function `foo`  are
  // global variables `g` and `q`, the function `foo` is in the contributions of
  // `g` and `q`.
  // ContributedToGVs[`foo`] = [`g`, `q`]
  // ====> Contributions[`g`] = [`foo`],
  //       Contributions[`q`] = [`foo`]
  for (auto &GO : Result.Contributions) {
    assert(isa<GlobalVariable>(GO) &&
           "Only global variables can have contributions!");
    assert(isa<llvm::Function>(G) && "All contributions are functions!");
    GOIMap[GO].Contributions.emplace_back(G);
  }
  // Update G's dependencies.
  GOInfo &GInfo = GOIMap[G];
  GInfo.InitialDigest = std::move(Result.InitialDigest);
  GInfo.Dependencies = std::move(Result.Dependencies);
}

void HashCache::calculateGOInfo(const GlobalObject *GO) {
  if (const auto GV = dyn_cast<GlobalVariable>(GO)) {
    calculateGOInfo(GV);
  } else if (const auto Fn = dyn_cast<Function>(GO)) {
    calculateGOInfo(Fn);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
}

GODigestState HashCache::accumulateGODigest(const GlobalObject *GO, MD5 &GOHash,
                                            bool Final) {
  if (Final) {
    if (const auto *const GOMD = GO->getMetadata(LLVMContext::MD_repo_ticket)) {
      if (const TicketNode *const TN = dyn_cast<TicketNode>(GOMD)) {
        DigestType D = TN->getDigest();
        GOHash.update(D.Bytes);
        const GOInfo &GOInformation =
            GOIMap.try_emplace(GO, std::move(D), GOVec(), GOVec())
                .first->second;
        Changed = true;
        return GODigestState(GOInformation.Contributions,
                             GOInformation.Dependencies);
      }
      report_fatal_error("Failed to get TicketNode metadata!");
    }
    calculateGOInfo(GO);
  }
  const GOInfo &GOInformation = GOIMap[GO];
  GOHash.update(GOInformation.InitialDigest.Bytes);
  Changed = true;
  return GODigestState(GOInformation.Contributions, GOInformation.Dependencies);
}

// Calculate the GOs' initial digest, dependencies, contributions and the
// number of hashed global variables and functions.
void HashCache::calculateGONumAndGOIMap(const Module &M) {
  for (auto &GV : M.globals()) {
    if (GV.isDeclaration())
      continue;
    calculateGOInfo(&GV);
    ++NumVariables;
  }
  for (auto &Fn : M.functions()) {
    if (Fn.isDeclaration())
      continue;
    calculateGOInfo(&Fn);
    ++NumFunctions;
  }
#ifndef NDEBUG
  for (auto &GI : GOIMap) {
    LLVM_DEBUG(dbgs() << "\nGO Name:" << GI.first->getName() << "\n");
    LLVM_DEBUG(GI.second.dump());
  }
#endif
}

void HashCache::updateDigestUseContributions(MD5 &GOHash,
                                             const GOVec &Contributions) {
  for (const GlobalObject *const G : Contributions) {
    GOHash.update(GOIMap[G].InitialDigest.Bytes);
  }
  // GOHash/Module is changed if Contributions is not empty.
  Changed = Changed || !Contributions.empty();
}

auto HashCache::updateDigestUseDependencies(const GlobalObject *GO, MD5 &GOHash,
                                            unsigned GOVisitedIndex,
                                            const GOVec &Dependencies,
                                            bool Final)
    -> std::tuple<size_t, DigestType> {
  auto LoopPoint = std::numeric_limits<size_t>::max();
  for (const GlobalObject *const G : Dependencies) {
    const llvm::Function *const Fn = dyn_cast<const llvm::Function>(G);
    // if function will not be inlined and not be discarded if it is not used,
    // skip it.
    if (Fn && Fn->hasFnAttribute(Attribute::NoInline) &&
        !Fn->isDiscardableIfUnused())
      continue;
    size_t GVisitedIndex;
    DigestType GDigest;
    std::tie(GVisitedIndex, GDigest) =
        updateDigestUseDependenciesAndContributions(G, Final);
    // A GO which loops back to itself doesn't count as a loop.
    if (G != GO)
      LoopPoint = std::min(LoopPoint, GVisitedIndex);
    GOHash.update(GDigest.Bytes);
  }
  // Encoded the 'End' in the GOhash after accumulating all GO's Dependencies.
  GOHash.update(static_cast<char>(Tags::End));

  DigestType Digest;
  GOHash.final(Digest);

  // If LoopPoint is bigger than GOVisitedIndex, GO doesn't belong to any loops
  // (except self-loop). Record GO's hash in memoized hashes.
  if (LoopPoint > GOVisitedIndex) {
    GOHashCache[GO] = Digest;
    LLVM_DEBUG(dbgs() << "Recording result for \"" << GO->getName() << "\"\n");
    ++NumMemoizedHashes;
    ++VisitedCachedGOTimes;
  } else {
    ++VisitedUnCachedGOTimes;
  }
  return std::make_tuple(LoopPoint, Digest);
}

auto HashCache::updateDigestUseDependenciesAndContributions(
    const GlobalObject *GO, bool Final) -> std::tuple<size_t, DigestType> {
  assert(!GO->isDeclaration() && "Can only be used for global definitions");

  const auto GOVisitedIndex = Visited.size();
  LLVM_DEBUG(dbgs() << "Computing hash for \"" << GO->getName() << "\" (#"
                    << GOVisitedIndex << ")\n");

  // If the hash has been memoized, we can return the result immediately.
  bool Hit;
  DigestType GODigest;
  std::tie(GODigest, Hit) = get(GO);
  if (Hit) {
    ++VisitedCachedGOTimes;
    LLVM_DEBUG(dbgs() << "Returning pre-computed hash for \"" << GO->getName()
                      << "\"\n");
    return std::make_tuple(GOVisitedIndex, GODigest);
  }

  MD5 GOHash = MD5();
  bool Inserted;
  typename GOStateMap::const_iterator StateIt;
  std::tie(StateIt, Inserted) = Visited.try_emplace(GO, GOVisitedIndex);
  if (!Inserted) {
    assert(GOVisitedIndex > StateIt->second);
    // If GO is visited, mark as a Backref and use its state as the value.
    GOHash.update(static_cast<char>(Tags::Backref));
    GOHash.update(GOVisitedIndex - StateIt->second - 1U);
    LLVM_DEBUG(dbgs() << "Hashing back reference to #" << StateIt->second
                      << "\n");
    DigestType Digest;
    GOHash.final(Digest);
    Changed = true;
    return std::make_tuple(StateIt->second, Digest);
  }

  GOHash.update(static_cast<char>(Tags::GO));
  auto State = accumulateGODigest(GO, GOHash, Final);
  // The hashes of the GO's 'contributions' and 'dependencies' are separately
  // added to the GO's hash since their respective arrows point are in different
  // direction. In addition, 'contributions' cannot form loops since 1) only
  // global variables have 'contributions' and 2) all 'contributions' are
  // functions. Therefore, the 'contributions' can be simplified not to record
  // 'visited' table, which avoids having two 'visited' maps. Firstly, add the
  // GO's contributions to its hash.
  updateDigestUseContributions(GOHash, State.Contributions);

  // Add the GO's dependencies to its hash.
  return updateDigestUseDependencies(GO, GOHash, GOVisitedIndex,
                                     State.Dependencies, Final);
}

DigestType HashCache::calculateDigest(const GlobalObject *GO, bool Final) {
  Visited.clear();
  return std::get<1>(updateDigestUseDependenciesAndContributions(GO, Final));
}

HashCache::HashCache(Module &M) {
  auto GOSize = M.size() + M.getGlobalList().size();
  Visited.reserve(GOSize);
  GOHashCache.reserve(GOSize);
  Changed = false;

  // Calculate the GOInfoMap.
  calculateGONumAndGOIMap(M);

  // Update the GO's digest using the dependencies and the contributions.
  for (auto &GO : M.global_objects()) {
    if (GO.isDeclaration())
      continue;
    set(&GO, calculateDigest(&GO, false));
  }
}

DigestType HashCache::calculateDigest(const GlobalObject *GO) {
  return std::get<1>(updateDigestUseDependenciesAndContributions(GO, true));
}

bool generateTicketMDs(Module &M) {
  HashCache HC(M);
  return HC.isChanged();
}

DigestType calculateDigest(const GlobalObject *GO) {
  HashCache HC;
  return HC.calculateDigest(GO);
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
