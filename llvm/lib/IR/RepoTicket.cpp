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

// Loop through the contributions and add the initial digest of each global
// object inside of the contributions to GOHash. Return true if GOhash is
// changed.
static bool updateDigestUseContributions(MD5 &GOHash,
                                         const GOVec &Contributions,
                                         GOInfoMap &GOIMap) {
  for (const GlobalObject *const G : Contributions) {
    GOHash.update(GOIMap[G].InitialDigest.Bytes);
  }
  return !Contributions.empty();
}

// Loop through the dependences and add the final digest of each global object
// inside of the dependences to GOHash.
template <typename Function>
static auto updateDigestUseDependencies(const GlobalObject *GO, MD5 &GOHash,
                                        unsigned GOVisitedIndex,
                                        const GOVec &Dependencies,
                                        GOStateMap &Visited, GOInfoMap &GOIMap,
                                        MemoizedHashes &GOHashCache,
                                        Function AccumulateGODigest)
    -> std::tuple<bool, size_t, DigestType> {
  bool Changed = false;
  auto LoopPoint = std::numeric_limits<size_t>::max();
  for (const GlobalObject *const G : Dependencies) {
    const llvm::Function *const Fn = dyn_cast<const llvm::Function>(G);
    // if function will not be inlined and not be discarded if it is not used,
    // skip it.
    if (Fn && Fn->hasFnAttribute(Attribute::NoInline) &&
        !Fn->isDiscardableIfUnused())
      continue;
    bool GChanged;
    size_t GVisitedIndex;
    DigestType GDigest;
    std::tie(GChanged, GVisitedIndex, GDigest) =
        updateDigestUseDependenciesAndContributions<Function>(
            G, Visited, GOIMap, GOHashCache, AccumulateGODigest);
    Changed = Changed || GChanged;
    // A GO which loops back to itself doesn't count as a loop.
    if (G != GO)
      LoopPoint = std::min(LoopPoint, GVisitedIndex);
    GOHash.update(GDigest.Bytes);
  }
  // Encoded the final edge. Record that in the hash.
  GOHash.update(static_cast<char>(Tags::End));

  DigestType Digest;
  GOHash.final(Digest);

  if (LoopPoint > GOVisitedIndex) {
    GOHashCache[GO] = Digest;
    LLVM_DEBUG(dbgs() << "Recording result for \"" << GO->getName() << "\"\n");
    ++NumMemoizedHashes;
    ++VisitedCachedGOTimes;
  } else {
    ++VisitedUnCachedGOTimes;
  }
  return std::make_tuple(Changed, LoopPoint, Digest);
}

// Update the digest of an invidual GO incorporating the hashes of all its
// dependencies and contributions.
// \param GO The global object whose digest is to be computed.
// \param Visited A map of GO to a unique number in the dependencies or
//        contributions graph.
// \param GOIMap A map of GO to GOInfo
// \param GOHashCache Used to record memoized hashes.
// \param AccumulateGODigest A function is used to accumulate the GO's digest.
// \returns a tuple containing the state information which includes 1) return
// true if the module M has been changed, 2) a numerical identifier which will
// be used if GO loops back to itself in future and 3) the GO's digest.
template <typename Function>
static auto updateDigestUseDependenciesAndContributions(
    const GlobalObject *GO, GOStateMap &Visited, GOInfoMap &GOIMap,
    MemoizedHashes &GOHashCache, Function AccumulateGODigest)
    -> std::tuple<bool, size_t, DigestType> {
  if (GO->isDeclaration())
    return std::make_tuple(false, std::numeric_limits<size_t>::max(),
                           NullDigest);

  const auto GOVisitedIndex = Visited.size();
  LLVM_DEBUG(dbgs() << "Computing hash for \"" << GO->getName() << "\" (#"
                    << GOVisitedIndex << ")\n");

  // If the hash has been memoized, we can return the result immediately.
  const auto TablePos = GOHashCache.find(GO);
  if (TablePos != GOHashCache.end()) {
    ++VisitedCachedGOTimes;
    LLVM_DEBUG(dbgs() << "Returning pre-computed hash for \"" << GO->getName()
                      << "\"\n");
    return std::make_tuple(true, GOVisitedIndex, TablePos->second);
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
    return std::make_tuple(true, StateIt->second, Digest);
  }

  GOHash.update(static_cast<char>(Tags::GO));
  auto State = AccumulateGODigest(GO, GOHash, GOIMap);
  bool Changed = State.Changed;
  // The hashes of the GO's 'contributions' and 'dependencies' are separately
  // added to the GO's hash since their respective arrows point are in different
  // direction. In addition, 'contributions' cannot form loops since 1) only
  // global variables have 'contributions' and 2) all 'contributions' are
  // functions. Therefore, the 'contributions' can be simplified not to record
  // 'visited' table, which avoids having two 'visited' maps. Firstly, add the
  // GO's contributions to its hash.
  Changed = updateDigestUseContributions(GOHash, State.Contributions, GOIMap) ||
            Changed;

  // Add the GO's dependencies to its hash.
  auto GOState = updateDigestUseDependencies<Function>(
      GO, GOHash, GOVisitedIndex, State.Dependencies, Visited, GOIMap,
      GOHashCache, AccumulateGODigest);
  std::get<0>(GOState) = std::get<0>(GOState) || Changed;
  return std::move(GOState);
}

// Calculate the GOs' initial digest, dependencies, contributions and the
// number of hashed global variables and functions.
GOInfoMap calculateGONumAndGOIMap(Module &M) {
  GOInfoMap GOIMap;

  for (auto &GV : M.globals()) {
    if (GV.isDeclaration())
      continue;
    calculateGOInfo(&GV, GOIMap);
    ++NumVariables;
  }
  for (auto &Fn : M.functions()) {
    if (Fn.isDeclaration())
      continue;
    calculateGOInfo(&Fn, GOIMap);
    ++NumFunctions;
  }
  return std::move(GOIMap);
}

bool generateTicketMDs(Module &M) {
  bool Changed = false;
  GOStateMap Visited;
  auto GOSize = M.size() + M.getGlobalList().size();
  Visited.reserve(GOSize);
  GOInfoMap GOIMap;
  MemoizedHashes GOHashCache;
  GOHashCache.reserve(GOSize);

  // Calculate the GOInfoMap.
  GOIMap = calculateGONumAndGOIMap(M);

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
    auto Helper = [](const GlobalObject *GO, MD5 &GOHash, GOInfoMap &GOIMap) {
      return accumulateGOInitialDigestAndGetGODigestState(GO, GOHash, GOIMap);
    };
    set(&GO, std::get<2>(updateDigestUseDependenciesAndContributions(
                 &GO, Visited, GOIMap, GOHashCache, Helper)));
    Changed = true;
  }

  return Changed;
}

DigestType calculateDigest(const GlobalObject *GO) {
  GOStateMap Visited;
  GOInfoMap GOIMap;
  MemoizedHashes GOHashCache;
  updateDigestUseDependenciesAndContributions(
      GO, Visited, GOIMap, GOHashCache,
      accumulateGOFinalDigestOrInitialDigestAndGetGODigestState);

  return std::get<2>(updateDigestUseDependenciesAndContributions(
      GO, Visited, GOIMap, GOHashCache,
      accumulateGOFinalDigestOrInitialDigestAndGetGODigestState));
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
