//===- RepoDefinition.cpp - Implement Repo definition metadata. -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Repo definition data structure.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoDefinition.h"
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

namespace repodefinition {

void set(GlobalObject *GO, DigestType const &D) {
  auto M = GO->getParent();
  MDBuilder MDB(M->getContext());
  auto MD = MDB.createRepoDefinition(GO->getName(), D, GO->getLinkage(),
                                     GO->getVisibility());
  assert(MD && "RepoDefinition cannot be NULL!");
  GO->setMetadata(LLVMContext::MD_repo_definition, MD);
  NamedMDNode *NMD = M->getOrInsertNamedMetadata("repo.definitions");
  assert(NMD && "NamedMDNode cannot be NULL!");
  // Add GO's RepoDefinition metadata to the module's repo.definitions metadata
  // if the GO will be emitted to the object file.
  if (!GO->hasAvailableExternallyLinkage())
    NMD->addOperand(MD);
}

const RepoDefinition *getDefinition(const GlobalObject *GO) {
  if (const auto *const D = GO->getMetadata(LLVMContext::MD_repo_definition)) {
    if (const RepoDefinition *const MD = dyn_cast<RepoDefinition>(D)) {
      return MD;
    }
  }
  report_fatal_error("Failed to get RepoDefinition metadata!");
}

auto get(const GlobalObject *GO) -> std::pair<DigestType, bool> {

  if (const auto *const D = GO->getMetadata(LLVMContext::MD_repo_definition)) {
    if (const RepoDefinition *const MD = dyn_cast<RepoDefinition>(D)) {
      return std::make_pair(MD->getDigest(), false);
    }
    // If invalid, report the error with report_fatal_error.
    report_fatal_error(
        "Failed to get RepoDefinition metadata for global object '" +
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
void ModuleHashGenerator::calculateGOInfo(const GlobalType *G) {
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

void ModuleHashGenerator::calculateGOInfo(const GlobalObject *GO) {
  if (const auto GV = dyn_cast<GlobalVariable>(GO)) {
    calculateGOInfo(GV);
  } else if (const auto Fn = dyn_cast<Function>(GO)) {
    calculateGOInfo(Fn);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
}

GODigestState
ModuleHashGenerator::accumulateGODigest(const GlobalObject *GO, MD5 &GOHash,
                                        bool UseRepoDefinitionMD) {
  if (UseRepoDefinitionMD) {
    if (const auto *const GOMD =
            GO->getMetadata(LLVMContext::MD_repo_definition)) {
      if (const auto *const RD = dyn_cast<RepoDefinition>(GOMD)) {
        DigestType D = RD->getDigest();
        GOHash.update(D.Bytes);
        const GOInfo &GOInformation =
            GOIMap.try_emplace(GO, std::move(D), GOVec(), GOVec())
                .first->second;
        Changed = true;
        return GODigestState(GOInformation.Contributions,
                             GOInformation.Dependencies);
      }
      report_fatal_error("Failed to get RepoDefinition metadata!");
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
void ModuleHashGenerator::calculateGONumAndGOIMap(const Module &M) {
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

size_t ModuleHashGenerator::updateDigestUseContributions(
    const GlobalObject *GO, MD5 &GOHash, const GOVec &Contributions,
    bool UseRepoDefinitionMD) {
  // GOHash/Module is changed if Contributions is not empty.
  Changed = Changed || !Contributions.empty();

  auto LoopPoint = std::numeric_limits<size_t>::max();
  for (const GlobalObject *const G : Contributions) {
    // Although 'contributions' and 'dependencies' respective arrows point are
    // in different direction,  they can affect each other and form loops.
    // Consider the following example:
    // int var = 0;
    // int Func() {
    //    var = var + 5;
    //    return var;
    //  }
    // 'Func' is a contribution of 'var' and 'var' is a dependency of 'Func`.
    // Their hash values are dependent on each other. They can form loops.
    size_t GDepth;
    DigestType GDigest;
    std::tie(GDepth, GDigest) =
        updateDigestUseDependenciesAndContributions(G, UseRepoDefinitionMD);
    // A GO which loops back to itself doesn't count as a loop.
    if (G != GO)
      LoopPoint = std::min(LoopPoint, GDepth);
    GOHash.update(GDigest.Bytes);
  }
  return LoopPoint;
}

auto ModuleHashGenerator::updateDigestUseDependencies(
    const GlobalObject *GO, MD5 &GOHash, size_t ContributionsLoopPoint,
    unsigned GODepth, const GOVec &Dependencies, bool UseRepoDefinitionMD)
    -> std::tuple<size_t, DigestType> {
  auto LoopPoint = ContributionsLoopPoint;
  for (const GlobalObject *const G : Dependencies) {
    size_t GDepth;
    DigestType GDigest;
    std::tie(GDepth, GDigest) =
        updateDigestUseDependenciesAndContributions(G, UseRepoDefinitionMD);
    // A GO which loops back to itself doesn't count as a loop.
    if (G != GO)
      LoopPoint = std::min(LoopPoint, GDepth);
    GOHash.update(GDigest.Bytes);
  }
  // Encode the 'End' in the GOhash after accumulating all GO's Dependencies.
  GOHash.update(static_cast<char>(Tags::End));

  DigestType Digest;
  GOHash.final(Digest);

  // Memoize GO's hash if GO doesn't belong to any loops (except self-loop).
  // Loop not present if LoopPoint is greater than current GO depth.
  if (LoopPoint > GODepth) {
    GOHashCache[GO] = Digest;
    LLVM_DEBUG(dbgs() << "Recording result for \"" << GO->getName() << "\"\n");
    ++NumMemoizedHashes;
    ++VisitedCachedGOTimes;
  } else {
    ++VisitedUnCachedGOTimes;
  }
  return std::make_tuple(LoopPoint, Digest);
}

auto ModuleHashGenerator::updateDigestUseDependenciesAndContributions(
    const GlobalObject *GO, bool UseRepoDefinitionMD)
    -> std::tuple<size_t, DigestType> {
  assert(!GO->isDeclaration() && "Can only be used for global definitions");

  const auto GODepth = Visited.size();
  LLVM_DEBUG(dbgs() << "Computing hash for \"" << GO->getName() << "\" (#"
                    << GODepth << ")\n");

  // If the hash has been memoized, we can return the result immediately.
  bool Hit;
  DigestType GODigest;
  std::tie(GODigest, Hit) = getGOHash(GO);
  if (Hit) {
    ++VisitedCachedGOTimes;
    LLVM_DEBUG(dbgs() << "Returning pre-computed hash for \"" << GO->getName()
                      << "\"\n");
    return std::make_tuple(GODepth, GODigest);
  }

  MD5 GOHash = MD5();
  bool Inserted;
  typename GOStateMap::const_iterator StateIt;
  // Record visit to GO and its depth to detect loops in the future. If we have
  // previously visited this GO (on this path), add a back-reference to that
  // point and return.
  std::tie(StateIt, Inserted) = Visited.try_emplace(GO, GODepth);
  if (!Inserted) {
    assert(GODepth > StateIt->second);
    // If GO is visited, mark as a Backref and use its state as the value.
    GOHash.update(static_cast<char>(Tags::Backref));
    GOHash.update(GODepth - StateIt->second - 1U);
    LLVM_DEBUG(dbgs() << "Hashing back reference to #" << StateIt->second
                      << "\n");
    DigestType Digest;
    GOHash.final(Digest);
    Changed = true;
    // Return its position to the caller.
    return std::make_tuple(StateIt->second, Digest);
  }

  GOHash.update(static_cast<char>(Tags::GO));
  auto State = accumulateGODigest(GO, GOHash, UseRepoDefinitionMD);

  // Add the GO's contributions to its hash.
  auto ContributionLoopPoint = updateDigestUseContributions(
      GO, GOHash, State.Contributions, UseRepoDefinitionMD);

  // Add the GO's dependencies to its hash.
  return updateDigestUseDependencies(GO, GOHash, ContributionLoopPoint, GODepth,
                                     State.Dependencies, UseRepoDefinitionMD);
}

DigestType ModuleHashGenerator::calculateDigest(const GlobalObject *GO,
                                                bool UseRepoDefinitionMD) {
  Visited.clear();
  return std::get<DigestType>(
      updateDigestUseDependenciesAndContributions(GO, UseRepoDefinitionMD));
}

void ModuleHashGenerator::digestModule(Module &M) {
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

bool generateRepoDefinitions(Module &M) {
  ModuleHashGenerator MHG;
  MHG.digestModule(M);
  return MHG.isChanged();
}

DigestType calculateDigest(const GlobalObject *GO) {
  ModuleHashGenerator MHG;
  return MHG.calculateDigest(GO, true);
}

} // namespace repodefinition
#ifndef NDEBUG
static bool isCanonical(const MDString *S) {
  return !S || !S->getString().empty();
}
#endif

RepoDefinition *RepoDefinition::getImpl(LLVMContext &Context, MDString *Name,
                                        ConstantAsMetadata *Digest,
                                        GlobalValue::LinkageTypes Linkage,
                                        GlobalValue::VisibilityTypes Visibility,
                                        bool Pruned, StorageType Storage,
                                        bool ShouldCreate) {
  if (Storage == Uniqued) {
    if (auto *N = getUniqued(Context.pImpl->RepoDefinitions,
                             RepoDefinitionInfo::KeyTy(Linkage, Visibility,
                                                       Pruned, Name, Digest)))
      return N;
    if (!ShouldCreate)
      return nullptr;
  } else {
    assert(ShouldCreate && "Expected non-uniqued nodes to always be created");
  }

  assert(isCanonical(Name) && "Expected canonical MDString");
  Metadata *Ops[] = {Name, Digest};
  return storeImpl(new (array_lengthof(Ops)) RepoDefinition(
                       Context, Storage, Linkage, Visibility, Pruned, Ops),
                   Storage, Context.pImpl->RepoDefinitions);
}

RepoDefinition *
RepoDefinition::getImpl(LLVMContext &Context, StringRef Name,
                        repodefinition::DigestType const &Digest,
                        GlobalValue::LinkageTypes Linkage,
                        GlobalValue::VisibilityTypes Visibility, bool Pruned,
                        StorageType Storage, bool ShouldCreate) {
  MDString *MDName = nullptr;
  if (!Name.empty())
    MDName = MDString::get(Context, Name);
  MDBuilder MDB(Context);
  const auto Size = repodefinition::DigestSize;
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

repodefinition::DigestType RepoDefinition::getDigest() const {
  ConstantAsMetadata const *C = getDigestAsMDConstant();
  auto const ArrayType = C->getType();
  auto const Elems = ArrayType->getArrayNumElements();
  repodefinition::DigestType D;

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
