//===----  RepoPruning.cpp - Program repository pruning  ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/RepoDefinition.h"
#include "llvm/IR/RepoGlobals.h"
#include "llvm/IR/RepoHashCalculator.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/GlobalStatus.h"
#include <algorithm>
#include <iostream>
#include <map>
using namespace llvm;

#define DEBUG_TYPE "prepo-pruning"

STATISTIC(NumFunctions, "Number of functions removed");
STATISTIC(NumVariables, "Number of variables removed");
STATISTIC(NumAliases, "Number of aliases removed");

namespace {

/// RepoPruning removes the redundant global objects.
class RepoPruning : public ModulePass {
public:
  static char ID;
  RepoPruning() : ModulePass(ID) {
    initializeRepoPruningPass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override { return "RepoPruningPass"; }

  bool runOnModule(Module &M) override;
};

} // end anonymous namespace

char RepoPruning::ID = 0;
INITIALIZE_PASS(RepoPruning, "prepo-pruning",
                "Program Repository Global Object Pruning", false, false)

ModulePass *llvm::createRepoPruningPass() { return new RepoPruning(); }

GlobalValue::LinkageTypes toGVLinkage(pstore::repo::linkage L) {
  switch (L) {
  case pstore::repo::linkage::external:
    return GlobalValue::ExternalLinkage;
  case pstore::repo::linkage::link_once_any:
    return GlobalValue::LinkOnceAnyLinkage;
  case pstore::repo::linkage::link_once_odr:
    return GlobalValue::LinkOnceODRLinkage;
  case pstore::repo::linkage::weak_any:
    return GlobalValue::WeakAnyLinkage;
  case pstore::repo::linkage::weak_odr:
    return GlobalValue::WeakODRLinkage;
  case pstore::repo::linkage::internal_no_symbol:
    return GlobalValue::PrivateLinkage;
  case pstore::repo::linkage::internal:
    return GlobalValue::InternalLinkage;
  case pstore::repo::linkage::common:
    return GlobalValue::CommonLinkage;
  case pstore::repo::linkage::append:
    return GlobalValue::AppendingLinkage;
  }
  llvm_unreachable("Unsupported linkage type");
}

GlobalValue::VisibilityTypes toGVVisibility(pstore::repo::visibility V) {
  switch (V) {
  case pstore::repo::visibility::default_vis:
    return GlobalValue::VisibilityTypes::DefaultVisibility;
  case pstore::repo::visibility::hidden_vis:
    return GlobalValue::VisibilityTypes::HiddenVisibility;
  case pstore::repo::visibility::protected_vis:
    return GlobalValue::VisibilityTypes::ProtectedVisibility;
  }
  llvm_unreachable("Unsupported visibility type");
}

static StringRef toStringRef(pstore::raw_sstring_view S) {
  return {S.data(), S.size()};
}

static bool isIntrinsicGV(const GlobalObject &GO) {
  return GO.getName().startswith("llvm.");
}

static bool isSafeToPruneIntrinsicGV(const GlobalObject &GO) {
  const llvm::StringRef Name = GO.getName();
  bool Result = Name == "llvm.global_ctors" || Name == "llvm.global_dtors";
  assert(!Result || isIntrinsicGV(GO));
  return Result;
}

static bool isSafeToPrune(const GlobalObject &GO) {
  return !isIntrinsicGV(GO) || isSafeToPruneIntrinsicGV(GO);
}

static repodefinition::DigestType toDigestType(pstore::index::digest D) {
  repodefinition::DigestType Digest;
  support::endian::write64le(&Digest, D.low());
  support::endian::write64le(&(Digest.Bytes[8]), D.high());
  return Digest;
}

/// We have two kinds of pruning: repository level and module level.
/// The repository level pruning: if the global object is compiled before and
/// exists in the repository, it will be pruned.
///
/// \param Repository The database.
/// \param FragmentIndex A database fragment index.
/// \param Key The GO's digest.
/// \param GO A global object.
/// \returns true if the GO is in the existing repository.
static bool hasExistingGOInRepository(
    const pstore::database &Repository,
    const std::shared_ptr<const pstore::index::fragment_index> &FragmentIndex,
    const pstore::index::digest &Key, const GlobalObject &GO) {
  if (FragmentIndex &&
      FragmentIndex->find(Repository, Key) != FragmentIndex->end(Repository)) {
    LLVM_DEBUG(dbgs() << "Existing GO (in repository) name: " << GO.getName()
                      << '\n');
    return true;
  }
  return false;
}

/// The module level pruning : in the same compilation unit (module), some
/// global objects have the same digests. The RepoPruning pass could prune them
/// as well.
///
/// \param ModuleFragments The collection of fragments that will appear in this
///        compilation but not including the pruned entries.
/// \param Key The GO's digest
/// \param GO A global object.
/// \returns true if the GO exists in the ModuleFragments.
static bool hasExistingGOInModule(
    const std::map<pstore::index::digest, const GlobalObject *>
        &ModuleFragments,
    const pstore::index::digest &Key, const GlobalObject &GO) {
  auto It = ModuleFragments.find(Key);
  if (It != ModuleFragments.end() && !It->second->isDiscardableIfUnused()) {
    // The definition of some global objects may be discarded if not used.
    // If a global has been pruned and its digest matches a discardable GO,
    // a missing fragment error might be met during the assembler. To avoid
    // this issue, this global object is put into the ModuleFragments only if
    // it is not a discardable GO.
    LLVM_DEBUG(dbgs() << "Existing GO (in new fragments) name: " << GO.getName()
                      << '\n');
    return true;
  }
  return false;
}

static void addLinkedDefinitions(
    Module &M, StringSet<> &LinkedNames,
    std::shared_ptr<const pstore::index::fragment_index> const &Fragments,
    const pstore::database &Repository, const pstore::index::digest Digest) {

  const auto Pos = Fragments->find(Repository, Digest);
  assert(Pos != Fragments->end(Repository) &&
         "Digest was not found in the Repository");

  // Create the linked RepoDefinition if it exists in the repository.
  auto Fragment = pstore::repo::fragment::load(Repository, Pos->second);
  if (auto LinkedDefinitions =
          Fragment->atp<pstore::repo::section_kind::linked_definitions>()) {
    NamedMDNode *const RepoDefinitions =
        M.getOrInsertNamedMetadata("repo.definitions");
    assert(RepoDefinitions && "RepoDefinitions cannot be NULL!");

    for (pstore::repo::linked_definitions::value_type const &LinkedDefinition :
         *LinkedDefinitions) {
      const auto Definition = pstore::repo::compilation_member::load(
          Repository, LinkedDefinition.pointer);
      pstore::shared_sstring_view Owner;
      const auto MDName =
          toStringRef(get_sstring_view(Repository, Definition->name, &Owner));
      LLVM_DEBUG(dbgs() << "    Adding linked-definition name: " << MDName
                        << '\n');

      // If fragments A and B are both linked to fragment C, ensure that C
      // appears in 'repo.definitions' only once to avoid multiple instances of
      // definition C in the compilation.
      if (LinkedNames.insert(MDName).second) {
        RepoDefinitions->addOperand(RepoDefinition::get(
            M.getContext(), MDName, toDigestType(Definition->digest),
            toGVLinkage(Definition->linkage()),
            toGVVisibility(Definition->visibility()), true));

        // If fragment A is has a linked_definition referencing fragment B and B
        // is linked to C, the RepoDefinition for both 'B' and 'C' needs to be
        // added to 'repo.definitions'.
        addLinkedDefinitions(M, LinkedNames, Fragments, Repository,
                             Definition->digest);
      }
    }
  }
}

// Remove the function body and make it external.
static void deleteFunction(Function *Fn) {
  // This will set the linkage to external
  Fn->deleteBody();
  Fn->removeDeadConstantUsers();
  NumFunctions++;
}

// Remove the global variables and its initializers.
static void deleteGlobalVariable(GlobalVariable *GV) {
  if (GV->hasInitializer()) {
    Constant *Init = GV->getInitializer();
    GV->setInitializer(nullptr);
    if (isSafeToDestroyConstant(Init))
      Init->destroyConstant();
  }
  GV->removeDeadConstantUsers();
  GV->setLinkage(GlobalValue::ExternalLinkage);
  NumVariables++;
}

// Remove the global object.
static void deleteGlobalObject(GlobalObject *GO) {
  if (auto *GV = dyn_cast<GlobalVariable>(GO)) {
    deleteGlobalVariable(GV);
  } else if (auto *Fn = dyn_cast<Function>(GO)) {
    deleteFunction(Fn);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
}

/// Remove or prune the global object GO.
/// If 1) GO has an empty use list or 2) it is an intrinsic global variable and
/// is safe to remove, remove the GO and make it external. Otherwise, it is
/// pruned by setting the pruned field of the RepoDefinition to true and
/// changing the GO's linkage type to available_externally.
///
/// \param GO A global object.
static void removeOrPrune(GlobalObject &GO) {
  GO.setComdat(nullptr);
  GO.setDSOLocal(false);
  RepoDefinition *const MD =
      dyn_cast<RepoDefinition>(GO.getMetadata(LLVMContext::MD_repo_definition));
  MD->setPruned(true);

  if (isSafeToPruneIntrinsicGV(GO) || GO.use_empty()) {
    deleteGlobalObject(&GO);
    return;
  }
  GO.setLinkage(GlobalValue::AvailableExternallyLinkage);
}

static bool doPruning(Module &M) {
  MDBuilder MDB(M.getContext());
  const pstore::database &Repository = getRepoDatabase();

  std::shared_ptr<const pstore::index::fragment_index> const Fragments =
      pstore::index::get_index<pstore::trailer::indices::fragment>(Repository,
                                                                   false);
  if (!Fragments && !M.getNamedMetadata("repo.definitions")) {
    return false;
  }

  std::map<pstore::index::digest, const GlobalObject *> ModuleFragments;
  StringSet<> LinkedDefinitions; // Record all linked definitions.

  // Erase the unchanged global objects.
  auto EraseUnchangedGlobalObject =
      [&ModuleFragments, &Fragments, &Repository, &M,
       &LinkedDefinitions](GlobalObject &GO) -> bool {
    if (GO.isDeclaration() || GO.hasAvailableExternallyLinkage() ||
        !isSafeToPrune(GO))
      return false;

    auto const DigestStatus = repodefinition::get(&GO);
    assert(!DigestStatus.second &&
           "The repo_definition metadata should be created by "
           "the RepoMetadataGeneration pass!");
    auto const Key = pstore::index::digest{DigestStatus.first.high(),
                                           DigestStatus.first.low()};

    if (hasExistingGOInRepository(Repository, Fragments, Key, GO)) {
      addLinkedDefinitions(M, LinkedDefinitions, Fragments, Repository, Key);
      removeOrPrune(GO);
      return true;
    }

    if (hasExistingGOInModule(ModuleFragments, Key, GO)) {
      removeOrPrune(GO);
      return true;
    }

    // If GO is not in repository and not in ModuleFragments, put it into the
    // ModuleFragments.
    LLVM_DEBUG(dbgs() << "Putting new GO (named: " << GO.getName()
                      << ") into ModuleFragments.\n");
    ModuleFragments.emplace(Key, &GO);
    return false;
  };

  bool Changed = false;
  for (auto &GO : M.global_objects()) {
    if (EraseUnchangedGlobalObject(GO)) {
      Changed = true;
    }
  }

  return Changed;
}

static bool wasPruned(const GlobalObject &GO) {
  if (const MDNode *const MD =
          GO.getMetadata(LLVMContext::MD_repo_definition)) {
    if (const RepoDefinition *const RD = dyn_cast<RepoDefinition>(MD)) {
      return RD->getPruned();
    }
  }
  return false;
}

static bool eliminateUnreferencedAndPrunedGOs(Module &M) {
  bool Changed = false;
  // If a global object is pruned (already in the database) and are not
  // referenced by other live global objects, it can be deleted.
  for (auto &GO : M.global_objects()) {
    if (GO.isDeclaration() || !GO.use_empty() || !wasPruned(GO))
      continue;
    deleteGlobalObject(&GO);
    Changed = true;
  }
  return Changed;
}

static llvm::Value *createDeclaration(Module &M, const GlobalAlias *GA) {
  Type *Ty = GA->getValueType();
  if (FunctionType *FTy = dyn_cast<FunctionType>(Ty))
    return Function::Create(FTy, GlobalValue::ExternalLinkage,
                            GA->getAddressSpace(), GA->getName(), &M);
  return new GlobalVariable(M, Ty, false, GlobalValue::ExternalLinkage, nullptr,
                            GA->getName());
}

bool RepoPruning::runOnModule(Module &M) {
  assert(Triple(M.getTargetTriple()).isOSBinFormatRepo() &&
         "This pass should be only run on the Repo target");

  if (skipModule(M))
    return false;

  bool IsPruned = doPruning(M);

  // If a global object is pruned (already in the database) and has no live
  // callers, it can be deleted rather than being marked available_extern
  // since nothing will attempt to use it for code generation.
  if (IsPruned) {
    bool IsEliminated = false;
    do {
      IsEliminated = eliminateUnreferencedAndPrunedGOs(M);
    } while (IsEliminated);
  }

  // Enable this pass support for alias.
  // First, check whether a GlobalAlias references a pruned GO. If yes, record
  // it. Second, replace all recorded aliases by the declaration. These two
  // steps can't be merged since the second step could change the base object of
  // the alias. For example:
  // Assuming: 1) the aliasee of alias 'A' is alias 'B',
  //           2) the aliasee of alias 'B' is global object 'C',
  //           3) the global object 'C' is pruned.
  // If merging two steps, the order in the alias list is 'B' and then 'A'.
  // Since the global object 'C' is pruned, the alias 'B' is changed from
  // definition to declaration. Then, since 'B' is changed to declaration, the
  // base object of the alias 'A' will be changed from 'C' to 'B'. 'A' will not
  // be removed because 'B' isn't a global object. However, 'A' must point to a
  // definition but 'B' is not a definition. This will result in the backend
  // error.
  // Step 1. Record aliases.
  SmallVector<GlobalAlias *, 16> Aliases;
  for (GlobalAlias &A : M.aliases()) {
    // TODO: An Alias doesn't have an aliasee, for example:
    // @v = alias i32, inttoptr(i32 42 to i32*)
    if (auto *BaseObject = A.getBaseObject()) {
      if (wasPruned(*BaseObject)) {
        LLVM_DEBUG(dbgs() << "Alias: " << A.getName() << ", its base object: "
                          << BaseObject->getName() << " is pruned! \n");
        Aliases.push_back(&A);
        ++NumAliases;
      }
    }
  }
  // 2. Resolve aliases.
  for (GlobalAlias *A : Aliases) {
    // Remove the alias from Module. If not removed, the clone name of the alias
    // will be created in the `createDeclaration` function. This might result in
    // a linker error.
    A->removeFromParent();
    // Replace the alias by a declaration.
    A->replaceAllUsesWith(createDeclaration(M, A));
    delete A;
  }

  return IsPruned;
}
