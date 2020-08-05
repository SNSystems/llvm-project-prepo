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
#include "pstore/mcrepo/section.hpp"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallSite.h"
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

StringRef toStringRef(pstore::raw_sstring_view S) {
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

repodefinition::DigestType toDigestType(pstore::index::digest D) {
  repodefinition::DigestType Digest;
  support::endian::write64le(&Digest, D.low());
  support::endian::write64le(&(Digest.Bytes[8]), D.high());
  return Digest;
}

template <pstore::repo::section_kind Kind>
void collect(pstore::database const &Db, const pstore::repo::fragment &Fragment,
             StringSet<> &XFixupNames) {

  const auto *const Section = Fragment.atp<Kind>();
  for (pstore::repo::external_fixup const &Xfx : Section->xfixups()) {
    LLVM_DEBUG(dbgs() << "    Adding XFixup name: "
                      << pstore::indirect_string::read(Db, Xfx.name).to_string()
                      << '\n');
    XFixupNames.insert(
        std::move(pstore::indirect_string::read(Db, Xfx.name).to_string()));
  }
}

template <>
inline void collect<pstore::repo::section_kind::dependent>(
    pstore::database const & /*Db*/,
    const pstore::repo::fragment & /*Fragment*/,
    StringSet<> & /*XFixupNames*/) {}

static void collectFragmentXFixupNames(
    const pstore::database &Repository,
    std::shared_ptr<const pstore::repo::fragment> const &Fragment,
    StringSet<> &XFixupNames) {
  for (const pstore::repo::section_kind Kind : *Fragment) {
    assert(Fragment->has_section(Kind));

#define X(k)                                                                   \
  case pstore::repo::section_kind::k: {                                        \
    collect<pstore::repo::section_kind::k>(Repository, *Fragment,              \
                                           XFixupNames);                       \
    break;                                                                     \
  }

    switch (Kind) {
      PSTORE_MCREPO_SECTION_KINDS
    case pstore::repo::section_kind::last:
      llvm_unreachable("Bad section kind");
      break;
    }
#undef X
  }
}

/// Two kinds of pruning exist in the repository level and the module level.
/// The repository level pruning: if the global object is compiled before and
/// exists in the repository, it will be pruned. The module level pruning: in
/// the same compilation unit (module), some global objects have the same
/// digests. The RepoPruning pass could prune them as well.
///
/// This function is used to collect the GO's pruning status which include three
/// possible states: in the repository, in the ModuleFragments and not in both.
/// If GO is in the repository, collect the GO's xfixup names. If it is not in
/// repository and not in ModuleFragments, put it into the ModuleFragments.
///
/// \param Repository The database.
/// \param FragmentIndex A database fragment index.
/// \param ModuleFragments The collection of fragments that will appear in this
///        compilation but not include the pruned entries.
/// \param XFixupNames A set of xfixup's names.
/// \param PrunedDiscardableIfUnusedGos A set of GOs if GOs exist in the
///        repository and  may be discarded if it is not used.
/// \returns a tuple containing 1) true if the GO is in the existing repository.
///          2) true if the GO doesn't exist in the existing repository but is
///          in the ModuleFragments, and 3) the GO's digest.
static std::tuple<bool, bool, pstore::index::digest> hasExistingGO(
    const pstore::database &Repository,
    std::shared_ptr<const pstore::index::fragment_index> const &FragmentIndex,
    std::map<pstore::index::digest, const GlobalObject *> &ModuleFragments,
    GlobalObject &GO, StringSet<> &XFixupNames,
    DenseSet<GlobalObject *> &PrunedDiscardableIfUnusedGos) {
  auto const Result = repodefinition::get(&GO);
  assert(!Result.second && "The repo_definitio metadata should be created by "
                           "the RepoMetadataGeneration pass!");
  auto const Digest =
      pstore::index::digest{Result.first.high(), Result.first.low()};

  //Case 1: the GO is in the existing repository.
  if (FragmentIndex) {
    auto It = FragmentIndex->find(Repository, Digest);
    if (It != FragmentIndex->end(Repository)) {
      LLVM_DEBUG(dbgs() << "Existing GO (in repository) name: " << GO.getName()
                        << '\n');
      if (!GO.isDiscardableIfUnused()) {
        // Collect xfixup's names.
        auto Fragment = pstore::repo::fragment::load(Repository, It->second);
        collectFragmentXFixupNames(Repository, Fragment, XFixupNames);
      } else {
        PrunedDiscardableIfUnusedGos.insert(&GO);
      }
      return std::make_tuple(true, false, Digest);
    }
  }

  // Case 2: the GO is in the ModuleFragments.
  auto It = ModuleFragments.find(Digest);
  if (It != ModuleFragments.end() && !It->second->isDiscardableIfUnused()) {
    // The definition of some global objects may be discarded if not used.
    // If a global has been pruned and its digest matches a discardable GO,
    // a missing fragment error might be met during the assembler. To avoid
    // this issue, this global object is put into the ModuleFragments only if
    // it is not a discardable GO.
    LLVM_DEBUG(dbgs() << "Existing GO (in new fragments) name: " << GO.getName()
                      << '\n');
    return std::make_tuple(false, true, Digest);
  }

  LLVM_DEBUG(dbgs() << "Putting new GO (named: " << GO.getName()
                    << ") into ModuleFragments.\n");
  ModuleFragments.emplace(Digest, &GO);
  return std::make_tuple(false, false, Digest);
}

static void addDependentFragments(
    Module &M, StringSet<> &DependentFragments,
    std::shared_ptr<const pstore::index::fragment_index> const &Fragments,
    const pstore::database &Repository, StringSet<> &XFixupNames,
    pstore::index::digest const &Digest) {

  auto It = Fragments->find(Repository, Digest);
  assert(It != Fragments->end(Repository));
  // Load  the dependent fragments if existing in the repository.
  auto Fragment = pstore::repo::fragment::load(Repository, It->second);
  if (auto Dependents =
          Fragment->atp<pstore::repo::section_kind::dependent>()) {
    for (pstore::typed_address<pstore::repo::compilation_member> Dependent :
         *Dependents) {
      auto CM = pstore::repo::compilation_member::load(Repository, Dependent);
      StringRef MDName =
          toStringRef(pstore::get_sstring_view(Repository, CM->name).second);
      LLVM_DEBUG(dbgs() << "    Prunning dependent name: " << MDName << '\n');
      auto DMD = RepoDefinition::get(
          M.getContext(), MDName, toDigestType(CM->digest),
          toGVLinkage(CM->linkage()), toGVVisibility(CM->visibility()), true);
      // If functions 'A' and 'B' are dependent on function 'C', only add a
      // single RepoDefinition of 'C' to the 'repo.definitions' in order to
      // avoid multiple compilation_members of function 'C' in the compilation.
      if (DependentFragments.insert(MDName).second) {
        NamedMDNode *const NMD = M.getOrInsertNamedMetadata("repo.definitions");
        assert(NMD && "NamedMDNode cannot be NULL!");
        NMD->addOperand(DMD);
        // Collect the Dependent's XFixups.
        auto DependentIt = Fragments->find(Repository, CM->digest);
        assert(DependentIt != Fragments->end(Repository));
        // Load  the dependent fragments if existing in the repository.
        auto DependentFragment =
            pstore::repo::fragment::load(Repository, DependentIt->second);
        collectFragmentXFixupNames(Repository, DependentFragment, XFixupNames);
        // If function 'A' is dependent on function 'B' and 'B' is dependent on
        // function 'C', both RepoDefinition of 'B' and 'C' need to be added
        // into in the 'repo.definitions' during the pruning.
        addDependentFragments(M, DependentFragments, Fragments, Repository,
                              XFixupNames, CM->digest);
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

// Removed the global variables and its initializers.
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

// Removed the global object.
static void deleteGlobalObject(GlobalObject *GO) {
  if (auto *GV = dyn_cast<GlobalVariable>(GO)) {
    deleteGlobalVariable(GV);
  } else if (auto *Fn = dyn_cast<Function>(GO)) {
    deleteFunction(Fn);
  } else {
    llvm_unreachable("Unknown global object type!");
  }
}

static void pruning(GlobalObject &GO) {
  GO.setComdat(nullptr);
  GO.setDSOLocal(false);
  RepoDefinition *MD =
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
  StringSet<> DependentFragments; // Record all dependents.
  StringSet<> XFixupNames;        // Record all xfixups' name for pruned GOs.
  DenseSet<GlobalObject *> PrunedDiscardableIfUnusedGos;

  // Erase the unchanged global objects which is not discardable if unused.
  auto EraseUnchangedGlobalObject =
      [&ModuleFragments, &Fragments, &Repository, &M, &DependentFragments,
       &XFixupNames, &PrunedDiscardableIfUnusedGos](GlobalObject &GO) -> bool {
    if (GO.isDeclaration() || GO.hasAvailableExternallyLinkage() ||
        !isSafeToPrune(GO))
      return false;

    bool InRepository;
    bool InNewFragments;
    pstore::index::digest Key;
    std::tie(InRepository, InNewFragments, Key) =
        hasExistingGO(Repository, Fragments, ModuleFragments, GO, XFixupNames,
                      PrunedDiscardableIfUnusedGos);

    if (!InRepository && !InNewFragments) {
      return false;
    }

    if (InRepository && GO.isDiscardableIfUnused()) {
      return false;
    }

    if (InRepository) {
      addDependentFragments(M, DependentFragments, Fragments, Repository,
                            XFixupNames, Key);
    }

    pruning(GO);
    return true;
  };

  bool Changed = false;
  for (auto &GO : M.global_objects()) {
    if (EraseUnchangedGlobalObject(GO)) {
      Changed = true;
    }
  }

  auto EraseUnchangedDiscardableGlobalObject =
      [&XFixupNames, &Fragments, &Repository, &M,
       &DependentFragments](GlobalObject *GO) -> bool {
    if (XFixupNames.find(GO->getName()) != XFixupNames.end()) {
      LLVM_DEBUG(dbgs() << "Pruning GO name (DiscardableIfUnused): "
                        << GO->getName() << '\n');
      auto const Result = repodefinition::get(GO);
      auto const Digest =
          pstore::index::digest{Result.first.high(), Result.first.low()};

      addDependentFragments(M, DependentFragments, Fragments, Repository,
                            XFixupNames, Digest);

      auto It = Fragments->find(Repository, Digest);
      auto Fragment = pstore::repo::fragment::load(Repository, It->second);
      collectFragmentXFixupNames(Repository, Fragment, XFixupNames);

      pruning(*GO);
      return true;
    }
    return false;
  };

  bool IsPruned;
  do {
    IsPruned = false;
    // Go through discardable and pruned GOs.
    for (auto It = PrunedDiscardableIfUnusedGos.begin();
         It != PrunedDiscardableIfUnusedGos.end();) {
      auto CurIt = It;
      ++It;
      if (EraseUnchangedDiscardableGlobalObject(*CurIt)) {
        IsPruned = true;
        Changed = true;
        PrunedDiscardableIfUnusedGos.erase(CurIt);
      }
    }
  } while (IsPruned);

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
