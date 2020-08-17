//===- RepoDefinition.h - Program repository definition metadata structure-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//

#ifndef LLVM_IR_REPODEFINITION_H
#define LLVM_IR_REPODEFINITION_H

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MD5.h"

#include <map>
#include <tuple>

namespace pstore {
namespace repo {
struct compilation_member;
class dependents;
} // namespace repo
} // namespace pstore

namespace llvm {

/// Global object repo definition metadata description.
namespace repodefinition {
using DigestType = MD5::MD5Result;
static constexpr size_t DigestSize =
    std::tuple_size<decltype(DigestType::Bytes)>::value;
static constexpr DigestType NullDigest{std::array<uint8_t, DigestSize>{{0}}};
using GOVec = SmallVector<const GlobalObject *, 1>;
/// Map GO to a unique number in the function call graph.
using GOStateMap = llvm::DenseMap<const GlobalObject *, unsigned>;

const Constant *getAliasee(const GlobalAlias *GA);
/// Set repo definition metadata value and add this to the module level
/// metadata named repo.definitions.
void set(GlobalObject *GO, DigestType const &D);
/// Get global object digest metadata value.
/// \param GO The global object.
/// \return A pair of the global object's hash value and a bool which is true if
/// GO does not contain the RepoDefinition metadata.
std::pair<DigestType, bool> get(const GlobalObject *GO);

/// A structure of a global object (GO) information.
struct GOInfo {
  /// The InitialDigest is the hash of an object itself (a function or global
  /// variable) without consideration for any references to other objects.
  DigestType InitialDigest;
  /// The Contributions of an object (X) are those objects (Y) which
  /// transitively reference X and where a potential optimisation to either X or
  /// any of Y may invalidate both.
  GOVec Contributions;
  /// The Dependencies of an object are those objects which it transitively
  /// references but are not Contributions.
  GOVec Dependencies;

  GOInfo() = default;
  GOInfo(DigestType &&Digest, GOVec &&Contributions, GOVec &&Dependencies)
      : InitialDigest(std::move(Digest)),
        Contributions(std::move(Contributions)),
        Dependencies(std::move(Dependencies)) {}

#ifndef NDEBUG
  void dump() const {
    dbgs() << "GOInfo:";
    dbgs() << "\n\tInitial Digest: " << InitialDigest.digest();
    dbgs() << "\n\tContributions: [ ";
    bool first = true;
    for (const auto &Contribution : Contributions) {
      if (!first)
        dbgs() << ",";
      dbgs() << Contribution->getName();
      first = false;
    }
    dbgs() << "]";
    dbgs() << "\n\tDependencies: [ ";
    first = true;
    for (const auto &Dependent : Dependencies) {
      if (!first)
        dbgs() << ",";
      dbgs() << Dependent->getName();
      first = false;
    }
    dbgs() << "]\n";
  }
#endif
};
using GOInfoMap = DenseMap<const GlobalObject *, GOInfo>;
using MemoizedHashes = llvm::DenseMap<const GlobalObject *, DigestType>;

/// A structure of a global object (GO) state.
struct GODigestState {
  /// A reference to an object's contributions. The GOInfo struct takes
  /// ownership of it.
  const GOVec &Contributions;
  /// A reference to an object's dependencies. The GOInfo struct takes ownership
  /// of it.
  const GOVec &Dependencies;
  GODigestState() = delete;
  GODigestState(const GOVec &Contributions, const GOVec &Dependencies)
      : Contributions(Contributions), Dependencies(Dependencies) {}
};

/// Helper Class for generating the hash for each global objects on the Module.
/// Also caches hashes for previously generated global objects (GOs).
class ModuleHashGenerator {

  enum class Tags : char {
    Backref = 'R',
    End = 'E',
    GO = 'T',
  };

  /// A map of GO to a unique number in the dependencies graph.
  GOStateMap Visited;
  /// Used to keep the GO information.
  GOInfoMap GOIMap;
  /// Memoize GO's hash if one of the following conditions is satisfied:
  /// 1. GO is not in a cycle/loop.
  /// 2. GO is in a self-loop but not if there is a loop involving another
  /// object.
  MemoizedHashes GOHashCache;
  /// True if the module M is changed.
  bool Changed = false;

public:
  ModuleHashGenerator() {}
  /// calculate the digest for all global objects inside of the Module M.
  void digestModule(Module &M);

  /// Compute the hash value for the given global object GO.
  /// \param GO The global object.
  /// \param UseRepoDefinitionMD true if using the digest of GO's dependencies
  ///        stored in the RepoDefinition metadata.
  /// \return The global object's digest value.
  DigestType calculateDigest(const GlobalObject *GO, bool UseRepoDefinitionMD);

  bool isChanged() const { return Changed; }

  const GOInfoMap &getGOInfoMap() const { return GOIMap; }

  const MemoizedHashes &getGOHashCache() const { return GOHashCache; }

private:
  /// Get global object hash value from memoized hashes.
  /// \param GO The global object.
  /// \return A tuple of global object's hash value and a bool which is true if
  /// GO's hash is cached.
  std::tuple<DigestType, bool> getGOHash(const GlobalObject *GO) const {
    const auto Iter = GOHashCache.find_as(GO);
    return (Iter != GOHashCache.end()) ? std::make_tuple(Iter->second, true)
                                       : std::make_tuple(NullDigest, false);
  }

  /// Construct the global object information map (GOIMap) and calculate the
  /// number of hashed functions and variables inside of the Module M.
  void calculateGONumAndGOIMap(const Module &M);

  /// Calculate the initial hash value, dependencies and contributions for the
  /// global object 'G' and store these informaion in the GOImap.
  /// \param G Calculated global object.
  template <typename GlobalType> void calculateGOInfo(const GlobalType *G);
  void calculateGOInfo(const GlobalObject *GO);

  /// Accumulate the GO's digest and get its GODigestState.
  /// \param GO Calculated global object.
  /// \param GOHash A object that is used to calculate the GO's hash value.
  /// \param UseRepoDefinitionMD true if using the digest of GO's dependencies
  ///        stored in the RepoDefinition metadata.
  /// \returns a structure containing the GO's state.
  GODigestState accumulateGODigest(const GlobalObject *GO, MD5 &GOHash,
                                   bool UseRepoDefinitionMD);

  /// Add the initial digest of each contribution global object to GOHash.
  void updateDigestUseContributions(MD5 &GOHash, const GOVec &Contributions);

  /// Add the final digest of each dependencies global object to GOHash.
  std::tuple<size_t, DigestType>
  updateDigestUseDependencies(const GlobalObject *GO, MD5 &GOHash,
                              unsigned GODepth, const GOVec &Dependencies,
                              bool UseRepoDefinitionMD);

  /// Update the digest of an invidual GO incorporating the hashes of all its
  /// dependencies and contributions.
  /// \param GO The global object whose digest is to be computed.
  /// \param UseRepoDefinitionMD true if using the digest of GO's dependencies
  ///        stored in the RepoDefinition metadata.
  /// \returns a tuple containing 1) the depth of the path that we've traversed
  ///          (or max if it reaches the end of the path). and 2) the GO's
  ///         digest.
  std::tuple<size_t, DigestType>
  updateDigestUseDependenciesAndContributions(const GlobalObject *GO,
                                              bool UseRepoDefinitionMD);
};

/// Compute the hash value and set the RepoDefinition metadata for all global
/// objects inside of the Module M.
/// \param M Called module.
/// \return a bool which is true if the module M has been changed.
bool generateRepoDefinitions(Module &M);

/// Compute the hash value for the given global object GO.
/// \param GO The global object.
/// \return The global object's digest value.
DigestType calculateDigest(const GlobalObject *GO);
} // namespace repodefinition

/// Global object repo definition.
class RepoDefinition : public MDNode {
  friend class LLVMContextImpl;
  friend class MDNode;

  template <unsigned Bitwidth, typename Type> struct CheckType {
    using LT = typename std::underlying_type<Type>::type;
    using ULT = typename std::make_unsigned<LT>::type;
    template <typename U> static constexpr U max() {
      return std::numeric_limits<U>::max();
    }
    static bool isSafeCast(Type Value) {
      return (std::is_unsigned<LT>::value ? true : max<LT>() > 0) &&
             max<ULT>() <= max<unsigned>() &&
             static_cast<unsigned>(Value) <= (1u << Bitwidth);
    }
  };

  struct Data32 {
    static unsigned combine(GlobalValue::VisibilityTypes high,
                            GlobalValue::LinkageTypes low) {
      return static_cast<unsigned>(high) << 16 | static_cast<unsigned>(low);
    }

    static GlobalValue::VisibilityTypes visibility(unsigned combined) {
      return static_cast<GlobalValue::VisibilityTypes>(combined >> 16);
    }

    static GlobalValue::LinkageTypes linkage(unsigned combined) {
      return static_cast<GlobalValue::LinkageTypes>(
          combined & std::numeric_limits<uint16_t>::max());
    }
  };

  RepoDefinition(LLVMContext &C, StorageType Storage,
                 GlobalValue::LinkageTypes Linkage,
                 GlobalValue::VisibilityTypes Visibility, bool Pruned,
                 ArrayRef<Metadata *> MDs)
      : MDNode(C, RepoDefinitionKind, Storage, MDs) {
    assert(MDs.size() == 2 && "Expected a hash and name.");
    assert((CheckType<4, GlobalValue::LinkageTypes>::isSafeCast(Linkage)) &&
           "Linkage type will overflow!");
    assert(
        (CheckType<2, GlobalValue::VisibilityTypes>::isSafeCast(Visibility)) &&
        "Visibility type will overflow!");
    SubclassData32 = Data32::combine(Visibility, Linkage);
    SubclassData16 = static_cast<unsigned short>(Pruned);
  }
  ~RepoDefinition() { dropAllReferences(); }

  static RepoDefinition *getImpl(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 GlobalValue::VisibilityTypes Visibility,
                                 bool Pruned, StorageType Storage,
                                 bool ShouldCreate = true);

  static RepoDefinition *getImpl(LLVMContext &Context, StringRef Name,
                                 repodefinition::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 GlobalValue::VisibilityTypes Visibility,
                                 bool Pruned, StorageType Storage,
                                 bool ShouldCreate = true);

  TempRepoDefinition cloneImpl() const {
    // Get the raw name/hash since it is possible to invoke this on
    // a RepoDefinition containing temporary metadata.
    return getTemporary(getContext(), getNameAsString(), getDigest(),
                        getLinkage(), getVisibility(), getPruned());
  }

public:
  static RepoDefinition *get(LLVMContext &Context, MDString *Name,
                             ConstantAsMetadata *GVHash,
                             GlobalValue::LinkageTypes Linkage,
                             GlobalValue::VisibilityTypes Visibility,
                             bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned, Uniqued);
  }

  static RepoDefinition *get(LLVMContext &Context, StringRef Name,
                             repodefinition::DigestType const &Digest,
                             GlobalValue::LinkageTypes Linkage,
                             GlobalValue::VisibilityTypes Visibility,
                             bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Visibility, Pruned, Uniqued);
  }

  static RepoDefinition *getIfExists(LLVMContext &Context, MDString *Name,
                                     ConstantAsMetadata *GVHash,
                                     GlobalValue::LinkageTypes Linkage,
                                     GlobalValue::VisibilityTypes Visibility,
                                     bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned, Uniqued,
                   /* ShouldCreate */ false);
  }

  static RepoDefinition *getIfExists(LLVMContext &Context, StringRef Name,
                                     repodefinition::DigestType const &Digest,
                                     GlobalValue::LinkageTypes Linkage,
                                     GlobalValue::VisibilityTypes Visibility,
                                     bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Visibility, Pruned, Uniqued,
                   /* ShouldCreate */ false);
  }

  static RepoDefinition *getDistinct(LLVMContext &Context, MDString *Name,
                                     ConstantAsMetadata *GVHash,
                                     GlobalValue::LinkageTypes Linkage,
                                     GlobalValue::VisibilityTypes Visibility,
                                     bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned,
                   Distinct);
  }

  static RepoDefinition *getDistinct(LLVMContext &Context, StringRef Name,
                                     repodefinition::DigestType const &Digest,
                                     GlobalValue::LinkageTypes Linkage,
                                     GlobalValue::VisibilityTypes Visibility,
                                     bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Visibility, Pruned,
                   Distinct);
  }

  static TempRepoDefinition
  getTemporary(LLVMContext &Context, MDString *Name, ConstantAsMetadata *GVHash,
               GlobalValue::LinkageTypes Linkage,
               GlobalValue::VisibilityTypes Visibility, bool Pruned) {
    return TempRepoDefinition(
        getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned, Temporary));
  }

  static TempRepoDefinition
  getTemporary(LLVMContext &Context, StringRef Name,
               repodefinition::DigestType const &Digest,
               GlobalValue::LinkageTypes Linkage,
               GlobalValue::VisibilityTypes Visibility, bool Pruned) {
    return TempRepoDefinition(
        getImpl(Context, Name, Digest, Linkage, Visibility, Pruned, Temporary));
  }

  /// Return a (temporary) clone of this.
  TempRepoDefinition clone() const { return cloneImpl(); }

  GlobalValue::LinkageTypes getLinkage() const {
    return Data32::linkage(SubclassData32);
  }

  GlobalValue::VisibilityTypes getVisibility() const {
    return Data32::visibility(SubclassData32);
  }

  bool getPruned() const { return static_cast<bool>(SubclassData16); }
  void setPruned(bool Value) {
    SubclassData16 = static_cast<unsigned short>(Value);
  }

  Metadata *getNameAsMD() const { return getOperand(0); }
  MDString *getNameAsMDString() const { return cast<MDString>(getNameAsMD()); }
  StringRef getNameAsString() const { return getNameAsMDString()->getString(); }

  Metadata *getDigestAsMD() const { return getOperand(1); }
  ConstantAsMetadata *getDigestAsMDConstant() const {
    return cast<ConstantAsMetadata>(getDigestAsMD());
  }
  repodefinition::DigestType getDigest() const;

  static bool classof(const Metadata *MD) {
    return MD->getMetadataID() == RepoDefinitionKind;
  }

  // A pointer to the corresponding compilation member in the repository. It is
  // updated when the compilation member is generated by the object writer.
  const pstore::repo::compilation_member *CorrespondingCompilationMember =
      nullptr;
};

} // end namespace llvm

#endif // LLVM_IR_REPODEFINITION_H
