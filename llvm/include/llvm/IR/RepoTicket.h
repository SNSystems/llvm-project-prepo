//===- RepoTicket.h - Program repository digest data structure. -*- C++ --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//

#ifndef LLVM_IR_REPO_TICKET_H
#define LLVM_IR_REPO_TICKET_H

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

struct Ticket;

/// Global value ticket metadata description.
namespace ticketmd {
using DigestType = MD5::MD5Result;
static constexpr size_t DigestSize =
    std::tuple_size<decltype(DigestType::Bytes)>::value;
static constexpr ticketmd::DigestType NullDigest{
    std::array<uint8_t, DigestSize>{{0}}};
using GOVec = SmallVector<const GlobalObject *, 1>;
/// Map GO to a unique number in the function call graph.
using GOStateMap = llvm::DenseMap<const GlobalObject *, unsigned>;
/// Memoize GO's hash if one of the following conditions is satisfied:
/// 1. GO is not in a cycle/loop.
/// 2. GO is in a self-loop but not if there is a loop involving another object.
using MemoizedHashes = llvm::DenseMap<const GlobalObject *, DigestType>;

const Constant *getAliasee(const GlobalAlias *GA);
/// Set global object ticket metadata value and add this to the module level
/// metadata named repo.tickets.
void set(GlobalObject *GO, DigestType const &D);
/// Get global object digest metadata value.
/// \param GO The global object.
/// \return A pair of the global object's hash value and a bool which is true if
/// GO does not contain the ticket metadata.
std::pair<DigestType, bool> get(const GlobalObject *GO);

enum class Tags : char {
  Backref = 'R',
  End = 'E',
  GO = 'T',
};

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

/// Helper Class for generating the hash for each global objects on the Module
/// and it also represents how a global object (GO) is mapped into its cached
/// hash.
class HashCache {
  /// A map of GO to a unique number in the dependencies graph.
  GOStateMap Visited;
  /// Used to keep the GO information.
  GOInfoMap GOIMap;
  // Used to record memoized hashes.
  MemoizedHashes GOHashCache;
  /// True if the module M is changed.
  bool Changed;

public:
  HashCache() : Changed(false) {}
  /// calculate the GO's hash and construct a memoized hashes for module.
  HashCache(Module &M);

  /// Compute the hash value for the given global object GO.
  /// \param GO The global object.
  /// \return The global object's digest value.
  DigestType calculateDigest(const GlobalObject *GO);

  bool isChanged() const { return Changed; }

  const GOInfoMap &getGOInfoMap() const { return GOIMap; }

private:
  /// Get global object hash value from memoized hashes.
  /// \param GO The global object.
  /// \return A tuple of global object's hash value and  a bool which is true if
  /// GO's hash is cached.
  std::tuple<DigestType, bool> get(const GlobalObject *GO) const {
    const auto Iter = GOHashCache.find_as(GO);
    return (Iter != GOHashCache.end()) ? std::make_tuple(Iter->second, true)
                                       : std::make_tuple(NullDigest, false);
  }

  /// Construct the global object information map (GOIMap) and calculate the
  /// number of hashed functions and variable  inside of the Module M.
  void calculateGONumAndGOIMap(const Module &M);

  /// Compute the hash value for the given global object GO.
  /// \param GO The global object.
  /// \param Final (document this)
  /// \return The global object's digest value.
  DigestType calculateDigest(const GlobalObject *GO, bool Final);

  /// Calculate the initial hash value, dependencies and contributions for the
  /// global object 'G' and store these informaion in the GOImap.
  /// \param G Calculated global object.
  template <typename GlobalType> void calculateGOInfo(const GlobalType *G);
  void calculateGOInfo(const GlobalObject *GO);

  /// Accumulate the GO's digest and get its GODigestState.
  GODigestState accumulateGODigest(const GlobalObject *GO, MD5 &GOHash,
                                   bool Final);

  /// Loop through the contributions and add the initial digest of each global
  /// object inside of the contributions to GOHash.
  void updateDigestUseContributions(MD5 &GOHash, const GOVec &Contributions);

  /// Loop through the dependences and add the final digest of each global
  /// object inside of the dependences to GOHash.
  std::tuple<size_t, DigestType>
  updateDigestUseDependencies(const GlobalObject *GO, MD5 &GOHash,
                              unsigned GOVisitedIndex,
                              const GOVec &Dependencies, bool Final);

  /// Update the digest of an invidual GO incorporating the hashes of all its
  /// dependencies and contributions.
  /// \param GO The global object whose digest is to be computed.
  /// \param Final (doc here)
  /// \returns a tuple containing the state information which includes
  /// 1) a numerical identifier which will be used if GO loops back to itself in
  /// future and 2) the GO's digest.
  std::tuple<size_t, DigestType>
  updateDigestUseDependenciesAndContributions(const GlobalObject *GO,
                                              bool Final);
};

/// Compute the hash value and set the ticket metadata for all global objects
/// inside of the Module M.
/// \param M Called module.
/// \return a bool which is true if the module M has been changed.
bool generateTicketMDs(Module &M);

/// Compute the hash value for the given global object GO.
/// \param GO The global object.
/// \return The global object's digest value.
DigestType calculateDigest(const GlobalObject *GO);
} // namespace ticketmd

/// Global value ticket node description.
class TicketNode : public MDNode {
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

  TicketNode(LLVMContext &C, StorageType Storage,
             GlobalValue::LinkageTypes Linkage,
             GlobalValue::VisibilityTypes Visibility, bool Pruned,
             ArrayRef<Metadata *> MDs)
      : MDNode(C, TicketNodeKind, Storage, MDs) {
    assert(MDs.size() == 2 && "Expected a hash and name.");
    assert((CheckType<4, GlobalValue::LinkageTypes>::isSafeCast(Linkage)) &&
           "Linkage type will overflow!");
    assert(
        (CheckType<2, GlobalValue::VisibilityTypes>::isSafeCast(Visibility)) &&
        "Visibility type will overflow!");
    SubclassData32 = Data32::combine(Visibility, Linkage);
    SubclassData16 = static_cast<unsigned short>(Pruned);
  }
  ~TicketNode() { dropAllReferences(); }

  static TicketNode *getImpl(LLVMContext &Context, MDString *Name,
                             ConstantAsMetadata *GVHash,
                             GlobalValue::LinkageTypes Linkage,
                             GlobalValue::VisibilityTypes Visibility,
                             bool Pruned, StorageType Storage,
                             bool ShouldCreate = true);

  static TicketNode *getImpl(LLVMContext &Context, StringRef Name,
                             ticketmd::DigestType const &Digest,
                             GlobalValue::LinkageTypes Linkage,
                             GlobalValue::VisibilityTypes Visibility,
                             bool Pruned, StorageType Storage,
                             bool ShouldCreate = true);

  TempTicketNode cloneImpl() const {
    // Get the raw name/hash since it is possible to invoke this on
    // a TicketNode containing temporary metadata.
    return getTemporary(getContext(), getNameAsString(), getDigest(),
                        getLinkage(), getVisibility(), getPruned());
  }

public:
  static TicketNode *get(LLVMContext &Context, MDString *Name,
                         ConstantAsMetadata *GVHash,
                         GlobalValue::LinkageTypes Linkage,
                         GlobalValue::VisibilityTypes Visibility, bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned, Uniqued);
  }

  static TicketNode *get(LLVMContext &Context, StringRef Name,
                         ticketmd::DigestType const &Digest,
                         GlobalValue::LinkageTypes Linkage,
                         GlobalValue::VisibilityTypes Visibility, bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Visibility, Pruned, Uniqued);
  }

  static TicketNode *getIfExists(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 GlobalValue::VisibilityTypes Visibility,
                                 bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getIfExists(LLVMContext &Context, StringRef Name,
                                 ticketmd::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 GlobalValue::VisibilityTypes Visibility,
                                 bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Visibility, Pruned, Uniqued,
                   /* ShouldCreate */ false);
  }

  static TicketNode *getDistinct(LLVMContext &Context, MDString *Name,
                                 ConstantAsMetadata *GVHash,
                                 GlobalValue::LinkageTypes Linkage,
                                 GlobalValue::VisibilityTypes Visibility,
                                 bool Pruned) {
    return getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned,
                   Distinct);
  }

  static TicketNode *getDistinct(LLVMContext &Context, StringRef Name,
                                 ticketmd::DigestType const &Digest,
                                 GlobalValue::LinkageTypes Linkage,
                                 GlobalValue::VisibilityTypes Visibility,
                                 bool Pruned) {
    return getImpl(Context, Name, Digest, Linkage, Visibility, Pruned,
                   Distinct);
  }

  static TempTicketNode getTemporary(LLVMContext &Context, MDString *Name,
                                     ConstantAsMetadata *GVHash,
                                     GlobalValue::LinkageTypes Linkage,
                                     GlobalValue::VisibilityTypes Visibility,
                                     bool Pruned) {
    return TempTicketNode(
        getImpl(Context, Name, GVHash, Linkage, Visibility, Pruned, Temporary));
  }

  static TempTicketNode getTemporary(LLVMContext &Context, StringRef Name,
                                     ticketmd::DigestType const &Digest,
                                     GlobalValue::LinkageTypes Linkage,
                                     GlobalValue::VisibilityTypes Visibility,
                                     bool Pruned) {
    return TempTicketNode(
        getImpl(Context, Name, Digest, Linkage, Visibility, Pruned, Temporary));
  }

  /// Return a (temporary) clone of this.
  TempTicketNode clone() const { return cloneImpl(); }

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
  ticketmd::DigestType getDigest() const;

  static bool classof(const Metadata *MD) {
    return MD->getMetadataID() == TicketNodeKind;
  }

  // A pointer to the corresponding compilation member in the repository. It is
  // updated when the ticket member is generated by the object writer.
  const pstore::repo::compilation_member *CorrespondingCompilationMember =
      nullptr;
};

} // end namespace llvm

#endif // LLVM_IR_REPO_TICKET_H
