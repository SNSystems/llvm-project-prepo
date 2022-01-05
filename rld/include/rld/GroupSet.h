#ifndef RLD_GROUP_SET_H
#define RLD_GROUP_SET_H

#include "pstore/core/index_types.hpp"

#include "rld/Shadow.h"
#include "rld/context.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"

#include <atomic>
#include <mutex>

template <> struct llvm::DenseMapInfo<pstore::index::digest> {
  static constexpr pstore::index::digest getEmptyKey() { return {}; }
  static constexpr pstore::index::digest getTombstoneKey() {
    return std::numeric_limits<pstore::index::digest>::max();
  }
  static constexpr unsigned getHashValue(pstore::index::digest Val) {
    return Val.high() ^ Val.low();
  }
  static constexpr bool isEqual(pstore::index::digest LHS,
                                pstore::index::digest RHS) {
    return LHS == RHS;
  }
};

namespace rld {

using CompilationGroup =
    llvm::DenseMap<pstore::index::digest, std::shared_ptr<std::string>>;

class GroupSet {
public:
  GroupSet() = default;
  void insert(shadow::AtomicTaggedPointer *const Ref) {
    std::lock_guard<std::mutex> _{Mutex_};
    Set_.insert(Ref);
  }

  void clear() {
    std::lock_guard<std::mutex> _{Mutex_};
    Set_.clear();
  }

  void transferTo(Context &C,
                  llvm::DenseMap<pstore::index::digest,
                                 std::shared_ptr<std::string>> *const Group) {
    std::lock_guard<std::mutex> _{Mutex_};
    Group->clear();
    Group->reserve(Set_.size());

    for (auto P : Set_) {
      if (auto *const CR =
              P->load(std::memory_order_acquire).get_if<CompilationRef *>()) {
        Group->try_emplace(CR->Digest, CR->Origin);

        if (CR->Sym != nullptr) {
          P->store(shadow::TaggedPointer{CR->Sym}, std::memory_order_release);
        } else {
          P->store(shadow::TaggedPointer{}, std::memory_order_release);
        }
      }
    }
    Set_.clear();
  }

  template <typename Function> void for_each(Function F) {
    std::lock_guard<std::mutex> _{Mutex_};
    std::for_each(std::begin(Set_), std::end(Set_), F);
  }

private:
  llvm::DenseSet<shadow::AtomicTaggedPointer *> Set_;
  std::mutex Mutex_;

  static constexpr auto DebugType_ = "rld-GroupSet";
};

} // end namespace rld

#endif // RLD_GROUP_SET_H
