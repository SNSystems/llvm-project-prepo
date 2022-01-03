#ifndef RLD_GROUP_SET_H
#define RLD_GROUP_SET_H

#include "rld/Shadow.h"
#include "rld/context.h"

#include "llvm/ADT/DenseSet.h"

#include <atomic>
#include <mutex>

namespace rld {

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

  template <typename Container> void transferTo(Container *const Group) {
    std::lock_guard<std::mutex> _{Mutex_};
    Group->clear();
    Group->reserve(Set_.size());
    for (auto P : Set_) {
      if (auto *const CR = P->load().get_if<CompilationRef *>()) {
        Group->emplace_back(CR);
        assert(CR->Sym != nullptr);
        P->store(shadow::TaggedPointer{CR->Sym});
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
};

} // end namespace rld

#endif // RLD_GROUP_SET_H
