#include "rld/CSAlloc.h"

rld::FixupStorage::FixupStorage(unsigned NumWorkerThreads)
    : S_{NumWorkerThreads} {}

// get thread storage
// ~~~~~~~~~~~~~~~~~~
auto rld::FixupStorage::getThreadStorage() -> NotNull<Container *> {
  static thread_local char tls = 0;
  auto *const ptr = &tls;
  const std::lock_guard<std::mutex> _{Mut_};
  return &S_.try_emplace(ptr).first->second;
}
