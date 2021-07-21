#ifndef RLD_CSALLOC_HPP
#define RLD_CSALLOC_HPP

#include "rld/types.h"

#include "pstore/adt/chunked_sequence.hpp"

#include "llvm/ADT/DenseMap.h"

#include <cstdint>
#include <mutex>

namespace rld {

/// Provides per-worker thread storage for symbol pointers yielded by
/// fixup resolution.
class FixupStorage {
  constexpr static size_t ChunkSize = 256 * 1024;

public:
  explicit FixupStorage(unsigned NumWorkerThreads);
  using Container = pstore::chunked_sequence<uint8_t, ChunkSize>;
  NotNull<Container *> getThreadStorage();

private:
  llvm::DenseMap<char *, Container> S_;
  std::mutex Mut_;
};

/// Increases the number of elements in a byte-wide chunked sequence by \p
/// required ensuring that the resulting storage is contiguous.
///
/// \param Storage  A chunked-sequence which will be used to manage the storage.
/// \param required  The number of contiguous bytes required.
/// \param align  The required alignment for the start of the returned storage.
/// \result A pointer to a contiguous block of storage which is sufficient for
///   \p required bytes.
template <size_t ElementsPerChunk>
void *
csAlloc(pstore::chunked_sequence<uint8_t, ElementsPerChunk> *const Storage,
        size_t Required, size_t const Align) {
  assert(Storage != nullptr && "Storage must not be null");
  assert(Required <= ElementsPerChunk);
  Required = std::max(Required, size_t{1});
  if (Required > ElementsPerChunk) {
    return nullptr;
  }
  size_t const Capacity = Storage->capacity();
  size_t Size = Storage->size();
  assert(Capacity >= Size && "Capacity cannot be less than size");
  if (Capacity - (Size + Align - 1U) < Required) {
    // A resize to burn through the remaining members of the container's final
    // chunk.
    Storage->resize(Capacity);
    Size = Capacity;
  }
  // Add a default-constructed value. This is the first element of the returned
  // array and gets us the starting address.
  auto *Result = &Storage->emplace_back();
  ++Size;
  auto Misaligned = reinterpret_cast<uintptr_t>(Result) % Align;
  if (Misaligned != 0) {
    Required += Align - Misaligned;
    Result += Align - Misaligned;
  }
  assert(Storage->size() == Size &&
         "Size didn't track the container size correctly");
  Storage->resize(Size + Required - 1U);
  return Result;
}

template <typename T, size_t ElementsPerChunk>
T *csAlloc(pstore::chunked_sequence<uint8_t, ElementsPerChunk> *const Storage,
           size_t Elements) {
  return reinterpret_cast<T *>(
      csAlloc(Storage, Elements * sizeof(T), alignof(T)));
}

} // end namespace rld

#endif // RLD_CSALLOC_HPP
