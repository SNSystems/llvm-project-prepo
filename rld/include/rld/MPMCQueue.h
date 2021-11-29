#ifndef RLD_MPMC_QUEUE_H
#define RLD_MPMC_QUEUE_H

/*
Copyright (c) 2020 Erik Rigtorp <erik@rigtorp.se>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */
#include <llvm/ADT/Optional.h>

#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstddef> // offsetof
#include <limits>
#include <memory>
#include <new> // std::hardware_destructive_interference_size
#include <vector>

#ifndef __cpp_aligned_new
#ifdef _WIN32
#include <malloc.h> // _aligned_malloc
#else
#include <stdlib.h> // posix_memalign
#endif
#endif

namespace rld {
namespace mpmc {

#ifdef __cpp_lib_hardware_interference_size
static constexpr std::size_t HardwareInterferenceSize =
    std::hardware_destructive_interference_size;
#else
static constexpr std::size_t HardwareInterferenceSize = 64;
#endif // __cpp_lib_hardware_interference_size

//*    _   _ _                  _     _   _ _              _            *
//*   /_\ | (_)__ _ _ _  ___ __| |   /_\ | | |___  __ __ _| |_ ___ _ _  *
//*  / _ \| | / _` | ' \/ -_) _` |  / _ \| | / _ \/ _/ _` |  _/ _ \ '_| *
//* /_/ \_\_|_\__, |_||_\___\__,_| /_/ \_\_|_\___/\__\__,_|\__\___/_|   *
//*           |___/                                                     *
#if defined(__cpp_aligned_new)
#error
template <typename T> using AlignedAllocator = std::allocator<T>;
#else
template <typename T> struct AlignedAllocator {
  using value_type = T;
  using size_type = size_t;
  using difference_type = ptrdiff_t;

  AlignedAllocator() noexcept = default;
  AlignedAllocator(const AlignedAllocator &) noexcept = default;
  template <typename U>
  AlignedAllocator(const AlignedAllocator<U> &) noexcept {}

  T *allocate(std::size_t n);
  void deallocate(T *p, std::size_t);
};

#ifdef _WIN32
// allocate
// ~~~~~~~~
template <typename T> T *AlignedAllocator<T>::allocate(std::size_t const N) {
  return static_cast<T *>(::_aligned_malloc(sizeof(T) * N, alignof(T)));
}

// deallocate
// ~~~~~~~~~~
template <typename T>
inline void AlignedAllocator<T>::deallocate(T *const P, std::size_t) {
  ::_aligned_free(P);
}
#else
// allocate
// ~~~~~~~~
template <typename T> T *AlignedAllocator<T>::allocate(std::size_t const N) {
  T *p = nullptr;
  ::posix_memalign(reinterpret_cast<void **>(&p), alignof(T), sizeof(T) * N);
  return p;
}

// deallocate
// ~~~~~~~~~~
template <typename T>
inline void AlignedAllocator<T>::deallocate(T *const P, std::size_t) {
  std::free(P);
}
#endif // !_WIN32

#endif //__cpp_aligned_new

//*  ___ _     _    *
//* / __| |___| |_  *
//* \__ \ / _ \  _| *
//* |___/_\___/\__| *
//*                 *
template <typename T> struct Slot {
  constexpr Slot() = default;
  ~Slot() {
    if (Turn & 1U) {
      destroy();
    }
  }

  template <typename... Args> void construct(Args &&...A) {
    new (&Storage) T(std::forward<Args>(A)...);
  }

  void destroy() { reinterpret_cast<T *>(&Storage)->~T(); }

  T &&move() { return reinterpret_cast<T &&>(Storage); }

  // Align to avoid false sharing between adjacent slots
  alignas(HardwareInterferenceSize) std::atomic<std::size_t> Turn = {0};
  typename std::aligned_storage_t<sizeof(T), alignof(T)> Storage;
};

//*   ___                     *
//*  / _ \ _  _ ___ _  _ ___  *
//* | (_) | || / -_) || / -_) *
//*  \__\_\\_,_\___|\_,_\___| *
//*                           *
template <typename T, typename Allocator = AlignedAllocator<Slot<T>>>
class Queue {
public:
  explicit Queue(std::size_t capacity,
                 Allocator const &allocator = Allocator());
  Queue(Queue const &) = delete;
  Queue(Queue &&) = delete;

  ~Queue() = default;

  Queue &operator=(Queue const &) = delete;
  Queue &operator=(Queue &&) = delete;

  /// Enqueue an item using inplace construction. Blocks if the queue is full.
  template <typename... Args> void emplace(Args &&...args);

  /// Try to enqueue an item using inplace construction.
  /// \returns Returns true on success and false if the queue is full.
  template <typename... Args> bool try_emplace(Args &&...args);

  /// Enqueue an item using copy construction. Blocks if the queue is full.
  void push(T const &v) {
    static_assert(std::is_nothrow_copy_constructible<T>::value,
                  "T must be nothrow copy constructible");
    emplace(v);
  }

  /// Enqueue an item using move construction. Participates in overload
  /// resolution only if type P is nothrow constructible. Blocks if the queue is
  /// full.
  template <typename P, typename = typename std::enable_if_t<
                            std::is_nothrow_constructible<T, P &&>::value>>
  void push(P &&v) {
    emplace(std::forward<P>(v));
  }

  /// Try to enqueue an item using copy construction.
  /// \returns True on success and false if the queue is full.
  bool try_push(T const &V) {
    static_assert(std::is_nothrow_copy_constructible<T>::value,
                  "T must be nothrow copy constructible");
    return try_emplace(V);
  }

  /// Try to enqueue an item using move construction. Participates in overload
  /// resolution only if type P is nothrow constructible. \returns true on
  /// success, false if the queue is full.
  template <typename P, typename = typename std::enable_if_t<
                            std::is_nothrow_constructible<T, P &&>::value>>
  bool try_push(P &&V) {
    return try_emplace(std::forward<P>(V));
  }

  /// Dequeue an item by copying or moving the item into v. Blocks if the queue
  /// is empty.
  T pop();
  llvm::Optional<T> try_pop();

private:
  constexpr std::size_t idx(std::size_t I) const { return I % Capacity_; }
  constexpr std::size_t turn(std::size_t I) const { return I / Capacity_; }

  std::size_t const Capacity_;
  std::vector<Slot<T>, Allocator> Slots_;

  // Align to avoid false sharing between head_ and tail_
  alignas(HardwareInterferenceSize) std::atomic<std::size_t> Head_ = {0};
  alignas(HardwareInterferenceSize) std::atomic<std::size_t> Tail_ = {0};
};

// (ctor)
// ~~~~~~
template <typename T, typename Allocator>
Queue<T, Allocator>::Queue(std::size_t const Capacity, Allocator const &Alloc)
    : Capacity_{std::max(Capacity, size_t{1})},
      Slots_{Capacity_ + 1U,
             Alloc} /* One extra to prevent false sharing on the last */ {
  static_assert(
      alignof(Slot<T>) == HardwareInterferenceSize,
      "Slot must be aligned to cache line boundary to prevent false sharing");
  static_assert(
      sizeof(Slot<T>) % HardwareInterferenceSize == 0,
      "Slot size must be a multiple of cache line size to prevent false "
      "sharing between adjacent slots");
  static_assert(
      sizeof(Queue) % HardwareInterferenceSize == 0,
      "Queue size must be a multiple of cache line size to prevent false "
      "sharing between adjacent queues");
  static_assert(
      offsetof(Queue, Tail_) - offsetof(Queue, Head_) ==
          static_cast<std::ptrdiff_t>(HardwareInterferenceSize),
      "head and tail must be a cache line apart to prevent false sharing");
}

// emplace
// ~~~~~~~
template <typename T, typename Allocator>
template <typename... Args>
void Queue<T, Allocator>::emplace(Args &&...A) {
  auto const Head = Head_.fetch_add(1);
  auto &Slot = Slots_[idx(Head)];
  while (turn(Head) * 2 != Slot.Turn.load(std::memory_order_acquire)) {
  }
  Slot.construct(std::forward<Args>(A)...);
  Slot.Turn.store(turn(Head) * 2 + 1, std::memory_order_release);
}

// try emplace
// ~~~~~~~~~~~
template <typename T, typename Allocator>
template <typename... Args>
bool Queue<T, Allocator>::try_emplace(Args &&...A) {
  auto Head = Head_.load(std::memory_order_acquire);
  for (;;) {
    auto &Slot = Slots_[idx(Head)];
    if (turn(Head) * 2 == Slot.Turn.load(std::memory_order_acquire)) {
      if (Head_.compare_exchange_strong(Head, Head + 1)) {
        Slot.construct(std::forward<Args>(A)...);
        Slot.Turn.store(turn(Head) * 2 + 1, std::memory_order_release);
        return true;
      }
    } else {
      auto const PrevHead = Head;
      Head = Head_.load(std::memory_order_acquire);
      if (Head == PrevHead) {
        return false;
      }
    }
  }
}

// pop
// ~~~
template <typename T, typename Allocator> T Queue<T, Allocator>::pop() {
  auto const Tail = Tail_.fetch_add(1);
  auto &Slot = Slots_[idx(Tail)];
  while (turn(Tail) * 2 + 1 != Slot.Turn.load(std::memory_order_acquire)) {
  }
  T const Result = Slot.move();
  Slot.destroy();
  Slot.Turn.store(turn(Tail) * 2 + 2, std::memory_order_release);
  return Result;
}

// try_pop
// ~~~~~~~
template <typename T, typename Allocator>
llvm::Optional<T> Queue<T, Allocator>::try_pop() {
  auto Tail = Tail_.load(std::memory_order_acquire);
  for (;;) {
    auto &Slot = Slots_[idx(Tail)];
    if (turn(Tail) * 2 + 1 == Slot.Turn.load(std::memory_order_acquire)) {
      if (Tail_.compare_exchange_strong(Tail, Tail + 1)) {
        T const Result = Slot.move();
        Slot.destroy();
        Slot.Turn.store(turn(Tail) * 2 + 2, std::memory_order_release);
        return Result;
      }
    } else {
      auto const prev_tail = Tail;
      Tail = Tail_.load(std::memory_order_acquire);
      if (Tail == prev_tail) {
        return {llvm::None};
      }
    }
  }
}

} // end namespace mpmc

template <typename T,
          typename Allocator = mpmc::AlignedAllocator<mpmc::Slot<T>>>
using MPMCQueue = mpmc::Queue<T, Allocator>;

} // end namespace rld

#endif // RLD_MPMC_QUEUE_H
