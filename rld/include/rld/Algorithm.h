//===- include/rld/Algorithm.h ----------------------------*- mode: C++ -*-===//
//*     _    _                  _ _   _                *
//*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___   *
//*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \  *
//*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | | *
//* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_| *
//*            |___/                                   *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ALGORITHM_H
#define RLD_ALGORITHM_H

#include <atomic>
#include <utility>

namespace rld {

// update maximum
// ~~~~~~~~~~~~~~
/// An atomic equivalent to std::max(). Sets Maximum to the greater of its
/// current value and Value.
template <typename T>
inline void updateMaximum(std::atomic<T> &Maximum, T const &Value) noexcept {
  T Prev = Maximum;
  while (Prev < Value && !Maximum.compare_exchange_weak(
                             Prev, Value, std::memory_order_relaxed)) {
  }
}

template <typename Function> class Once {
public:
  explicit Once(Function F) : F_{F} {}
  template <typename... Args> void operator()(Args &&... args) {
    if (!Done_) {
      Done_ = true;
      F_(std::forward<Args>(args)...);
    }
  }
  void reset() { Done_ = false; }

private:
  bool Done_ = false;
  Function F_;
};

template <typename Function> decltype(auto) makeOnce(Function fn) {
  return Once<Function>{fn};
}

} // end namespace rld

#endif // RLD_ALGORITHM_H
