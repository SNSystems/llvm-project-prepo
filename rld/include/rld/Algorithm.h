//*     _    _                  _ _   _                *
//*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___   *
//*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \  *
//*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | | *
//* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_| *
//*            |___/                                   *
//===- include/rld/Algorithm.h --------------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
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
