//*   __ _ _                                _  *
//*  / _(_) |__   ___  _ __   __ _  ___ ___(_) *
//* | |_| | '_ \ / _ \| '_ \ / _` |/ __/ __| | *
//* |  _| | |_) | (_) | | | | (_| | (_| (__| | *
//* |_| |_|_.__/ \___/|_| |_|\__,_|\___\___|_| *
//*                                            *
//===- tools/gen/fibonacci.h ----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RLD_GEN_FIBONACCI_H
#define RLD_GEN_FIBONACCI_H

#include <array>
#include <iterator>

template <typename IntegerType = unsigned>
class fibonacci_generator
    : public std::iterator<std::forward_iterator_tag, IntegerType> {
public:
  using result_type = IntegerType;

  explicit fibonacci_generator(IntegerType v) : state_{{v, v}} {}
  fibonacci_generator() : fibonacci_generator(IntegerType{1}) {}
  fibonacci_generator(fibonacci_generator const &rhs) = default;
  fibonacci_generator &operator=(fibonacci_generator const &rhs) = default;

  bool operator==(fibonacci_generator const &rhs) const {
    return state_[1] == rhs.state_[1];
  }
  bool operator!=(fibonacci_generator const &rhs) const {
    return !operator==(rhs);
  }

  IntegerType operator*() const { return state_[1]; }
  fibonacci_generator &operator++() {
    auto const next = state_[0] + state_[1];
    state_[0] = state_[1];
    state_[1] = next;
    return *this;
  }

  fibonacci_generator operator++(int) {
    fibonacci_generator old = *this;
    ++(*this);
    return old;
  }

private:
  std::array<IntegerType, 2> state_;
};

#endif // RLD_GEN_FIBONACCI_H
