//===- include/rld/SectionArray.h -------------------------*- mode: C++ -*-===//
//*  ____            _   _                  _                          *
//* / ___|  ___  ___| |_(_) ___  _ __      / \   _ __ _ __ __ _ _   _  *
//* \___ \ / _ \/ __| __| |/ _ \| '_ \    / _ \ | '__| '__/ _` | | | | *
//*  ___) |  __/ (__| |_| | (_) | | | |  / ___ \| |  | | | (_| | |_| | *
//* |____/ \___|\___|\__|_|\___/|_| |_| /_/   \_\_|  |_|  \__,_|\__, | *
//*                                                             |___/  *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_SECTION_ARRAY_H
#define RLD_SECTION_ARRAY_H

#include "rld/SectionKind.h"

#include <array>
#include <type_traits>

namespace rld {

template <typename Key, Key Max, typename Value> class EnumIndexedArray {
  using UnderlyingType = std::underlying_type_t<Key>;
  using Array = std::array<Value, static_cast<UnderlyingType>(Max)>;

public:
  using value_type = typename Array::value_type;
  using reference = typename Array::reference;
  using const_reference = typename Array::const_reference;
  using iterator = typename Array::iterator;
  using const_iterator = typename Array::const_iterator;
  using pointer = typename Array::pointer;
  using const_pointer = typename Array::const_pointer;
  using size_type = typename Array::size_type;
  using difference_type = typename Array::difference_type;
  using reverse_iterator = typename Array::reverse_iterator;
  using const_reverse_iterator = typename Array::const_reverse_iterator;

  iterator begin() { return Array_.begin(); }
  const_iterator begin() const { return Array_.begin(); }
  const_iterator cbegin() const { return Array_.cbegin(); }
  reverse_iterator rbegin() { return Array_.rbegin(); }
  const_reverse_iterator rbegin() const { return Array_.rbegin(); }
  const_reverse_iterator crbegin() const { return Array_.crbegin(); }

  iterator end() { return Array_.end(); }
  const_iterator end() const { return Array_.end(); }
  const_iterator cend() const { return Array_.cend(); }
  reverse_iterator rend() { return Array_.rend(); }
  const_reverse_iterator rend() const { return Array_.rend(); }
  const_reverse_iterator crend() const { return Array_.crend(); }

  constexpr size_type size() const { return Array_.size(); }
  constexpr size_type max_size() const { return Array_.max_size(); }
  constexpr bool empty() const { return Array_.empty(); }

  Value &operator[](Key K) { return Array_[static_cast<UnderlyingType>(K)]; }
  const Value &operator[](Key K) const {
    return Array_[static_cast<UnderlyingType>(K)];
  }

  Value *data() { return Array_.data(); }
  const Value *data() const { return Array_.data(); }

  Value &front() { return Array_.front(); }
  const Value &front() const { return Array_.front(); }
  Value &back() { return Array_.back(); }
  const Value &back() const { return Array_.back(); }

  Array Array_;
};

template <typename ValueType>
using SectionArray =
    EnumIndexedArray<SectionKind, rld::SectionKind::last, ValueType>;

} // end namespace rld

#endif // RLD_SECTION_ARRAY_H
