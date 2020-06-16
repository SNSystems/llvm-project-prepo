//*  _                          *
//* | |_ _   _ _ __   ___  ___  *
//* | __| | | | '_ \ / _ \/ __| *
//* | |_| |_| | |_) |  __/\__ \ *
//*  \__|\__, | .__/ \___||___/ *
//*      |___/|_|               *
//===- include/rld/types.h ------------------------------------------------===//
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
#ifndef RLD_TYPES_H
#define RLD_TYPES_H

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/support/gsl.hpp"

#include "llvm/ADT/DenseMap.h"

#include <memory>

namespace rld {

using StringAddress = pstore::typed_address<pstore::indirect_string>;
using CompilationIndexPtr = std::shared_ptr<pstore::index::compilation_index const>;
using FragmentPtr = std::shared_ptr<pstore::repo::fragment const>;

template <typename T> using NotNull = pstore::gsl::not_null<T>;

} // end namespace rld

namespace llvm {

template <> struct DenseMapInfo<rld::StringAddress> {
  using string_address = rld::StringAddress;
  static inline constexpr string_address getEmptyKey() {
    return string_address::null();
  }
  static constexpr string_address getTombstoneKey() {
    return string_address::make(std::numeric_limits<std::uint64_t>::max());
  }
  static constexpr unsigned getHashValue(string_address Address) {
    return static_cast<unsigned>(Address.absolute());
  }
  static constexpr bool isEqual(string_address Lhs, string_address Rhs) {
    return Lhs == Rhs;
  }
};

} // end namespace llvm

#endif // RLD_TYPES_H
