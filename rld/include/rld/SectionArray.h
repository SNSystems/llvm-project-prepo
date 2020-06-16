//*  ____            _   _                _                          *
//* / ___|  ___  ___| |_(_) ___  _ __    / \   _ __ _ __ __ _ _   _  *
//* \___ \ / _ \/ __| __| |/ _ \| '_ \  / _ \ | '__| '__/ _` | | | | *
//*  ___) |  __/ (__| |_| | (_) | | | |/ ___ \| |  | | | (_| | |_| | *
//* |____/ \___|\___|\__|_|\___/|_| |_/_/   \_\_|  |_|  \__,_|\__, | *
//*                                                           |___/  *
//===- include/rld/SectionArray.h -----------------------------------------===//
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
#ifndef RLD_SECTION_ARRAY_H
#define RLD_SECTION_ARRAY_H

#include "rld/SectionKind.h"

#include <array>

namespace rld {

template <typename T> class SectionArray {
  using Array = std::array<T, static_cast<size_t>(rld::SectionKind::last)>;

public:
  using iterator = typename Array::iterator;
  using const_iterator = typename Array::const_iterator;

  T &operator[](rld::SectionKind S) {
    assert(S != rld::SectionKind::last);
    return Arr_[static_cast<size_t>(S)];
  }
  const T &operator[](rld::SectionKind S) const {
    assert(S != rld::SectionKind::last);
    return Arr_[static_cast<size_t>(S)];
  }

  iterator begin() { return Arr_.begin(); }
  const_iterator begin() const { return Arr_.begin(); }
  const_iterator cbegin() { return Arr_.begin(); }
  iterator end() { return Arr_.end(); }
  const_iterator end() const { return Arr_.end(); }
  const_iterator cend() { return Arr_.cend(); }

private:
  Array Arr_;
};

} // end namespace rld

#endif // RLD_SECTION_ARRAY_H
