//*  ____            _   _             _  ___           _  *
//* / ___|  ___  ___| |_(_) ___  _ __ | |/ (_)_ __   __| | *
//* \___ \ / _ \/ __| __| |/ _ \| '_ \| ' /| | '_ \ / _` | *
//*  ___) |  __/ (__| |_| | (_) | | | | . \| | | | | (_| | *
//* |____/ \___|\___|\__|_|\___/|_| |_|_|\_\_|_| |_|\__,_| *
//*                                                        *
//===- include/rld/SectionKind.h ------------------------------------------===//
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
#ifndef RLD_SECTION_KIND_H
#define RLD_SECTION_KIND_H

#include "pstore/mcrepo/section.hpp"
#include "rld/AdvanceEnum.h"

namespace rld {

//*  ___         _   _          _  ___         _  *
//* / __| ___ __| |_(_)___ _ _ | |/ (_)_ _  __| | *
//* \__ \/ -_) _|  _| / _ \ ' \| ' <| | ' \/ _` | *
//* |___/\___\__|\__|_\___/_||_|_|\_\_|_||_\__,_| *
//*                                               *
#define RLD_SECTION_KINDS                                                      \
  RLD_X(shstrtab)                                                              \
  RLD_X(strtab)                                                                \
  RLD_X(symtab)

enum class SectionKind {
#define X(a) a,
#define RLD_X(a) a,
  PSTORE_MCREPO_SECTION_KINDS RLD_SECTION_KINDS last // Never used. Always last.
#undef RLD_X
#undef X
};

#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    return OS << #x;
#define RLD_X(x) X(x)

template <typename OStream> OStream &operator<<(OStream &OS, SectionKind Kind) {
  switch (Kind) {
    PSTORE_MCREPO_SECTION_KINDS
    RLD_SECTION_KINDS
  case SectionKind::last:
    break;
  }
  llvm_unreachable("unknown SectionKind");
}
#undef RLD_X
#undef X

constexpr auto firstSectionKind() noexcept -> SectionKind {
  using utype = std::underlying_type<SectionKind>::type;
  constexpr auto result = SectionKind::text;
  static_assert(static_cast<utype>(result) == utype{0},
                "expected result to have value 0");
  return result;
}

// Convert from a pstore section-kind to an rld section-kind
template <pstore::repo::section_kind SKind> struct ToRldSectionKind {};
#define X(x)                                                                   \
  template <> struct ToRldSectionKind<pstore::repo::section_kind::x> {         \
    static constexpr auto value = rld::SectionKind::x;                         \
  };
PSTORE_MCREPO_SECTION_KINDS
#undef X

// pre-increment
inline constexpr SectionKind &operator++(SectionKind &SK) noexcept {
#define X(x) SectionKind::x,
#define RLD_X(x) X(x)
  return SK = enum_values<SectionKind,
                          PSTORE_MCREPO_SECTION_KINDS RLD_SECTION_KINDS
                              SectionKind::last>::advance(SK);
#undef RLD_X
#undef X
}

// post-increment
inline SectionKind operator++(SectionKind &SK, int) noexcept {
  auto const prev = SK;
  ++SK;
  return prev;
}

} // end namespace rld

#endif // RLD_SECTION_KIND_H
