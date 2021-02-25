//*  ____            _   _             _  ___           _  *
//* / ___|  ___  ___| |_(_) ___  _ __ | |/ (_)_ __   __| | *
//* \___ \ / _ \/ __| __| |/ _ \| '_ \| ' /| | '_ \ / _` | *
//*  ___) |  __/ (__| |_| | (_) | | | | . \| | | | | (_| | *
//* |____/ \___|\___|\__|_|\___/|_| |_|_|\_\_|_| |_|\__,_| *
//*                                                        *
//===- include/rld/SectionKind.h ------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
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
    OS << #x;                                                                  \
    return OS;
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

// section kind name
// ~~~~~~~~~~~~~~~~~
#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    return #x;
#define RLD_X(x) X(x)

inline constexpr auto sectionKindName(SectionKind Kind) noexcept {
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

// first section kind
// ~~~~~~~~~~~~~~~~~~
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
