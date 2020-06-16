#ifndef RLD_SECTION_KIND_H
#define RLD_SECTION_KIND_H

#include "pstore/mcrepo/section.hpp"

namespace rld {

//*  ___         _   _          _  ___         _  *
//* / __| ___ __| |_(_)___ _ _ | |/ (_)_ _  __| | *
//* \__ \/ -_) _|  _| / _ \ ' \| ' <| | ' \/ _` | *
//* |___/\___\__|\__|_\___/_||_|_|\_\_|_||_\__,_| *
//*                                               *
enum class SectionKind {
#define X(a) a,
  PSTORE_MCREPO_SECTION_KINDS shstrtab,
  strtab,
  last // Never used. Always last.
#undef X
};

#define X(x)                                                                   \
  case SectionKind::x:                                                         \
    return OS << #x;
template <typename OStream> OStream &operator<<(OStream &OS, SectionKind Kind) {
  switch (Kind) {
    PSTORE_MCREPO_SECTION_KINDS
  case SectionKind::shstrtab:
    return OS << "shstrtab";
  case SectionKind::strtab:
    return OS << "strtab";
  case SectionKind::last:
    break;
  }
  llvm_unreachable("unknown SectionKind");
}
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
inline SectionKind &operator++(SectionKind &SK) noexcept {
#define X(x) SectionKind::x,
  return SK = enum_values<SectionKind,
                          PSTORE_MCREPO_SECTION_KINDS SectionKind::shstrtab,
                          SectionKind::strtab, SectionKind::last>::advance(SK);
#undef X
}

// post-increment
inline SectionKind operator++(SectionKind &SK, int) noexcept {
  auto const prev = SK;
  ++SK;
  return prev;
}

constexpr std::size_t NumSectionKinds =
    static_cast<std::underlying_type<SectionKind>::type>(SectionKind::last);

} // end namespace rld

#endif // RLD_SECTION_KIND_H
