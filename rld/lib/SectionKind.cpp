#include "rld/SectionKind.h"

namespace rld {

#define X(x)                                                                   \
  constexpr SectionKind ToRldSectionKind<pstore::repo::section_kind::x>::value;
PSTORE_MCREPO_SECTION_KINDS
#undef X

} // end namespace rld
