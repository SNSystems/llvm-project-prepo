#include "rld/SpecialNames.h"

#include "rld/types.h"

#include "pstore/core/hamt_set.hpp"

namespace rld {

// initialize
// ~~~~~~~~~~
void SpecialNames::initialize(const pstore::database &Db) {
  if (auto const NameIndex =
          pstore::index::get_index<pstore::trailer::indices::name>(
              Db, false /*create?*/)) {

    GlobalCtors = findString(Db, *NameIndex, "llvm.global_ctors");
    GlobalDtors = findString(Db, *NameIndex, "llvm.global_dtors");
    InitArrayStart = findString(Db, *NameIndex, "__init_array_start");
    InitArrayEnd = findString(Db, *NameIndex, "__init_array_end");
  }
}

// find string
// ~~~~~~~~~~~
template <std::size_t Size>
StringAddress
SpecialNames::findString(const pstore::database &Db,
                         const pstore::index::name_index &NameIndex,
                         const char (&Str)[Size]) {
  const pstore::raw_sstring_view View =
      pstore::make_sstring_view(Str, Size - 1U);
  const auto Pos = NameIndex.find(Db, pstore::indirect_string{Db, &View});
  return Pos != NameIndex.end(Db) ? StringAddress{Pos.get_address()}
                                  : StringAddress::null();
}

} // end namespace rld
