//===- lib/SpecialNames.cpp -----------------------------------------------===//
//*  ____                  _       _   _   _                            *
//* / ___| _ __   ___  ___(_) __ _| | | \ | | __ _ _ __ ___   ___  ___  *
//* \___ \| '_ \ / _ \/ __| |/ _` | | |  \| |/ _` | '_ ` _ \ / _ \/ __| *
//*  ___) | |_) |  __/ (__| | (_| | | | |\  | (_| | | | | | |  __/\__ \ *
//* |____/| .__/ \___|\___|_|\__,_|_| |_| \_|\__,_|_| |_| |_|\___||___/ *
//*       |_|                                                           *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
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
    FiniArrayStart = findString(Db, *NameIndex, "__fini_array_start");
    FiniArrayEnd = findString(Db, *NameIndex, "__fini_array_end");
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
