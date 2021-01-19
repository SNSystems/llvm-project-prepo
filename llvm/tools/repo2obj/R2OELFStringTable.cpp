//===- R2OELFStringTable.cpp ----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "R2OELFStringTable.h"

#include "llvm/Support/Debug.h"

#include "pstore/core/database.hpp"
#include "pstore/core/db_archive.hpp"
#include "pstore/core/sstring_view_archive.hpp"
#include "pstore/serialize/standard_types.hpp"

//*   ___                       _          _ _  _                    *
//*  / __|___ _ _  ___ _ _ __ _| |_ ___ __| | \| |__ _ _ __  ___ ___ *
//* | (_ / -_) ' \/ -_) '_/ _` |  _/ -_) _` | .` / _` | '  \/ -_|_-< *
//*  \___\___|_||_\___|_| \__,_|\__\___\__,_|_|\_\__,_|_|_|_\___/__/ *
//*                                                                  *
pstore::indirect_string
GeneratedNames::add(pstore::shared_sstring_view const &Name) {
  auto Pos =
      Storage_.emplace(Name, pstore::raw_sstring_view{Name.data(), Name.size()})
          .first;
  return {Db_, &Pos->second};
}

//*  ___ _       _          _____     _    _      *
//* / __| |_ _ _(_)_ _  __ |_   _|_ _| |__| |___  *
//* \__ \  _| '_| | ' \/ _` || |/ _` | '_ \ / -_) *
//* |___/\__|_| |_|_||_\__, ||_|\__,_|_.__/_\___| *
//*                    |___/                      *
std::uint64_t StringTable::insert(pstore::indirect_string const &Name) {
  typename container::iterator Pos;
  bool DidInsert;
  std::tie(Pos, DidInsert) = Strings_.emplace(Name, 0);
  if (DidInsert) {
    pstore::shared_sstring_view Owner;
    pstore::raw_sstring_view View = Name.as_string_view(&Owner);

    DEBUG_WITH_TYPE("repo2obj",
                    (llvm::dbgs() << "  strtab insert "
                                  << llvm::StringRef{View.data(), View.length()}
                                  << " at " << DataSize_ << '\n'));
    Pos->second = DataSize_;
    DataSize_ += View.length() + 1;
    Data_.push_back(Name);
  }
  return Pos->second;
}

std::uint64_t StringTable::position(pstore::indirect_string const &Name) const {
  auto Pos = Strings_.find(Name);
  return Pos != Strings_.end() ? Pos->second : npos;
}

std::tuple<std::uint64_t, std::uint64_t>
StringTable::write(llvm::raw_ostream &OS) const {
  std::uint64_t Start = OS.tell();
  OS << '\0';
  for (pstore::indirect_string const &Name : Data_) {
    assert(OS.tell() - Start == this->position(Name));
    pstore::shared_sstring_view Owner;
    pstore::raw_sstring_view const View = Name.as_string_view(&Owner);
    OS << llvm::StringRef{View.data(), View.length()} << '\0';
  }

  std::uint64_t End = OS.tell();
  assert(End >= Start);
  return std::make_tuple(Start, End - Start);
}
// eof:R2OELFStringTable.cpp
