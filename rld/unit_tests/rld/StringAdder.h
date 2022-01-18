//===- unit_tests/rld/StringAdder.h -----------------------*- mode: C++ -*-===//
//*  ____  _        _                  _       _     _            *
//* / ___|| |_ _ __(_)_ __   __ _     / \   __| | __| | ___ _ __  *
//* \___ \| __| '__| | '_ \ / _` |   / _ \ / _` |/ _` |/ _ \ '__| *
//*  ___) | |_| |  | | | | | (_| |  / ___ \ (_| | (_| |  __/ |    *
//* |____/ \__|_|  |_|_| |_|\__, | /_/   \_\__,_|\__,_|\___|_|    *
//*                         |___/                                 *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_UNIT_TESTS_STRING_ADDER_H
#define RLD_UNIT_TESTS_STRING_ADDER_H

#include "rld/Types.h"

#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/core/transaction.hpp"

#include "llvm/ADT/StringRef.h"

#include <cassert>
#include <list>
#include <memory>
#include <tuple>

// FIXME: copied from gen.cpp
class StringAdder {
public:
  explicit StringAdder(pstore::database &Db);

  template <typename Lock>
  rld::StringAddress add(pstore::transaction<Lock> &Transaction,
                         llvm::StringRef const &Str);

  template <typename Lock> void flush(pstore::transaction<Lock> &Transaction);

private:
  std::shared_ptr<pstore::index::name_index> getNameIndex() {
    return pstore::index::get_index<pstore::trailer::indices::name>(Db_);
  }

  pstore::database &Db_;
  using storage = std::tuple<pstore::raw_sstring_view, rld::StringAddress>;
  std::map<std::string, storage> Strings_;
  pstore::indirect_string_adder Adder_;
};

template <typename Lock>
rld::StringAddress StringAdder::add(pstore::transaction<Lock> &Transaction,
                                    llvm::StringRef const &Str) {
  auto Res = Strings_.emplace(Str, storage{});
  auto &Pos = Res.first;
  auto &Value = Pos->second;
  if (!Res.second) {
    // The insertion did not happen. The string is already in the map, so we can
    // just return its pstore address. the
    assert(std::get<rld::StringAddress>(Value) != rld::StringAddress{} &&
           "String in the map already had an associated address!");
    return std::get<rld::StringAddress>(Value);
  }
  auto &View = std::get<pstore::raw_sstring_view>(Value);
  View = pstore::raw_sstring_view{Pos->first.data(), Pos->first.size()};
  auto const AddRes = Adder_.add(Transaction, getNameIndex(), &View);
  assert(AddRes.second && "String was already found in the index");
  auto &Address = std::get<rld::StringAddress>(Value);
  Address = rld::StringAddress::make(AddRes.first.get_address());
  return Address;
}

template <typename Lock>
void StringAdder::flush(pstore::transaction<Lock> &Transaction) {
  Adder_.flush(Transaction);
}

#endif // RLD_UNIT_TESTS_STRING_ADDER_H
