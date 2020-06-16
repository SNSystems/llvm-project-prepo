//*  ____  _        _                _       _     _            *
//* / ___|| |_ _ __(_)_ __   __ _   / \   __| | __| | ___ _ __  *
//* \___ \| __| '__| | '_ \ / _` | / _ \ / _` |/ _` |/ _ \ '__| *
//*  ___) | |_| |  | | | | | (_| |/ ___ \ (_| | (_| |  __/ |    *
//* |____/ \__|_|  |_|_| |_|\__, /_/   \_\__,_|\__,_|\___|_|    *
//*                         |___/                               *
//===- unit_tests/rld/StringAdder.h ---------------------------------------===//
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
#ifndef RLD_UNIT_TESTS_STRING_ADDER_H
#define RLD_UNIT_TESTS_STRING_ADDER_H

#include "rld/types.h"

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
