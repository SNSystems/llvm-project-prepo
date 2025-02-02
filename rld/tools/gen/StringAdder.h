//===- tools/gen/StringAdder.h ----------------------------*- mode: C++ -*-===//
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
#ifndef RLD_GEN_STRING_ADDER_H
#define RLD_GEN_STRING_ADDER_H

#include "llvm/ADT/StringRef.h"

#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"

using IStringAddress = pstore::typed_address<pstore::indirect_string>;

class StringAdder {
public:
  StringAdder(size_t ExpectedStrings,
              std::shared_ptr<pstore::index::name_index> const &NamesIndex);
  /// \param T  The transaction to which the string is to be added.
  /// \param Str The string to be added.
  /// \param IsDefinition true if this string represents the name of a
  /// definition, false otherwise.
  IStringAddress add(pstore::transaction_base &T, const llvm::StringRef &Str,
                     bool IsDefinition);
  void flush(pstore::transaction_base &Transaction);

  size_t size() const { return Strings_.size(); }
  IStringAddress pick(size_t Ind) const {
    const auto Size = this->size();
    Ind %= Size;
    while (!Strings_[Ind].IsDefinition) {
      const auto Ind2 = (Ind + 1U) % Size;
      assert(Ind2 != Ind);
      Ind = Ind2;
    }
    return Strings_[Ind].Address;
  }

private:
  std::shared_ptr<pstore::index::name_index> NamesIndex_;
  pstore::indirect_string_adder Adder_;

  struct StringsMember {
    StringsMember(std::string S, bool Definition)
        : Str{std::move(S)}, IsDefinition{Definition} {}
    std::string Str;
    pstore::raw_sstring_view View;
    bool IsDefinition = false;
    IStringAddress Address;
  };

  // using StringsMember = std::tuple<std::string, pstore::raw_sstring_view>;
  std::vector<StringsMember> Strings_;
};

#endif // RLD_GEN_STRING_ADDER_H
