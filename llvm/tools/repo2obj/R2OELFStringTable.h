//===- R2OELFStringTable.h ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_REPO2OBJ_ELFSTRINGTABLE_H
#define LLVM_TOOLS_REPO2OBJ_ELFSTRINGTABLE_H

#include "pstore/adt/sstring_view.hpp"
#include "pstore/core/indirect_string.hpp"
#include "llvm/Support/raw_ostream.h"

#include <list>
#include <string>
#include <tuple>
#include <unordered_map>

namespace pstore {
class database;
struct address;
} // namespace pstore

namespace llvm {

inline raw_ostream &operator<<(raw_ostream &OS,
                               pstore::indirect_string const &ind_str) {
  pstore::shared_sstring_view owner;
  auto View = ind_str.as_string_view(&owner);
  return OS << StringRef{View.data(), View.length()};
}

} // end namespace llvm

/// Used to track names created during the object-file generation.
class GeneratedNames {
public:
  explicit GeneratedNames(pstore::database const &Db) : Db_{Db} {}

  pstore::indirect_string add(pstore::shared_sstring_view const &Name);
  pstore::indirect_string add(std::string const &S) {
    return this->add(pstore::make_shared_sstring_view(S));
  }
  pstore::indirect_string add(char const *S) {
    return this->add(pstore::make_shared_sstring_view(S));
  }

private:
  pstore::database const &Db_;
  std::unordered_map<pstore::shared_sstring_view, pstore::raw_sstring_view>
      Storage_;
};

class StringTable {
private:
  using container = std::unordered_map<pstore::indirect_string, std::uint64_t>;

public:
  using value_type = typename container::value_type;
  using iterator = typename container::const_iterator;
  using const_iterator = iterator;

  static constexpr auto npos = std::numeric_limits<std::uint64_t>::max();

  const_iterator begin() const { return std::begin(Strings_); }
  const_iterator end() const { return std::end(Strings_); }

  /// Inserts a name if not already present and returns its ELF string table
  /// offset.
  std::uint64_t insert(pstore::indirect_string const &Name);

  /// Writes the string data to the output stream \p OS a returns the extent
  /// (position and size) of the data written.
  std::tuple<std::uint64_t, std::uint64_t> write(llvm::raw_ostream &OS) const;

  std::uint64_t dataSize() const { return DataSize_; }

private:
  std::uint64_t position(pstore::indirect_string const &Name) const;

  container Strings_;
  std::list<pstore::indirect_string> Data_;
  std::uint64_t DataSize_ = 1U;
};

#endif // LLVM_TOOLS_REPO2OBJ_ELFSTRINGTABLE_H
