#ifndef RLD_TYPES_H
#define RLD_TYPES_H

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/support/gsl.hpp"

#include "llvm/ADT/DenseMap.h"

#include <memory>

namespace rld {

using StringAddress = pstore::typed_address<pstore::indirect_string>;
using CompilationIndexPtr = std::shared_ptr<pstore::index::compilation_index const>;
using FragmentPtr = std::shared_ptr<pstore::repo::fragment const>;

template <typename T> using NotNull = pstore::gsl::not_null<T>;

} // end namespace rld

namespace llvm {

template <> struct DenseMapInfo<rld::StringAddress> {
  using string_address = rld::StringAddress;
  static inline constexpr string_address getEmptyKey() {
    return string_address::null();
  }
  static constexpr string_address getTombstoneKey() {
    return string_address::make(std::numeric_limits<std::uint64_t>::max());
  }
  static constexpr unsigned getHashValue(string_address Address) {
    return static_cast<unsigned>(Address.absolute());
  }
  static constexpr bool isEqual(string_address Lhs, string_address Rhs) {
    return Lhs == Rhs;
  }
};

} // end namespace llvm

#endif // RLD_TYPES_H
