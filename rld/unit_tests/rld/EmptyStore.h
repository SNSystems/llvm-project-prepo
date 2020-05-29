#ifndef EMPTYSTORE_HPP
#define EMPTYSTORE_HPP

#include "pstore/core/database.hpp"
#include <cstdint>
#include <memory>

class EmptyStore {
public:
  static constexpr std::size_t FileSize = pstore::storage::min_region_size * 2;

  // Build an empty, in-memory database.
  EmptyStore();
  pstore::database & Db() { return *Db_; }

private:
  static std::size_t pageSize ();
  std::shared_ptr<std::uint8_t> Buffer_;
  std::shared_ptr<pstore::file::in_memory> File_;
  std::unique_ptr<pstore::database> Db_;
};

#endif //EMPTYSTORE_HPP
