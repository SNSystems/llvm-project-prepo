#include "EmptyStore.h"

#include "pstore/core/database.hpp"
#include "llvm/ADT/STLExtras.h"

constexpr std::size_t EmptyStore::FileSize;

EmptyStore::EmptyStore()
    : Buffer_{pstore::aligned_valloc(FileSize, pageSize())},
      File_{std::make_shared<pstore::file::in_memory>(Buffer_, FileSize)} {
  pstore::database::build_new_store(*File_);
  Db_ = std::make_unique<pstore::database>(File_);
  Db_->set_vacuum_mode(pstore::database::vacuum_mode::disabled);
}

std::size_t EmptyStore::pageSize() { return pstore::system_page_size{}.get(); }
