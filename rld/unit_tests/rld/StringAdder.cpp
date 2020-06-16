#include "StringAdder.h"

#include "pstore/core/hamt_set.hpp"

StringAdder::StringAdder(pstore::database &Db) : Db_{Db} {
  assert(getNameIndex()->size() == 0 && "The names index was not empty");
}
