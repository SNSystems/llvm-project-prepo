#ifndef RLD_SPECIALNAMES_H
#define RLD_SPECIALNAMES_H

#include "rld/types.h"

namespace pstore {
class database;
}

namespace rld {

class SpecialNames {
public:
  void initialize(const pstore::database &Db);

  StringAddress GlobalCtors = StringAddress::null();
  StringAddress GlobalDtors = StringAddress::null();
  StringAddress InitArrayStart = StringAddress::null();
  StringAddress InitArrayEnd = StringAddress::null();
  StringAddress FiniArrayStart = StringAddress::null();
  StringAddress FiniArrayEnd = StringAddress::null();

private:
  template <std::size_t Size>
  static StringAddress findString(const pstore::database &Db,
                                  const pstore::index::name_index &NameIndex,
                                  const char (&Str)[Size]);
};

} // end namespace rld

#endif // RLD_SPECIALNAMES_H
