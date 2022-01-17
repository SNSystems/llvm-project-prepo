//===- include/rld/SpecialNames.h -------------------------*- mode: C++ -*-===//
//*  ____                  _       _   _   _                            *
//* / ___| _ __   ___  ___(_) __ _| | | \ | | __ _ _ __ ___   ___  ___  *
//* \___ \| '_ \ / _ \/ __| |/ _` | | |  \| |/ _` | '_ ` _ \ / _ \/ __| *
//*  ___) | |_) |  __/ (__| | (_| | | | |\  | (_| | | | | | |  __/\__ \ *
//* |____/| .__/ \___|\___|_|\__,_|_| |_| \_|\__,_|_| |_| |_|\___||___/ *
//*       |_|                                                           *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_SPECIALNAMES_H
#define RLD_SPECIALNAMES_H

#include "rld/Types.h"

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
