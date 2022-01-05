//===- lib/SectionKind.cpp ------------------------------------------------===//
//*  ____            _   _               _  ___           _  *
//* / ___|  ___  ___| |_(_) ___  _ __   | |/ (_)_ __   __| | *
//* \___ \ / _ \/ __| __| |/ _ \| '_ \  | ' /| | '_ \ / _` | *
//*  ___) |  __/ (__| |_| | (_) | | | | | . \| | | | | (_| | *
//* |____/ \___|\___|\__|_|\___/|_| |_| |_|\_\_|_| |_|\__,_| *
//*                                                          *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/SectionKind.h"

namespace rld {

#define X(x)                                                                   \
  constexpr SectionKind ToRldSectionKind<pstore::repo::section_kind::x>::value;
PSTORE_MCREPO_SECTION_KINDS
#undef X

} // end namespace rld
