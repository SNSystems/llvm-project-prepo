//*  ____  _        _                _       _     _            *
//* / ___|| |_ _ __(_)_ __   __ _   / \   __| | __| | ___ _ __  *
//* \___ \| __| '__| | '_ \ / _` | / _ \ / _` |/ _` |/ _ \ '__| *
//*  ___) | |_| |  | | | | | (_| |/ ___ \ (_| | (_| |  __/ |    *
//* |____/ \__|_|  |_|_| |_|\__, /_/   \_\__,_|\__,_|\___|_|    *
//*                         |___/                               *
//===- unit_tests/rld/StringAdder.cpp -------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "StringAdder.h"

#include "pstore/core/hamt_set.hpp"

StringAdder::StringAdder(pstore::database &Db) : Db_{Db} {
  assert(getNameIndex()->size() == 0 && "The names index was not empty");
}
