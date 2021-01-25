//*  _____                 _         ____  _                  *
//* | ____|_ __ ___  _ __ | |_ _   _/ ___|| |_ ___  _ __ ___  *
//* |  _| | '_ ` _ \| '_ \| __| | | \___ \| __/ _ \| '__/ _ \ *
//* | |___| | | | | | |_) | |_| |_| |___) | || (_) | | |  __/ *
//* |_____|_| |_| |_| .__/ \__|\__, |____/ \__\___/|_|  \___| *
//*                 |_|        |___/                          *
//===- unit_tests/rld/EmptyStore.h ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
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
