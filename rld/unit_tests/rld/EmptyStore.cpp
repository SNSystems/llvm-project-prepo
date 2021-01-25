//*  _____                 _         ____  _                  *
//* | ____|_ __ ___  _ __ | |_ _   _/ ___|| |_ ___  _ __ ___  *
//* |  _| | '_ ` _ \| '_ \| __| | | \___ \| __/ _ \| '__/ _ \ *
//* | |___| | | | | | |_) | |_| |_| |___) | || (_) | | |  __/ *
//* |_____|_| |_| |_| .__/ \__|\__, |____/ \__\___/|_|  \___| *
//*                 |_|        |___/                          *
//===- unit_tests/rld/EmptyStore.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
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
