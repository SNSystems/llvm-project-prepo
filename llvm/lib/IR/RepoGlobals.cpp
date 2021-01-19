//===-  RepoGlobals.cpp - Program repository globals  ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/RepoGlobals.h"
#include "pstore/core/transaction.hpp"

using namespace llvm;

pstore::database &llvm::getRepoDatabase() {
  static std::unique_ptr<pstore::database> Repository;
  if (!Repository) {
    // FIXME: this should be coming from the command-line!
    char const * path = getenv ("REPOFILE");
    if (!path) {
        path = "./clang.db";
    }

    Repository.reset(new pstore::database(
        path, pstore::database::access_mode::writable));
  }
  return *Repository;
}

