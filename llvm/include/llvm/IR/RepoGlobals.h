//===-  RepoGlobals.h - Program repository globals  -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef REPOGLOBALS_H
#define REPOGLOBALS_H

#include <utility>

namespace pstore {
class database;
} // namespace store

namespace llvm {

/// Returns the pstore database instance used for repo output.
/// \note This is not in the best place. It should ideally be in the LLVMContext
/// structure. However, there doesn't seem to be a clean way of accessing that
/// data from the MC library. Needs investigation.
// FIXME: not in the right place.
pstore::database &getRepoDatabase();

} // namespace llvm

#endif // REPOGLOBALS_H
