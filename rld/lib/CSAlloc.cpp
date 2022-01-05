//===- lib/CSAlloc.cpp ----------------------------------------------------===//
//*   ____ ____    _    _ _             *
//*  / ___/ ___|  / \  | | | ___   ___  *
//* | |   \___ \ / _ \ | | |/ _ \ / __| *
//* | |___ ___) / ___ \| | | (_) | (__  *
//*  \____|____/_/   \_\_|_|\___/ \___| *
//*                                     *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/CSAlloc.h"

rld::FixupStorage::FixupStorage(unsigned NumWorkerThreads)
    : S_{NumWorkerThreads} {}

// get thread storage
// ~~~~~~~~~~~~~~~~~~
auto rld::FixupStorage::getThreadStorage() -> NotNull<Container *> {
  static thread_local char tls = 0;
  auto *const ptr = &tls;
  const std::lock_guard<std::mutex> _{Mut_};
  return &S_.try_emplace(ptr).first->second;
}
