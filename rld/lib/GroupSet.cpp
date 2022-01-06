//===- rld/lib/GroupSet.cpp -----------------------------------------------===//
//*   ____                         ____       _    *
//*  / ___|_ __ ___  _   _ _ __   / ___|  ___| |_  *
//* | |  _| '__/ _ \| | | | '_ \  \___ \ / _ \ __| *
//* | |_| | | | (_) | |_| | |_) |  ___) |  __/ |_  *
//*  \____|_|  \___/ \__,_| .__/  |____/ \___|\__| *
//*                       |_|                      *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/GroupSet.h"

using namespace llvm;

void rld::GroupSet::transferTo(Context &C, CompilationGroup *const Group) {
  llvm::DenseMap<pstore::index::digest, std::shared_ptr<std::string>> linkees;
  {
    std::lock_guard<std::mutex> _{Mutex_};
    for (auto P : Set_) {
      if (auto *const CR =
              P->load(std::memory_order_acquire).get_if<CompilationRef *>()) {
        linkees.try_emplace(CR->Digest, CR->Origin);
        P->store(shadow::TaggedPointer{CR->Sym != nullptr ? CR->Sym : nullptr},
                 std::memory_order_release);
      }
    }
    Set_.clear();
  }

  Group->clear();
  Group->reserve(linkees.size());
  std::copy(std::begin(linkees), std::end(linkees), std::back_inserter(*Group));
}
