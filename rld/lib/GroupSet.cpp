//===- lib/GroupSet.cpp ---------------------------------------------------===//
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

#include <map>

using namespace llvm;

void rld::GroupSet::transferTo(Context &C, CompilationGroup *const Group) {
  // FIXME: this is a temporary hack to ensure that the order of the Linkees
  // container is stable regardless of the sequence in which members are added.
  //  llvm::DenseMap<pstore::index::digest, std::shared_ptr<std::string>>
  //  Linkees;
  std::map<pstore::index::digest, std::shared_ptr<std::string>> Linkees;
  {
    std::lock_guard<std::mutex> _{Mutex_};
    for (auto P : Set_) {
      if (auto *const CR =
              P->load(std::memory_order_acquire).get_if<CompilationRef *>()) {
        //        Linkees.try_emplace(CR->Digest, CR->Origin);
        Linkees.emplace(CR->Digest, CR->Origin);
        P->store(shadow::TaggedPointer{CR->Sym != nullptr ? CR->Sym : nullptr},
                 std::memory_order_release);
      }
    }
    Set_.clear();
  }

  Group->clear();
  Group->reserve(Linkees.size());
  std::copy(std::begin(Linkees), std::end(Linkees), std::back_inserter(*Group));
}
