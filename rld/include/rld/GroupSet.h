//===- include/rld/GroupSet.h -----------------------------*- mode: C++ -*-===//
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
#ifndef RLD_GROUP_SET_H
#define RLD_GROUP_SET_H

#include "rld/Context.h"
#include "rld/Shadow.h"

#include "pstore/core/index_types.hpp"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"

#include <atomic>
#include <mutex>

template <> struct llvm::DenseMapInfo<pstore::index::digest> {
  static constexpr pstore::index::digest getEmptyKey() { return {}; }
  static constexpr pstore::index::digest getTombstoneKey() {
    return std::numeric_limits<pstore::index::digest>::max();
  }
  static constexpr unsigned getHashValue(pstore::index::digest Val) {
    return Val.high() ^ Val.low();
  }
  static constexpr bool isEqual(pstore::index::digest LHS,
                                pstore::index::digest RHS) {
    return LHS == RHS;
  }
};

namespace rld {

using CompilationGroup = llvm::SmallVector<
    std::pair<pstore::index::digest, std::shared_ptr<std::string>>, 256>;

class GroupSet {
public:
  GroupSet() = default;
  void insert(shadow::AtomicTaggedPointer *const Ref) {
    std::lock_guard<std::mutex> _{Mutex_};
    Set_.insert(Ref);
  }

  void clear() {
    std::lock_guard<std::mutex> _{Mutex_};
    Set_.clear();
  }

  void transferTo(Context &C, CompilationGroup *const Group);

  template <typename Function> void for_each(Function F) {
    std::lock_guard<std::mutex> _{Mutex_};
    std::for_each(std::begin(Set_), std::end(Set_), F);
  }

private:
  llvm::DenseSet<shadow::AtomicTaggedPointer *> Set_;
  std::mutex Mutex_;
};

} // end namespace rld

#endif // RLD_GROUP_SET_H
