//*  _____                                     _    ____                _         *
//* |  ___| __ __ _  __ _ _ __ ___   ___ _ __ | |_ / ___|_ __ ___  __ _| |_ ___   *
//* | |_ | '__/ _` |/ _` | '_ ` _ \ / _ \ '_ \| __| |   | '__/ _ \/ _` | __/ _ \  *
//* |  _|| | | (_| | (_| | | | | | |  __/ | | | |_| |___| | |  __/ (_| | || (_) | *
//* |_|  |_|  \__,_|\__, |_| |_| |_|\___|_| |_|\__|\____|_|  \___|\__,_|\__\___/  *
//*                 |___/                                                         *
//*        *
//*  _ __  *
//* | '__| *
//* | |    *
//* |_|    *
//*        *
//===- tools/gen/FragmentCreator.h ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_GEN_FRAGMENT_CREATOR_H
#define RLD_GEN_FRAGMENT_CREATOR_H

#include "llvm/ADT/SmallVector.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/transaction.hpp"
#include "pstore/mcrepo/fragment.hpp"
#include "pstore/mcrepo/generic_section.hpp"
#include "pstore/mcrepo/section.hpp"

#include "fibonacci.hpp"

#include <bitset>

constexpr auto MaxSection =
    static_cast<size_t>(pstore::repo::section_kind::last);
using SectionSet = std::bitset<MaxSection>;

class FragmentCreator {
public:
  using Transaction = pstore::transaction<pstore::transaction_lock>;
  using FragmentIndexValueType = pstore::index::fragment_index::value_type;

  explicit FragmentCreator(bool DataFibonacci, unsigned SectionSize,
                           SectionSet const &Sections);
  auto operator()(Transaction &T, size_t const Count) -> FragmentIndexValueType;

private:
  template <typename Generator>
  auto create(Transaction &T, Generator &&G, size_t const Count)
      -> FragmentIndexValueType;

  template <typename Generator>
  pstore::repo::section_content
  generateDataSection(Generator &G, pstore::repo::section_kind Kind,
                      size_t Count);

  using DispatcherPtr =
      std::unique_ptr<pstore::repo::section_creation_dispatcher>;

  static auto createDispatchers(const SectionSet &Sections)
      -> llvm::SmallVector<DispatcherPtr, MaxSection>;

  template <pstore::repo::section_kind Kind>
  static DispatcherPtr createDispatcher();

  template <pstore::repo::section_kind Kind>
  using KindToDispatcherType = typename pstore::repo::section_to_creation_dispatcher<typename pstore::repo::enum_to_section<Kind>::type>::type;

  template <pstore::repo::section_kind Kind, typename DispatcherType = KindToDispatcherType<Kind>>
  void setDispatcherContent(pstore::repo::section_creation_dispatcher *dispatcher, pstore::repo::section_content const *Content);

  bool const DataFibonacci_;
  unsigned const SectionSize_;
  SectionSet const Sections_;

  fibonacci_generator<> Fib_;
  llvm::SmallVector<DispatcherPtr, MaxSection> Dispatchers_;
};

#endif // RLD_GEN_FRAGMENT_CREATOR_H
