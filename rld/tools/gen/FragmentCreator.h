//===- tools/gen/FragmentCreator.h ------------------------*- mode: C++ -*-===//
//*  _____                                     _    *
//* |  ___| __ __ _  __ _ _ __ ___   ___ _ __ | |_  *
//* | |_ | '__/ _` |/ _` | '_ ` _ \ / _ \ '_ \| __| *
//* |  _|| | | (_| | (_| | | | | | |  __/ | | | |_  *
//* |_|  |_|  \__,_|\__, |_| |_| |_|\___|_| |_|\__| *
//*                 |___/                           *
//*   ____                _              *
//*  / ___|_ __ ___  __ _| |_ ___  _ __  *
//* | |   | '__/ _ \/ _` | __/ _ \| '__| *
//* | |___| | |  __/ (_| | || (_) | |    *
//*  \____|_|  \___|\__,_|\__\___/|_|    *
//*                                      *
//===----------------------------------------------------------------------===//
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

#include "StringAdder.h"
#include "fibonacci.h"

#include <bitset>

constexpr auto MaxSection =
    static_cast<size_t>(pstore::repo::section_kind::last);
using SectionSet = std::bitset<MaxSection>;

class FragmentCreator {
public:
  using FragmentIndexValueType = pstore::index::fragment_index::value_type;

  /// \param SectionSize  The number of 32-bit values to be added to each
  /// section's payload.
  /// \param XFixupSize  The number of external fixups to add to each section.
  /// \param IFixupSize  The number of internal fixups to add to each section.
  /// \param Sections  The set of sections to be added to the fragment.
  FragmentCreator(bool DataFibonacci, unsigned SectionSize, unsigned XFixupSize,
                  unsigned IFixupSize, const SectionSet &Sections);
  auto operator()(pstore::transaction_base &T, size_t Count,
                  StringAdder const &Strings) -> FragmentIndexValueType;

private:
  template <typename Generator>
  auto create(pstore::transaction_base &T, Generator &&G, size_t Count,
              const StringAdder &Strings) -> FragmentIndexValueType;

  template <typename Generator>
  pstore::repo::section_content
  generateDataSection(Generator &G, pstore::repo::section_kind Kind,
                      size_t Count, const StringAdder &Strings);

  using DispatcherPtr =
      std::unique_ptr<pstore::repo::section_creation_dispatcher>;

  static auto createDispatchers(const SectionSet &Sections)
      -> llvm::SmallVector<DispatcherPtr, MaxSection>;

  template <pstore::repo::section_kind Kind>
  static DispatcherPtr createDispatcher();

  template <pstore::repo::section_kind Kind>
  using KindToDispatcherType = typename pstore::repo::section_to_creation_dispatcher<typename pstore::repo::enum_to_section<Kind>::type>::type;

  template <pstore::repo::section_kind Kind,
            typename DispatcherType = KindToDispatcherType<Kind>>
  void
  setDispatcherContent(pstore::repo::section_creation_dispatcher *dispatcher,
                       const pstore::repo::section_content *Content);

  const bool DataFibonacci_;
  const unsigned SectionSize_;
  /// The number of external fixups to add to each section.
  const unsigned XFixupSize_;
  /// The number of internal fixups to add to each section.
  const unsigned IFixupSize_;
  const SectionSet Sections_;

  fibonacci_generator<> Fib_;
  llvm::SmallVector<DispatcherPtr, MaxSection> Dispatchers_;
};

#endif // RLD_GEN_FRAGMENT_CREATOR_H
