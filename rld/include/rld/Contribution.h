//===- include/rld/Contribution.h -------------------------*- mode: C++ -*-===//
//*   ____            _        _ _           _   _              *
//*  / ___|___  _ __ | |_ _ __(_) |__  _   _| |_(_) ___  _ __   *
//* | |   / _ \| '_ \| __| '__| | '_ \| | | | __| |/ _ \| '_ \  *
//* | |__| (_) | | | | |_| |  | | |_) | |_| | |_| | (_) | | | | *
//*  \____\___/|_| |_|\__|_|  |_|_.__/ \__,_|\__|_|\___/|_| |_| *
//*                                                             *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_CONTRIBUTION_H
#define RLD_CONTRIBUTION_H

#include "pstore/mcrepo/section.hpp"

#include "rld/context.h"

namespace rld {

struct OutputSection;

//-MARK: Contribution
struct Contribution {
  constexpr Contribution(pstore::repo::section_base const *S, UintptrAddress SA,
                         OutputSection *OScn_, uint64_t Offset_, uint64_t Size_,
                         unsigned Align_, StringAddress Name_,
                         unsigned const InputOrdinal_)
      : Section{S}, XfxShadow{SA}, OScn{OScn_}, Offset{Offset_}, Size{Size_},
        Align{Align_}, InputOrdinal{InputOrdinal_}, Name{Name_} {}

  pstore::repo::section_base const *const Section;
  UintptrAddress const XfxShadow;

  OutputSection *OScn;

  /// The offset from the first section of this type in the owning output
  /// section.
  uint64_t const Offset;
  /// The number of bytes occupied by this section.
  uint64_t const Size; // TODO: we really don't need 64-bits for the size of an
                       // individual section.
  /// The required alignment for this section's data.
  unsigned const Align;
  /// The input-ordinal of the ticket file from which this section originates.
  unsigned const InputOrdinal;

  StringAddress const Name;
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, Contribution const &SI);

} // end namespace rld

#endif // RLD_CONTRIBUTION_H
