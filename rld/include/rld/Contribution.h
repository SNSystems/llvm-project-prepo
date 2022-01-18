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

#include "rld/Context.h"
#include "rld/SectionKind.h"

#include "pstore/mcrepo/section.hpp"


namespace rld {

struct OutputSection;
struct Contribution;

struct ContributionEntry {
  constexpr ContributionEntry() = default;
  constexpr ContributionEntry(const Contribution *const C,
                              const pstore::repo::section_kind K)
      : Contribution {
    C
  }
#ifndef NDEBUG
  , Kind { K }
#endif
  { (void)K; }

  constexpr bool operator==(ContributionEntry const &Other) const {
    return Contribution == Other.Contribution
#ifndef NDEBUG
           && Kind == Other.Kind
#endif
        ;
  }
  constexpr bool operator!=(ContributionEntry const &Other) {
    return !operator==(Other);
  }
  const struct Contribution *Contribution = nullptr;
#ifndef NDEBUG
  pstore::repo::section_kind Kind = pstore::repo::section_kind::last;
#endif
};

using ContributionSpArray = pstore::repo::section_sparray<ContributionEntry>;

//-MARK: Contribution
struct Contribution {
  /// \param S  The contribution's fragment section.
  /// \param XfxSymbols_  An array of symbol pointers: one for each external
  ///   fixup.
  /// \param IfxContributions  A sparse array of contributions: one per
  ///   section of the original fragment. Used for processing internal fixups.
  /// \param OScn_  The output section to which this contribution belongs.
  /// \param Offset_  The offset of this section within the output section.
  /// \param Size_  The number of bytes occupied by this contribution's
  ///   section data.
  /// \param Align_  The alignment of this contribution's section data.
  Contribution(const pstore::repo::section_base *const S,
               const Symbol *const *XfxSymbols_,
               const ContributionSpArray *const IfxContributions_,
               OutputSection *OScn_, uint64_t Offset_, uint64_t Size_,
               unsigned Align_, unsigned InputOrdinal_,
               const SectionKind SectionK, const StringAddress Name)
      : Section{S}, XfxSymbols{XfxSymbols_},
        IfxContributions{IfxContributions_}, OScn{OScn_}, Offset{Offset_},
        Size{Size_}, Align{Align_},
        InputOrdinal{InputOrdinal_}, SectionK{SectionK}, Name{Name} {}

  Contribution(Contribution const &) = delete;
  Contribution(Contribution &&) noexcept = delete;

  Contribution &operator=(Contribution const &) = delete;
  Contribution &operator=(Contribution &&) noexcept = delete;

  const pstore::repo::section_base *const Section;
  const Symbol *const *XfxSymbols;
  const ContributionSpArray *const IfxContributions;
  OutputSection *const OScn;

  /// The offset from the first section of this type in the owning output
  /// section.
  const uint64_t Offset;
  /// The number of bytes occupied by this section.
  const uint64_t Size; // TODO: we really don't need 64-bits for the size of an
                       // individual section.
  /// The required alignment for this section's data.
  const unsigned Align;
  /// The input-ordinal of the ticket file from which this section originates.
  const unsigned InputOrdinal;

  // TODO: the final fields are primarily used for debugging.
  const SectionKind SectionK;
  const StringAddress Name;
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, Contribution const &SI);

} // end namespace rld

#endif // RLD_CONTRIBUTION_H
