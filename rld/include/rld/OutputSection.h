//===- include/rld/OutputSection.h ------------------------*- mode: C++ -*-===//
//*   ___        _               _     ____            _   _              *
//*  / _ \ _   _| |_ _ __  _   _| |_  / ___|  ___  ___| |_(_) ___  _ __   *
//* | | | | | | | __| '_ \| | | | __| \___ \ / _ \/ __| __| |/ _ \| '_ \  *
//* | |_| | |_| | |_| |_) | |_| | |_   ___) |  __/ (__| |_| | (_) | | | | *
//*  \___/ \__,_|\__| .__/ \__,_|\__| |____/ \___|\___|\__|_|\___/|_| |_| *
//*                 |_|                                                   *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_OUTPUT_SECTION_H
#define RLD_OUTPUT_SECTION_H

#include "rld/SectionKind.h"

#include "pstore/adt/chunked_sequence.hpp"

namespace rld {

class Layout;

//-MARK: OutputSection
struct OutputSection {
  explicit OutputSection(SectionKind Kind) : SectionK{Kind} {}

  using ContributionVector =
      pstore::chunked_sequence<Contribution,
                               (32 * 1024 * 1024) / sizeof(Contribution)>;
  SectionKind const SectionK;

  ContributionVector Contributions;

  /// For an output section containing data that is loaded on the target, the
  /// virtual address assigned to this section data. 0 otherwise.
  uint64_t VirtualAddr = 0;
  /// The total virtual memory size of the output section contents (which may
  /// either come from contributions or metadata stored elsewhere).
  uint64_t VirtualSize = 0;
  /// The total file size of the output section contents (which may either come
  /// from contributions or metadata stored elsewhere).
  uint64_t FileSize = 0;
  /// The alignment of the most aligned contribution.
  unsigned MaxAlign = 1U;

  bool AlwaysEmit = false;
  // The section to which this section is linked. Used to set the
  // ELF section header's sh_link field. A value of 'last' corresponds to an
  // sh_link value of 0 (i.e. no linked section).
  SectionKind Link = SectionKind::last;
  // The ELF section-info field. For relocation sections this is section to
  // which the relocations apply.
  SectionKind Info = SectionKind::last;

  bool shouldEmit() const {
    return AlwaysEmit || VirtualSize > 0 || FileSize > 0;
  }
};

} // end namespace rld

#endif // RLD_OUTPUT_SECTION_H
