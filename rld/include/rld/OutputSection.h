//*   ___       _             _     ___         _   _           *
//*  / _ \ _  _| |_ _ __ _  _| |_  / __| ___ __| |_(_)___ _ _   *
//* | (_) | || |  _| '_ \ || |  _| \__ \/ -_) _|  _| / _ \ ' \  *
//*  \___/ \_,_|\__| .__/\_,_|\__| |___/\___\__|\__|_\___/_||_| *
//*                |_|                                          *
//===- include/rld/OutputSection.h ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_OUTPUT_SECTION_H
#define RLD_OUTPUT_SECTION_H

#include "rld/SectionKind.h"

#include "pstore/adt/chunked_vector.hpp"

namespace rld {

class Layout;

//-MARK: OutputSection
struct OutputSection {
  explicit OutputSection(SectionKind Kind) : SectionK{Kind} {}

  using ContributionVector =
      pstore::chunked_vector<Contribution,
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
  unsigned MaxAlign = 0U;

  bool AlwaysEmit = false;
  // The section to which this section is linked. Used to set the
  // ELF section header's sh_link field. A value of 'last' corresponds to an
  // sh_link value of 0 (i.e. no linked section).
  SectionKind Link = SectionKind::last;

  bool shouldEmit() const {
    return AlwaysEmit || VirtualSize > 0 || FileSize > 0;
  }

  typedef void (*WriterFn)(Context &Ctxt, const OutputSection &OScn,
                           std::uint8_t *Data, const Layout &Lout);
  WriterFn Writer = nullptr;
};

} // end namespace rld

#endif // RLD_OUTPUT_SECTION_H
