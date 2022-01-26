//===- include/rld/WorkItem.h -----------------------------*- mode: C++ -*-===//
//* __        __         _      ___ _                  *
//* \ \      / /__  _ __| | __ |_ _| |_ ___ _ __ ___   *
//*  \ \ /\ / / _ \| '__| |/ /  | || __/ _ \ '_ ` _ \  *
//*   \ V  V / (_) | |  |   <   | || ||  __/ | | | | | *
//*    \_/\_/ \___/|_|  |_|\_\ |___|\__\___|_| |_| |_| *
//*                                                    *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_WORKITEM_H
#define RLD_WORKITEM_H

#include "rld/Contribution.h"
#include "rld/LayoutBuilder.h"
#include "rld/MPMCQueue.h"
#include "rld/Variant.h"

namespace rld {

class Context;
class Layout;

/// Represents a job in the queue of blocks to be copied to the output.
struct WorkItem {
  using Pointer =
      Variant<OutputSection::ContributionVector::chunk::const_iterator,
              const Symbol *, UndefsContainer::Container::const_iterator>;

  using HandlerFn = void (*)(uint8_t *, Context &, const Layout &,
                             const GOTPLTContainer &,
                             const SectionIndexedArray<unsigned> &, Pointer,
                             Pointer);

  WorkItem() = default;
  WorkItem(uint8_t *Out, HandlerFn Handler) : Out{Out}, Handler{Handler} {}
  WorkItem(uint8_t *Out, HandlerFn Handler, Pointer First, Pointer Last)
      : Out{Out}, Handler{Handler}, First{First}, Last{Last} {}

  static void call(const WorkItem &W, Context &Context, const Layout &Layout,
                   const GOTPLTContainer &GOTPLTs,
                   const SectionIndexedArray<unsigned> &SectionToIndex) {
    (*W.Handler)(W.Out, Context, Layout, GOTPLTs, SectionToIndex, W.First,
                 W.Last);
  }

  // Where output should be written. If this value is null, the consumer job
  // exits.
  uint8_t *Out = nullptr;
  // Function responsible for performing the copy.
  HandlerFn Handler = nullptr;
  // The first contribution to be copied.
  Pointer First{OutputSection::ContributionVector::chunk::const_iterator{
      static_cast<Contribution const *>(nullptr)}};
  // The end of the range of contributions to be copied.
  Pointer Last{OutputSection::ContributionVector::chunk::const_iterator{
      static_cast<Contribution const *>(nullptr)}};
};

void workConsumer(MPMCQueue<WorkItem> &Q, Context &Context,
                  const Layout &Layout, const GOTPLTContainer &GOTPLTs,
                  const SectionIndexedArray<unsigned> &SectionToIndex);

} // end namespace rld

#endif // RLD_WORKITEM_H
