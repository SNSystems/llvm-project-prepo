//===- lib/WorkItem.cpp ---------------------------------------------------===//
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
#include "rld/WorkItem.h"

void rld::workConsumer(MPMCQueue<WorkItem> &Q, Context &Context,
                  const Layout &Layout, const GOTPLTContainer &GOTPLTs,
                  const SectionIndexedArray<unsigned> &SectionToIndex) {
  for (;;) {
    WorkItem W = Q.pop();
    if (W.Out == nullptr) {
      break;
    }
    WorkItem::call(W, Context, Layout, GOTPLTs, SectionToIndex);
  }
}
