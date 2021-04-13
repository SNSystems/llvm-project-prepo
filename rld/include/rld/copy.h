//===- include/rld/copy.h ---------------------------------*- mode: C++ -*-===//
//*                         *
//*   ___ ___  _ __  _   _  *
//*  / __/ _ \| '_ \| | | | *
//* | (_| (_) | |_) | |_| | *
//*  \___\___/| .__/ \__, | *
//*           |_|    |___/  *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_COPY_H
#define RLD_COPY_H

#include "rld/LayoutBuilder.h"

#include <cstdint>

namespace llvm {
    class ThreadPool;
} // end namespace llvm

namespace rld {

    class Context;

    /// \param Ctxt The rld context.
    /// \param Workers  A pool of worker threads ready to accept jobs.
    /// \param Data The output buffer.
    /// \param Lout The data layout.
    /// \param SegmentDataOffsets  The absolute offset within the output buffer
    /// of each segment's data.
    void copyToOutput(
        Context &Ctxt, llvm::ThreadPool &Workers, uint8_t *const Data,
        const Layout &Lout, const LocalPLTsContainer &PLTs,
        const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
        uint64_t TargetDataOffset);

} // end namespace rld

#endif // RLD_COPY_H
