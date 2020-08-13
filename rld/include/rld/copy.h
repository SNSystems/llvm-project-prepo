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
    /// \param LO The data layout.
    /// \param SegmentDataOffsets  The absolute offset within the output buffer of each segment's data.
    void copyToOutput(
        Context &Ctxt,
        llvm::ThreadPool &Workers,
        uint8_t *Data,
        const LayoutOutput &LO,
        const SegmentIndexedArray<uint64_t> &SegmentDataOffsets
    );

} // end namespace rld

#endif // RLD_COPY_H
