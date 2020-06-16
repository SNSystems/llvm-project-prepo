//*  _                            _   ____        _ _     _            *
//* | |    __ _ _   _  ___  _   _| |_| __ ) _   _(_) | __| | ___ _ __  *
//* | |   / _` | | | |/ _ \| | | | __|  _ \| | | | | |/ _` |/ _ \ '__| *
//* | |__| (_| | |_| | (_) | |_| | |_| |_) | |_| | | | (_| |  __/ |    *
//* |_____\__,_|\__, |\___/ \__,_|\__|____/ \__,_|_|_|\__,_|\___|_|    *
//*             |___/                                                  *
//===- include/rld/LayoutBuilder.h ----------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
//===----------------------------------------------------------------------===//
#ifndef RLD_LAYOUT_BUILDER_H
#define RLD_LAYOUT_BUILDER_H

#include "llvm/ADT/DenseMap.h"

#include "pstore/adt/chunked_vector.hpp"

#include "rld/AdvanceEnum.h"
#include "rld/SectionKind.h"
#include "rld/symbol.h"

#include <condition_variable>
#include <cstdlib>
#include <memory>
#include <mutex>
#include <vector>

namespace rld {

class Context;

//*  ___                         _   _  ___         _  *
//* / __| ___ __ _ _ __  ___ _ _| |_| |/ (_)_ _  __| | *
//* \__ \/ -_) _` | '  \/ -_) ' \  _| ' <| | ' \/ _` | *
//* |___/\___\__, |_|_|_\___|_||_\__|_|\_\_|_||_\__,_| *
//*          |___/                                     *
#define RLD_SEGMENT_KIND                                                       \
  /* loaded segments */                                                        \
  X(phdr)                                                                      \
  X(data)                                                                      \
  X(rodata)                                                                    \
  X(text)                                                                      \
  X(tls)                                                                       \
  X(interp)                                                                    \
  /* non-loaded segments */                                                    \
  X(gnu_stack)                                                                 \
  X(discard)

// TODO: write a function which verifies that the loaded segments are together
// and before the non-loaded segments

enum class SegmentKind {
#define X(a) a,
  RLD_SEGMENT_KIND
#undef X
      last // Never used. Always last.
};

constexpr std::size_t NumSegments =
    static_cast<std::underlying_type<SegmentKind>::type>(SegmentKind::last);

// pre-increment
inline SegmentKind &operator++(SegmentKind &SK) noexcept {
#define X(x) SegmentKind::x,
  return SK = enum_values<SegmentKind,
                          RLD_SEGMENT_KIND SegmentKind::last>::advance(SK);
#undef X
}

// post-increment
inline SegmentKind operator++(SegmentKind &SK, int) noexcept {
  auto const prev = SK;
  ++SK;
  return prev;
}


struct SectionInfo {
  SectionInfo(pstore::repo::section_base const *S, UintptrAddress SA,
              std::uint64_t Offset_, std::uint64_t Size_, unsigned Align_)
      : Section{S}, XfxShadow{SA}, Offset{Offset_}, Size{Size_}, Align{Align_} {
  }

  pstore::repo::section_base const *Section;
  UintptrAddress XfxShadow;

  std::uint64_t Offset;
  std::uint64_t Size; // TODO: we really don't need 64-bits for the size of an
                      // individual section.
  unsigned Align;

  std::uint64_t OutputOffset;
};

using SectionInfoVector =
    pstore::chunked_vector<SectionInfo,
                           (32 * 1024 * 1024) / sizeof(SectionInfo)>;
struct Segment {
  Segment() : MaxAlign{1} {}
  std::array<SectionInfoVector, NumSectionKinds> Sections;
  std::uint64_t VAddr = 0;
  std::uint64_t VSize = 0;
  std::atomic<unsigned> MaxAlign;
  bool AlwaysEmit = false;

  bool shouldEmit() const { return AlwaysEmit || VSize > 0; }
};

using LayoutOutput = std::array<Segment, NumSegments>;

class LayoutBuilder {
public:
  LayoutBuilder(Context &Ctx, NotNull<GlobalsStorage *> const Globals,
                uint32_t NumCompilations);
  // no copying or assignment.
  LayoutBuilder(LayoutBuilder const &) = delete;
  LayoutBuilder &operator=(LayoutBuilder const &) = delete;

  /// Call when a compilation scan has been completed.
  void visited(uint32_t Ordinal, LocalSymbolsContainer &&Locals);

  /// The main layout thread entry point.
  void run();

  std::unique_ptr<LayoutOutput> flattenSegments();

private:
  Context &Ctx_;
  NotNull<GlobalsStorage *> const Globals_;
  uint32_t const NumCompilations_;

  /// Implements the ordered processing of input files.
  ///
  /// As symbol resolution completes for each input file, the visit() member is
  /// called. Layout waits for each file in turn by calling waitFor() with a
  /// monotonically increasing value.
  class Visited {
  public:
    explicit Visited(uint32_t NumCompilations);
    /// Marks given index as visited.
    void visit(uint32_t Index);
    /// Blocks until a specified index is visited.
    void waitFor(uint32_t Index);

  private:
    void resize(uint32_t NumCompilations);

    std::vector<bool> Visited_;
    std::mutex Mut_;
    std::condition_variable CV_;
  };
  Visited CompilationWaiter_;

  std::mutex CUsMut_;
  llvm::DenseMap<std::size_t, LocalSymbolsContainer> CUs_;

  using SectionToSegmentArray =
      std::array<std::pair<SectionKind, SegmentKind>, NumSectionKinds>;
  static SectionToSegmentArray const SectionToSegment_;

  /// The Segments_ container is built as layout runs.
  std::unique_ptr<LayoutOutput> Segments_;

  static constexpr auto sectionNum(pstore::repo::section_kind SKind)
      -> std::underlying_type<pstore::repo::section_kind>::type {
    return static_cast<std::underlying_type<pstore::repo::section_kind>::type>(
        SKind);
  }
  static constexpr auto segmentNum(SegmentKind Kind)
      -> std::underlying_type<SegmentKind>::type {
    return static_cast<std::underlying_type<SegmentKind>::type>(Kind);
  }

  template <pstore::repo::section_kind SKind>
  void addSectionToLayout(
      FragmentPtr const &F,
      pstore::typed_address<pstore::repo::fragment> FragmentAddress);

  LocalSymbolsContainer recoverDefinitionsFromCUMap(std::size_t Ordinal);
  void addBody(Symbol::Body const &Body, uint32_t Ordinal,
               StringAddress const Name);

  static std::uint64_t prevSectionEnd(SectionInfoVector const &SI);
  static void checkSectionToSegmentArray();
};

} // namespace rld
#endif // RLD_LAYOUT_BUILDER_H
