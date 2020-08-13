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
#include "rld/SectionArray.h"
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
  X(rodata)                                                                    \
  X(text)                                                                      \
  X(data)                                                                      \
  X(tls)                                                                       \
  /* non-loaded segments */                                                    \
  X(interp)                                                                    \
  X(gnu_stack)                                                                 \
  X(discard)

// TODO: write a function which verifies that the loaded segments are together
// and before the non-loaded segments

//-MARK: SegmentKind
enum class SegmentKind {
#define X(a) a,
  RLD_SEGMENT_KIND
#undef X
      last // Never used. Always last.
};

constexpr auto firstSegmentKind() noexcept -> SegmentKind {
  using utype = std::underlying_type<SegmentKind>::type;
  constexpr auto result = SegmentKind::phdr;
  static_assert(static_cast<utype>(result) == utype{0},
                "expected result to have value 0");
  return result;
}

template <typename OStream> OStream &operator<<(OStream &OS, SegmentKind S) {
  char const *Str = "unknown";
#define X(a)                                                                   \
  case SegmentKind::a:                                                         \
    Str = #a;                                                                  \
    break;

  switch (S) {
    RLD_SEGMENT_KIND
  case SegmentKind::last:
    llvm_unreachable("SegmentKind must not be last");
  }

#undef X
  return OS << Str;
}

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

//-MARK: SectionInfo
struct SectionInfo {
  SectionInfo(pstore::repo::section_base const *S, UintptrAddress SA,
              uint64_t Offset_, uint64_t Size_, unsigned Align_,
              StringAddress Name_, unsigned const InputOrdinal_)
      : Section{S}, XfxShadow{SA}, Offset{Offset_}, Size{Size_}, Align{Align_},
        InputOrdinal{InputOrdinal_}, Name{Name_} {}

  pstore::repo::section_base const *Section;
  UintptrAddress const XfxShadow;

  uint64_t const Offset;
  uint64_t const Size; // TODO: we really don't need 64-bits for the size of an
                 // individual section.
  unsigned const Align;
  unsigned const InputOrdinal;

  StringAddress const Name;
//  uint64_t VAddr = 0;
};

using SectionInfoVector =
    pstore::chunked_vector<SectionInfo,
                           (32 * 1024 * 1024) / sizeof(SectionInfo)>;

//-MARK: Segment
struct Segment {
  EnumIndexedArray<SectionKind, SectionKind::last, SectionInfoVector> Sections;
  uint64_t VirtualAddr = 0;
  uint64_t VirtualSize = 0;
  uint64_t FileSize = 0;
  unsigned MaxAlign = 1U;
  bool AlwaysEmit = false;

  bool shouldEmit() const { return AlwaysEmit || VirtualSize > 0; }
};

template <typename Value>
using SegmentIndexedArray =
    EnumIndexedArray<SegmentKind, SegmentKind::last, Value>;
using SegmentArray = SegmentIndexedArray<Segment>;
using LayoutOutput = SegmentArray;

namespace details {

// TODO: remove_cvref was introduced in C++20.
template <typename T> struct remove_cvref {
  typedef std::remove_cv_t<std::remove_reference_t<T>> type;
};
template <typename T> using remove_cvref_t = typename remove_cvref<T>::type;

template <typename LayoutOutputType, typename Function>
void for_each_segment(LayoutOutputType &LO, Function F) {
  for (auto Seg = firstSegmentKind(); Seg != SegmentKind::last; ++Seg) {
    if (Seg != rld::SegmentKind::discard) {
      F(Seg, LO[Seg]);
    }
  }
}

template <typename LayoutOutputType, typename Function>
void for_each_section(LayoutOutputType &LO, Function F) {
  for_each_segment(LO, [&F](SegmentKind SegmentK, auto &Seg) {
    static_assert(std::is_same<Segment, remove_cvref_t<decltype(Seg)>>::value,
                  "Expected Seg to be of type Segment");
    for (rld::SectionKind SKind = rld::firstSectionKind();
         SKind != rld::SectionKind::last; ++SKind) {
      F(SKind, Seg.Sections[SKind]);
    }
  });
}

template <typename LayoutOutputType, typename Function>
void for_each_contribution(LayoutOutputType &LO, Function F) {
  for_each_section(LO, [&F](SectionKind SKind, auto const &SI) {
    static_assert(
        std::is_same<SectionInfoVector, remove_cvref_t<decltype(SI)>>::value,
        "Expected SI to be of type SectionInfoVector");
    for (auto &Contribution : SI) {
      F(SKind, Contribution);
    }
  });
}

} // end namespace details

template <typename Function>
void for_each_segment(const LayoutOutput &LO, Function F) {
  details::for_each_segment(LO, F);
}
template <typename Function>
void for_each_segment(LayoutOutput &LO, Function F) {
  details::for_each_segment(LO, F);
}

template <typename Function>
void for_each_section(const LayoutOutput &LO, Function F) {
  details::for_each_section(LO, F);
}
template <typename Function>
void for_each_section(LayoutOutput &LO, Function F) {
  details::for_each_section(LO, F);
}

template <typename Function>
void for_each_contribution(const LayoutOutput &LO, Function F) {
  details::for_each_contribution(LO, F);
}
template <typename Function>
void for_each_contribution(LayoutOutput &LO, Function F) {
  details::for_each_contribution(LO, F);
}

//-MARK: LayoutBuilder
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
    EnumIndexedArray<SectionKind, SectionKind::last, std::pair<SectionKind, SegmentKind>>;

  static SectionToSegmentArray const SectionToSegment_;

  /// The Segments_ container is built as layout runs.
  std::unique_ptr<LayoutOutput> Segments_;

  static constexpr decltype(auto) sectionNum(pstore::repo::section_kind SKind) {
    return static_cast<std::underlying_type<pstore::repo::section_kind>::type>(
        SKind);
  }

  static constexpr decltype(auto) sectionNum(SectionKind SKind) {
    return static_cast<std::underlying_type<SectionKind>::type>(SKind);
  }
  static constexpr decltype(auto) segmentNum(SegmentKind Kind) {
    return static_cast<std::underlying_type<SegmentKind>::type>(Kind);
  }

  template <pstore::repo::section_kind SKind>
  void addSectionToLayout(FragmentPtr const &F, FragmentAddress FAddr,
                          StringAddress Name, unsigned InputOrdinal);

  LocalSymbolsContainer recoverDefinitionsFromCUMap(std::size_t Ordinal);
  void addSymbolBody(Symbol::Body const &Body, uint32_t Ordinal,
                     StringAddress const Name);

  static std::uint64_t prevSectionEnd(SectionInfoVector const &SI);
  static void checkSectionToSegmentArray();

  void debugDumpLayout() const;
};

class FileRegion {
public:
  constexpr FileRegion() = default;
  constexpr FileRegion(uint64_t Offset, uint64_t Size)
      : Offset_{Offset}, Size_{Size} {}

  constexpr uint64_t offset() const { return Offset_; }
  constexpr uint64_t size() const { return Size_; }
  constexpr uint64_t end() const { return offset() + size(); }

private:
  uint64_t Offset_ = 0U;
  uint64_t Size_ = 0U;
};

} // namespace rld
#endif // RLD_LAYOUT_BUILDER_H
