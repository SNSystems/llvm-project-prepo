//===- include/rld/LayoutBuilder.h ------------------------*- mode: C++ -*-===//
//*  _                            _     ____        _ _     _            *
//* | |    __ _ _   _  ___  _   _| |_  | __ ) _   _(_) | __| | ___ _ __  *
//* | |   / _` | | | |/ _ \| | | | __| |  _ \| | | | | |/ _` |/ _ \ '__| *
//* | |__| (_| | |_| | (_) | |_| | |_  | |_) | |_| | | | (_| |  __/ |    *
//* |_____\__,_|\__, |\___/ \__,_|\__| |____/ \__,_|_|_|\__,_|\___|_|    *
//*             |___/                                                    *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_LAYOUT_BUILDER_H
#define RLD_LAYOUT_BUILDER_H

#include "llvm/ADT/DenseMap.h"

#include "rld/AdvanceEnum.h"
#include "rld/Contribution.h"
#include "rld/OutputSection.h"
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

template <typename Function> inline void forEachSegmentKind(Function F) {
  for (auto SegmentK = rld::firstSegmentKind();
       SegmentK != rld::SegmentKind::last; ++SegmentK) {
    F(SegmentK);
  }
}
template <typename Function> inline void forEachSectionKind(Function F) {
  for (auto SectionK = rld::firstSectionKind();
       SectionK != rld::SectionKind::last; ++SectionK) {
    F(SectionK);
  }
}

template <SectionKind SK> struct ToPstoreSectionKind {};
#define X(a)                                                                   \
  template <> struct ToPstoreSectionKind<SectionKind::a> {                     \
    static constexpr auto value = pstore::repo::section_kind::a;               \
  };
PSTORE_MCREPO_SECTION_KINDS
#undef X

template <typename Value>
using SectionIndexedArray =
    EnumIndexedArray<SectionKind, SectionKind::last, Value>;

//-MARK: Segment
struct Segment {
  Segment() : Sections{{nullptr}} {}

  SectionIndexedArray<OutputSection *> Sections;
  uint64_t VirtualAddr = 0;
  uint64_t VirtualSize = 0;
  uint64_t FileSize = 0;
  unsigned MaxAlign = 1U;
  bool AlwaysEmit = false;
  bool HasOutputSections = false;

  bool shouldEmit() const { return AlwaysEmit || VirtualSize > 0; }
};

template <typename Value>
using SegmentIndexedArray =
    EnumIndexedArray<SegmentKind, SegmentKind::last, Value>;

//-MARK: Layout
class Layout {
public:
  Layout();

  SectionIndexedArray<OutputSection> Sections;
  SegmentIndexedArray<Segment> Segments;

  template <typename Function> void forEachSegment(Function F) const {
    forEachSegmentImpl(*this, F);
  }
  template <typename Function> void forEachSegment(Function F) {
    forEachSegmentImpl(*this, F);
  }

private:
  template <typename LayoutType, typename Function>
  static void forEachSegmentImpl(LayoutType &Lout, Function F) {
    for (auto Seg = firstSegmentKind(); Seg != SegmentKind::last; ++Seg) {
      if (Seg != rld::SegmentKind::discard) {
        F(Seg, Lout.Segments[Seg]);
      }
    }
  }
};

//-MARK: LayoutBuilder
class LayoutBuilder {
public:
  LayoutBuilder(Context &Ctx, NotNull<GlobalsStorage *> const Globals,
                uint32_t NumCompilations);
  // no copying or assignment.
  LayoutBuilder(LayoutBuilder const &) = delete;
  LayoutBuilder(LayoutBuilder &&) noexcept = delete;
  LayoutBuilder &operator=(LayoutBuilder const &) = delete;
  LayoutBuilder &operator=(LayoutBuilder &&) noexcept = delete;

  /// Call when a compilation scan has been completed.
  void visited(uint32_t Ordinal, LocalSymbolsContainer &&Locals);

  /// The main layout thread entry point.
  void run();

  std::unique_ptr<Layout> flattenSegments();

private:
  Context &Ctx_;
  NotNull<GlobalsStorage *> const Globals_;
  /// The number of compilations that are to be scanned by the front-end.
  uint32_t const NumCompilations_;

  /// Implements the ordered processing of input files.
  ///
  /// As symbol resolution completes for each input file, the visit() member is
  /// called. Layout waits for each file in turn by calling waitFor() with a
  /// monotonically increasing value. This ensures that we perform layout for
  /// each compilation in-order even in the face of their scan phases being
  /// completed out-of-order.
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

  /// The Layout_ container is built as layout runs.
  std::unique_ptr<Layout> Layout_;

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

  /// \tparam SKind The section kind to be added. This must exist within
  ///   fragment \p F.
  /// \param F  The fragment which owns the section to be added.
  /// \param FAddr The pstore address of fragment \p F.
  /// \param Name
  /// \param InputOrdinal
  /// \returns The contribution representing the newly added section data. This
  /// is nullptr if the section is not copied to the output file.
  template <pstore::repo::section_kind SKind>
  Contribution *addSectionToLayout(FragmentPtr const &F, FragmentAddress FAddr,
                                   StringAddress Name, unsigned InputOrdinal);

  /// \param SKind The kind of the section to be added.
  /// \param Size  The number of bytes required for the section data.
  /// \param Alignment  The alignment of the section data.
  /// \returns  The OutputSection to which the section was added.
  OutputSection *addToOutputSection(SectionKind SKind, size_t Size,
                                    unsigned Alignment);

  LocalSymbolsContainer recoverDefinitionsFromCUMap(std::size_t Ordinal);
  void addSymbolBody(Symbol *const Sym, Symbol::Body const &Body,
                     uint32_t Ordinal, StringAddress const Name);

  static std::uint64_t
  prevSectionEnd(OutputSection::ContributionVector const &Contributions);
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
