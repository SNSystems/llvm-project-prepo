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
#include "llvm/ADT/DenseSet.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"

#include "rld/AdvanceEnum.h"
#include "rld/Contribution.h"
#include "rld/OutputSection.h"
#include "rld/SectionArray.h"
#include "rld/SectionKind.h"
#include "rld/SymbolEmitList.h"
#include "rld/symbol.h"

#include <condition_variable>
#include <cstdlib>
#include <memory>
#include <mutex>
#include <queue>

namespace rld {

class Context;
class SpecialNames;

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
  X(gnu_relro)                                                                 \
  /* non-loaded segments */                                                    \
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

#define RLD_SECTION_FILE_ORDER                                                 \
  RLD_X(SectionKind::read_only)                                                \
  RLD_X(SectionKind::mergeable_1_byte_c_string)                                \
  RLD_X(SectionKind::mergeable_2_byte_c_string)                                \
  RLD_X(SectionKind::mergeable_4_byte_c_string)                                \
  RLD_X(SectionKind::mergeable_const_4)                                        \
  RLD_X(SectionKind::mergeable_const_8)                                        \
  RLD_X(SectionKind::mergeable_const_16)                                       \
  RLD_X(SectionKind::mergeable_const_32)                                       \
  RLD_X(SectionKind::thread_data)                                              \
  RLD_X(SectionKind::thread_bss)                                               \
                                                                               \
  RLD_X(SectionKind::text)                                                     \
                                                                               \
  RLD_X(SectionKind::rel_ro)                                                   \
  RLD_X(SectionKind::init_array)                                               \
  RLD_X(SectionKind::fini_array)                                               \
  RLD_X(SectionKind::data)                                                     \
  RLD_X(SectionKind::got)                                                      \
  RLD_X(SectionKind::bss)                                                      \
                                                                               \
  RLD_X(SectionKind::debug_line)                                               \
  RLD_X(SectionKind::debug_string)                                             \
  RLD_X(SectionKind::debug_ranges)                                             \
  RLD_X(SectionKind::linked_definitions)                                       \
  RLD_X(SectionKind::gotplt)                                                   \
  RLD_X(SectionKind::plt)                                                      \
  RLD_X(SectionKind::rela_plt)                                                 \
  RLD_X(SectionKind::shstrtab)                                                 \
  RLD_X(SectionKind::strtab)                                                   \
  RLD_X(SectionKind::symtab)

extern const std::array<SectionKind const,
                        static_cast<std::underlying_type_t<SectionKind>>(
                            SectionKind::last)>
    SectionFileOrder;

template <typename Function, typename... Args>
inline void forEachSegmentKind(Function F, Args &&...args) {
  for (auto SegmentK = rld::firstSegmentKind();
       SegmentK != rld::SegmentKind::last; ++SegmentK) {
    F(SegmentK, std::forward<Args>(args)...);
  }
}
template <typename Function, typename... Args>
inline void forEachSectionKind(Function F, Args &&...args) {
  for (auto SectionK = rld::firstSectionKind();
       SectionK != rld::SectionKind::last; ++SectionK) {
    F(SectionK, std::forward<Args>(args)...);
  }
}

template <typename Function, typename... Args>
inline void forEachSectionKindInFileOrder(Function F, Args &&...args) {
  for (const auto SectionK : SectionFileOrder) {
    F(SectionK, std::forward<Args>(args)...);
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

  bool shouldEmit() const { return AlwaysEmit || HasOutputSections; }
};

template <typename Value>
using SegmentIndexedArray =
    EnumIndexedArray<SegmentKind, SegmentKind::last, Value>;

//-MARK: Layout
class Layout {
public:
  Layout();

  uint64_t HeaderBlockSize = 0;
  SectionIndexedArray<OutputSection> Sections;
  SegmentIndexedArray<Segment> Segments;

  template <typename Function> void forEachSegment(Function F) const {
    forEachSegmentImpl(*this, F);
  }
  template <typename Function> void forEachSegment(Function F) {
    forEachSegmentImpl(*this, F);
  }

  unsigned numberOfSections(unsigned NumImplicitSections) const {
    return std::accumulate(
        std::begin(Sections), std::end(Sections), NumImplicitSections,
        [](const unsigned Acc, const rld::OutputSection &OutScn) {
          return Acc + static_cast<unsigned>(OutScn.shouldEmit());
        });
  }
  unsigned numberOfSegments() const {
    return std::accumulate(std::begin(Segments), std::end(Segments), 0U,
                           [](const unsigned Acc, const rld::Segment &Segment) {
                             return Acc +
                                    static_cast<unsigned>(Segment.shouldEmit());
                           });
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

template <SectionKind SK> struct IsPstoreSectionKind {
  static constexpr auto value = false;
};
#define X(a)                                                                   \
  template <> struct IsPstoreSectionKind<SectionKind::a> {                     \
    static constexpr auto value = true;                                        \
  };
PSTORE_MCREPO_SECTION_KINDS
#undef X

//-MARK: LayoutBuilder
class LayoutBuilder {
public:
  LayoutBuilder(Context &Ctx, const NotNull<UndefsContainer *> Undefs);
  LayoutBuilder(const LayoutBuilder &) = delete;
  LayoutBuilder(LayoutBuilder &&) noexcept = delete;

  ~LayoutBuilder() noexcept = default;

  LayoutBuilder &operator=(const LayoutBuilder &) = delete;
  LayoutBuilder &operator=(LayoutBuilder &&) noexcept = delete;

  /// Call when a compilation scan has been completed.
  void visited(uint32_t Ordinal,
               std::tuple<CompilationSymbolsView, GOTPLTContainer> &&Locals);
  void endGroup() { CompilationWaiter_.done(); }

  /// The main layout thread entry point.
  void run();

  /// Signals that an error was encountered and stops the layout.
  void error();

  /// Takes the two-dimensional layout produced by the main layout thread where
  /// each segment starts at address 0 and flattens it into a single-dimensional
  /// structure where each segment follows the previous in memory.
  std::tuple<std::unique_ptr<Layout>, std::unique_ptr<GOTPLTContainer>>
  flattenSegments(uint64_t Base, uint64_t HeaderBlockSize);

  template <typename ELFT> uint64_t elfHeaderBlockSize() const;

  SymbolOrder symbolOrder() const {
    return {LocalEmit_, GlobalEmit_};
  }

  static SectionKind mapInputToOutputSection(SectionKind InputSection) {
    assert(SectionToSegment_[InputSection].InputSection == InputSection);
    return SectionToSegment_[InputSection].OutputSection;
  }

private:
  Context &Ctx_;
  const NotNull<UndefsContainer *> Undefs_;

  /// Implements the ordered processing of input files.
  ///
  /// As symbol resolution completes for each input file, the visit() member is
  /// called. Layout waits for each file in turn by calling waitFor() with a
  /// monotonically increasing value. This ensures that we perform layout for
  /// each compilation in-order even in the face of their scan phases being
  /// completed out-of-order.
  class Visited {
  public:
    Visited() = default;
    ///@{
    /// Producer API.

    /// Marks the file with the given ordinal as ready for layout.
    void fileCompleted(uint32_t Ordinal);
    /// Signals that the last input from the last group has been completed.
    /// Wakes up any waiting threads.
    void done();
    /// Signals that an error was encountered and wakes up any waiting threads.
    void error();
    ///@}

    ///@{
    /// Consumer API.

    /// Blocks until an the next input is available.
    /// \returns Has the next ordinal value on success. If the optional<> has no
    /// value, either there was an error or the last file ordinal was already
    /// returned.
    llvm::Optional<uint32_t> next();
    ///@}

    /// Returns true if an error was signalled via a call to error().
    bool hasError() const;

  private:
    /// Synchonizes access to members of this instance.
    mutable std::mutex Mut_;
    /// Synchonizes producer (symbol resolution) and consumer (layout) threads.
    std::condition_variable CV_;
    /// An ordered collection of the files ready for processing by layout.
    std::priority_queue<uint32_t, llvm::SmallVector<uint32_t, 256>,
                        std::greater<uint32_t>>
        Waiting_;
#ifndef NDEBUG
    llvm::DenseSet<uint32_t> Visited_;
#endif // NDEBUG
    uint32_t ConsumerOrdinal_ = 0;
    bool Done_ = false;
    bool Error_ = false;
  };
  Visited CompilationWaiter_;

  std::mutex CUsMut_;
  llvm::DenseMap<uint32_t, std::tuple<CompilationSymbolsView, GOTPLTContainer>>
      CUs_;

  struct SectionMapping {
    constexpr SectionMapping(SectionKind InputSection,
                             SectionKind OutputSection, SegmentKind Segment)
        :
#ifndef NDEBUG
          InputSection{InputSection},
#endif
          OutputSection{OutputSection}, Segment{Segment} {
    }
    constexpr SectionMapping(SectionKind InputSection, SegmentKind Segment)
        :
#ifndef NDEBUG
          InputSection{InputSection},
#endif
          OutputSection{InputSection}, Segment{Segment} {
    }

#ifndef NDEBUG
    const SectionKind InputSection;
#endif
    const SectionKind OutputSection;
    const SegmentKind Segment;
  };

  using SectionToSegmentArray =
      EnumIndexedArray<SectionKind, SectionKind::last, SectionMapping>;
  /// A table which maps input section kinds to output section and segment.
  static SectionToSegmentArray const SectionToSegment_;

  /// The Layout_ container is built as layout runs.
  std::unique_ptr<Layout> Layout_;
  /// The GOTPLTs_ container is built as each compilation finishes scan. If
  /// contains the GOT and PLT entries that have been allocated during the scan
  /// of that compilation.
  std::unique_ptr<GOTPLTContainer> GOTPLTs_;

  SymbolEmitList LocalEmit_;
  SymbolEmitList GlobalEmit_;

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

  /// \tparam InSection The section containing the input data to be added.
  /// \tparam OutSection The layout section to which the data will be added.
  ///
  /// \param Name  The name of the associated symbol.
  /// \param Body  The symbol body which defines the data to be added.
  /// \param IfxContributions
  /// \returns The contribution representing the newly added section data. This
  ///   is nullptr if the section is not copied to the output file.
  template <pstore::repo::section_kind InSection,
            SectionKind OutSection = ToRldSectionKind<InSection>::value>
  Contribution *addSectionToLayout(StringAddress Name, const Symbol::Body &Body,
                                   ContributionSpArray *const IfxContributions);

  template <SectionKind InSection, SectionKind OutSection = InSection>
  std::enable_if_t<!IsPstoreSectionKind<InSection>::value,
                   ContributionSpArray::iterator>
  addSymbolSection(ContributionSpArray::iterator CIt, Symbol *const Sym,
                   const StringAddress Name, const Symbol::Body &Body,
                   ContributionSpArray *const IfxContributions);

  template <SectionKind InSection, SectionKind OutSection = InSection>
  std::enable_if_t<IsPstoreSectionKind<InSection>::value,
                   ContributionSpArray::iterator>
  addSymbolSection(ContributionSpArray::iterator CIt, Symbol *const Sym,
                   const StringAddress Name, const Symbol::Body &Body,
                   ContributionSpArray *const IfxContributions);

  /// \param SKind The kind of the section to be added.
  /// \param Size  The number of bytes required for the section data.
  /// \param Alignment  The alignment of the section data.
  /// \returns  The OutputSection to which the section was added.
  OutputSection *addToOutputSection(SectionKind SKind, size_t Size,
                                    unsigned Alignment);

  std::tuple<CompilationSymbolsView, GOTPLTContainer>
  recoverDefinitionsFromCUMap(std::size_t Ordinal);

  void addSymbolBody(Symbol *const Sym, const Symbol::Body &Body,
                     uint32_t Ordinal, StringAddress Name,
                     const SpecialNames &Magics,
                     ContributionSpArray *IfxContributions);

  void addAliasSymbol(StringAddress Alias, StringAddress Aliasee,
                      SectionKind SectionK, bool Start);

  static std::uint64_t
  prevSectionEnd(OutputSection::ContributionVector const &Contributions);
  static void checkSectionToSegmentArray();

  void debugDumpLayout() const;
};

// elf header block size
// ~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT> uint64_t LayoutBuilder::elfHeaderBlockSize() const {
  using Elf_Ehdr = typename llvm::object::ELFFile<ELFT>::Elf_Ehdr;
  using Elf_Phdr = typename llvm::object::ELFFile<ELFT>::Elf_Phdr;

  Layout_->Segments[SegmentKind::phdr].AlwaysEmit = true;
  Layout_->Segments[SegmentKind::rodata].AlwaysEmit = true;
  Layout_->Segments[SegmentKind::gnu_stack].AlwaysEmit = true;

  return sizeof(Elf_Ehdr) + sizeof(Elf_Phdr) * Layout_->numberOfSegments();
}

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
