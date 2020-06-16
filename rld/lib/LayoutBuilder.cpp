//*  _                            _   ____        _ _     _            *
//* | |    __ _ _   _  ___  _   _| |_| __ ) _   _(_) | __| | ___ _ __  *
//* | |   / _` | | | |/ _ \| | | | __|  _ \| | | | | |/ _` |/ _ \ '__| *
//* | |__| (_| | |_| | (_) | |_| | |_| |_) | |_| | | | (_| |  __/ |    *
//* |_____\__,_|\__, |\___/ \__,_|\__|____/ \__,_|_|_|\__,_|\___|_|    *
//*             |___/                                                  *
//===- lib/LayoutBuilder.cpp ----------------------------------------------===//
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
#include "rld/LayoutBuilder.h"

#include "rld/MathExtras.h"
#include "rld/context.h"

#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>

namespace {

constexpr auto DebugType = "rld-LayoutBuilder";

} // end anonymous namespace

namespace rld {

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

LayoutBuilder::Visited::Visited(uint32_t NumCompilations) {
  this->resize(NumCompilations);
}

void LayoutBuilder::Visited::visit(uint32_t Index) {
  std::lock_guard<std::mutex> const Lock{Mut_};
  // As archives are completed, file indices beyond the initial value will be
  // arriving here.
  this->resize(Index);
  Visited_[Index] = true;
  CV_.notify_one();
}

void LayoutBuilder::Visited::waitFor(uint32_t Index) {
  std::unique_lock<std::mutex> Lock{Mut_};
  CV_.wait(Lock, [this, Index] { return Visited_[Index]; });
  assert(Visited_[Index]);
}

void LayoutBuilder::Visited::resize(uint32_t NumCompilations) {
  if (NumCompilations > Visited_.size()) {
    // Add something to NumCompilations to allow for the fact that we can
    // anticipate additional compilations coming from archives.
    Visited_.resize(NumCompilations + NumCompilations / 2U);
  }
}

LayoutBuilder::SectionToSegmentArray const LayoutBuilder::SectionToSegment_{{
    {SectionKind::text, SegmentKind::text},
    {SectionKind::data, SegmentKind::data},
    {SectionKind::bss, SegmentKind::data},
    {SectionKind::rel_ro, SegmentKind::data},
    {SectionKind::mergeable_1_byte_c_string, SegmentKind::rodata},
    {SectionKind::mergeable_2_byte_c_string, SegmentKind::rodata},
    {SectionKind::mergeable_4_byte_c_string, SegmentKind::rodata},
    {SectionKind::mergeable_const_4, SegmentKind::rodata},
    {SectionKind::mergeable_const_8, SegmentKind::rodata},
    {SectionKind::mergeable_const_16, SegmentKind::rodata},
    {SectionKind::mergeable_const_32, SegmentKind::rodata},
    {SectionKind::read_only, SegmentKind::rodata},
    {SectionKind::thread_data, SegmentKind::tls},
    {SectionKind::thread_bss, SegmentKind::tls},
    {SectionKind::debug_line, SegmentKind::discard},
    {SectionKind::debug_string, SegmentKind::discard},
    {SectionKind::debug_ranges, SegmentKind::discard},
    {SectionKind::interp, SegmentKind::interp},
    {SectionKind::dependent, SegmentKind::discard},
    {SectionKind::shstrtab, SegmentKind::discard}, // TODO:An unnecessary entry?
                                                   // Use the repo section enum?
    {SectionKind::strtab, SegmentKind::discard},   // TODO:An unnecessary entry?
}};

// check section to segment array
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void LayoutBuilder::checkSectionToSegmentArray() {
#ifndef NDEBUG
  auto Index = 0;
  std::for_each(
      std::begin(SectionToSegment_), std::end(SectionToSegment_),
      [&Index](const SectionToSegmentArray::value_type &v) {
        auto const Section = v.first;
        assert(static_cast<std::underlying_type<decltype(Section)>::type>(
                   Section) == Index);
        ++Index;
      });
#endif
}

// (ctor)
// ~~~~~~
LayoutBuilder::LayoutBuilder(Context &Ctx,
                             const NotNull<GlobalsStorage *> Globals,
                             uint32_t NumCompilations)
    : Ctx_{Ctx}, Globals_{Globals}, NumCompilations_{NumCompilations},
      CompilationWaiter_{NumCompilations}, CUsMut_{}, CUs_{},
      Segments_{std::make_unique<LayoutOutput>()} {

  checkSectionToSegmentArray();
}

// visited
// ~~~~~~~
void LayoutBuilder::visited(uint32_t Index, LocalSymbolsContainer &&Locals) {
  {
    std::lock_guard<std::mutex> const CUsLock{CUsMut_};
    assert(CUs_.find(Index) == CUs_.end());

    auto const Res = CUs_.try_emplace(Index, std::move(Locals));
    (void)Res;
    assert(Res.second); // Ensure that the insertion happened.
  }
  CompilationWaiter_.visit(Index);
}

// prev section end
// ~~~~~~~~~~~~~~~~
std::uint64_t LayoutBuilder::prevSectionEnd(SectionInfoVector const &SI) {
  // TODO: initialize SI with an empty section to avoid this test?
  if (SI.empty()) {
    return 0;
  }
  SectionInfo const &last = SI.back();
  return last.Offset + last.Size;
}

template <SectionKind SK> struct ToPstoreSectionKind {};

#define X(a)                                                                   \
  template <> struct ToPstoreSectionKind<SectionKind::a> {                     \
    static constexpr auto value = pstore::repo::section_kind::a;               \
  };
PSTORE_MCREPO_SECTION_KINDS
#undef X

// update maximum
// ~~~~~~~~~~~~~~
template <typename T>
static inline void updateMaximum(std::atomic<T> &maximum_value,
                                 T const &value) noexcept {
  T prev_value = maximum_value;
  while (prev_value < value &&
         !maximum_value.compare_exchange_weak(prev_value, value,
                                              std::memory_order_relaxed)) {
  }
}

// add section to layout
// ~~~~~~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind SKind>
void LayoutBuilder::addSectionToLayout(
    FragmentPtr const &F,
    pstore::typed_address<pstore::repo::fragment> FragmentAddress) {
  assert(F->has_section(SKind) &&
         "Layout can't contain a section that doesn't exist");

  auto const SectionIndex =
      sectionNum(SKind); // Convert SectionKind to an array index.
  SegmentKind const SegmentK = SectionToSegment_[SectionIndex].second;
  if (SegmentK == SegmentKind::discard) {
    llvmDebug(DebugType, Ctx_.IOMut,
              [&] { llvm::dbgs() << "    Discarding " << SKind << '\n'; });
    return;
  }

  Segment &Seg = (*Segments_)[segmentNum(SegmentK)];
  SectionInfoVector &SI = Seg.Sections[SectionIndex];

  auto const *const Section = F->atp<SKind>();
  assert(Section != nullptr);
  assert(reinterpret_cast<std::uint8_t const *>(Section) >
         reinterpret_cast<std::uint8_t const *>(F.get()));
  auto const SectionAddress =
      UintptrAddress{FragmentAddress.to_address() +
                     (reinterpret_cast<std::uint8_t const *>(Section) -
                      reinterpret_cast<std::uint8_t const *>(F.get()))};

  auto const Alignment = pstore::repo::section_alignment(*Section);
  updateMaximum(Seg.MaxAlign, Alignment);
  SI.emplace_back(Section, SectionAddress,
                  alignTo(LayoutBuilder::prevSectionEnd(SI), Alignment),
                  pstore::repo::section_size(*Section), Alignment);

  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    auto const &Entry = SI.back();
    llvm::dbgs() << "    Adding " << SKind << " to segment " << SegmentK
                 << " (offset=" << Entry.Offset << ", size=" << Entry.Size
                 << ", align=" << Entry.Align << ")\n";
  });
}

// recover definitions from CU map
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// We were handed the compilation's definitions when its scan completed.
// Recover it from where we stashed it in the CUs_ map.
auto LayoutBuilder::recoverDefinitionsFromCUMap(std::size_t Ordinal)
    -> LocalSymbolsContainer {
  std::lock_guard<std::mutex> const CUsLock{CUsMut_};
  assert(CUs_.find(Ordinal) != CUs_.end());
  LocalSymbolsContainer D = std::move(CUs_[Ordinal]);
  CUs_.erase(Ordinal);
  return D;
}

// add body
// ~~~~~~~~
void LayoutBuilder::addBody(Symbol::Body const &Body, uint32_t Ordinal,
                            StringAddress const Name) {
  // Record this symbol's fragment if it was defined by this compilation.
  if (Body.inputOrdinal() != Ordinal) {
    return;
  }
  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    llvm::dbgs() << "  " << loadStdString(Ctx_.Db, Name) << '\n';
  });

  for (pstore::repo::section_kind Section : *Body.fragment()) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    this->addSectionToLayout<pstore::repo::section_kind::a>(                   \
        Body.fragment(), Body.fragmentAddress());                              \
    break;

    switch (Section) {
      PSTORE_MCREPO_SECTION_KINDS
    case pstore::repo::section_kind::last:
      llvm_unreachable("Unknown fragment section kind");
    }
#undef X
  }
}

// run
// ~~~
void LayoutBuilder::run() {
  llvm::set_thread_name("LayoutBuilder");

  // Produce a GNU_STACK segment even though it will contain no data.
  (*Segments_)[segmentNum(SegmentKind::gnu_stack)].AlwaysEmit = true;

  for (auto Ordinal = uint32_t{0}; Ordinal < NumCompilations_; ++Ordinal) {
    CompilationWaiter_.waitFor(Ordinal);

    llvmDebug(DebugType, Ctx_.IOMut, [&] {
      llvm::dbgs() << "Finished scanning #" << Ordinal << '\n';
    });

    for (auto const &Definition : recoverDefinitionsFromCUMap(Ordinal)) {
      StringAddress const Name = Definition.first;
      Symbol const *const Sym = Definition.second;
      // Get the symbol definition and a lock on the symbol table entry.
      auto const SymDef = Sym->definition();

      assert(std::get<Symbol::DefinitionIndex>(SymDef).hasValue() &&
             "Symbols that reach layout must be defined");

      auto &Bodies = *std::get<Symbol::DefinitionIndex>(SymDef);
      assert(Bodies.size() >= 1U &&
             "A defined symbol must have a least 1 body");
      if (Bodies.size() == 1U) {
        assert(Bodies.front().inputOrdinal() == Ordinal);
        this->addBody(Bodies.front(), Ordinal, Name);
      } else {
        auto First = std::begin(Bodies);
        auto Last = std::end(Bodies);

        assert(std::all_of(First, Last,
                           [](Symbol::Body const &B) {
                             return B.linkage() ==
                                    pstore::repo::linkage::append;
                           }) &&
               "Multiple bodies of a symbol must all have append linkage");

        auto const Pos = std::lower_bound(
            First, Last, Ordinal, [](Symbol::Body const &A, uint32_t B) {
              return A.inputOrdinal() < B;
            });
        assert(Pos != Last && Pos->inputOrdinal() == Ordinal);
        this->addBody(*Pos, Ordinal, Name);
      }
    }
  }

  llvmDebug(DebugType, Ctx_.IOMut,
            [&] { debugDumpSymbols(Ctx_, Globals_->all()); });
}

// flatten segments
// ~~~~~~~~~~~~~~~~
std::unique_ptr<LayoutOutput> LayoutBuilder::flattenSegments() {
  auto Base = std::uint64_t{0x0000000000200000};
  constexpr auto PageSize = std::uint64_t{0x1000};
  assert(Base % PageSize == 0U);

  for (auto &Segment : *Segments_) {
    Segment.VAddr = Base;
    for (auto &OutputSection : Segment.Sections) {
      for (SectionInfo &SI : OutputSection) {
        Base = alignTo(Base, Segment.MaxAlign);
        SI.OutputOffset = Base;
        Base += SI.Size;
      }
    }
    Segment.VSize = alignTo(Base - Segment.VAddr, PageSize);
    Base = alignTo(Base, PageSize);
  }

  return std::move(Segments_);
}

} // namespace rld
