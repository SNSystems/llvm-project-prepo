//===- lib/LayoutBuilder.cpp ----------------------------------------------===//
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
#include "rld/LayoutBuilder.h"

#include "rld/Algorithm.h"
#include "rld/MathExtras.h"
#include "rld/context.h"

#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>

namespace {

constexpr auto DebugType = "rld-LayoutBuilder";

} // end anonymous namespace

// has file data
// ~~~~~~~~~~~~~
static constexpr bool hasFileData(pstore::repo::section_kind Kind) {
  using pstore::repo::section_kind;
  switch (Kind) {
  case section_kind::text:
  case section_kind::data:
  case section_kind::rel_ro:
  case section_kind::mergeable_1_byte_c_string:
  case section_kind::mergeable_2_byte_c_string:
  case section_kind::mergeable_4_byte_c_string:
  case section_kind::mergeable_const_4:
  case section_kind::mergeable_const_8:
  case section_kind::mergeable_const_16:
  case section_kind::mergeable_const_32:
  case section_kind::read_only:
  case section_kind::thread_data:
  case section_kind::debug_line:
  case section_kind::debug_string:
  case section_kind::debug_ranges:
    return true;

  case section_kind::bss:
  case section_kind::thread_bss:
  case section_kind::linked_definitions:
  case section_kind::last:
    return false;
  }
}

#define X(a)                                                                   \
  case rld::SectionKind::a:                                                    \
    return hasFileData(rld::ToPstoreSectionKind<rld::SectionKind::a>::value);

static constexpr bool hasFileData(rld::SectionKind Kind) {
  switch (Kind) {
    PSTORE_MCREPO_SECTION_KINDS
  case rld::SectionKind::rela_plt:
  case rld::SectionKind::gotplt:
  case rld::SectionKind::plt:
    return true;
  case rld::SectionKind::shstrtab:
  case rld::SectionKind::strtab:
  case rld::SectionKind::symtab:
  case rld::SectionKind::last:
    return false;
  }
  llvm_unreachable("Unknown section-kind");
}
#undef X

namespace rld {

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, Contribution const &C) {
  return OS << "offset:" << C.Offset << ", size:" << C.Size
            << ", align:" << C.Align;
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
    Visited_.resize(static_cast<size_t>(NumCompilations) +
                    NumCompilations / 2U);
  }
}

LayoutBuilder::SectionToSegmentArray const LayoutBuilder::SectionToSegment_{{
    SectionToSegmentArray::value_type{SectionKind::text, SegmentKind::text},
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
    {SectionKind::linked_definitions, SegmentKind::discard},
    {SectionKind::gotplt, SegmentKind::data},
    {SectionKind::plt, SegmentKind::text},
    {SectionKind::rela_plt, SegmentKind::rodata},
    {SectionKind::shstrtab, SegmentKind::discard},
    {SectionKind::strtab, SegmentKind::discard},
    {SectionKind::symtab, SegmentKind::discard},
}};

// (ctor)
// ~~~~~~
#define X(x) OutputSection{SectionKind::x},
#define RLD_X(x) X(x)
Layout::Layout() : Sections{{PSTORE_MCREPO_SECTION_KINDS RLD_SECTION_KINDS}} {}
#undef RLD_X
#undef X

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

constexpr auto PageSize = 0x1000U;

// (ctor)
// ~~~~~~
LayoutBuilder::LayoutBuilder(Context &Ctx,
                             const NotNull<GlobalsStorage *> Globals,
                             uint32_t NumCompilations)
    : Ctx_{Ctx}, Globals_{Globals}, NumCompilations_{NumCompilations},
      CompilationWaiter_{NumCompilations}, CUsMut_{}, CUs_{},
      Layout_{std::make_unique<Layout>()},
      PLTs_{std::make_unique<LocalPLTsContainer>()} {

  for (auto SegmentK = firstSegmentKind(); SegmentK < SegmentKind::last;
       ++SegmentK) {
    Layout_->Segments[SegmentK].MaxAlign = PageSize;
  }

  checkSectionToSegmentArray();
}

// visited
// ~~~~~~~
void LayoutBuilder::visited(
    uint32_t Index,
    std::tuple<LocalSymbolsContainer, LocalPLTsContainer> &&Locals) {
  {
    std::lock_guard<std::mutex> const _{CUsMut_};
    assert(CUs_.find(Index) == CUs_.end() &&
           "Should not complete work on a CU twice");

    auto const Res = CUs_.try_emplace(Index, std::move(Locals));
    (void)Res;
    assert(Res.second); // Ensure that the insertion happened.
  }
  CompilationWaiter_.visit(Index);
}

// prev section end
// ~~~~~~~~~~~~~~~~
std::uint64_t LayoutBuilder::prevSectionEnd(
    OutputSection::ContributionVector const &Contributions) {
  // TODO: initialize SI with an empty section to avoid this test?
  if (Contributions.empty()) {
    return 0;
  }
  Contribution const &last = Contributions.back();
  return last.Offset + last.Size;
}

// add to output section
// ~~~~~~~~~~~~~~~~~~~~~
OutputSection *LayoutBuilder::addToOutputSection(SectionKind SKind, size_t Size,
                                                 unsigned Alignment) {
  Segment &Seg = Layout_->Segments[SectionToSegment_[SKind].second];
  Seg.MaxAlign = std::max(Seg.MaxAlign, Alignment);

  OutputSection *const OutputSection = &Layout_->Sections[SKind];
  Seg.Sections[SKind] = OutputSection;
  OutputSection->MaxAlign = std::max(OutputSection->MaxAlign, Alignment);
  OutputSection->VirtualSize =
      alignTo(OutputSection->VirtualSize, Alignment) + Size;
  if (hasFileData(SKind)) {
    OutputSection->FileSize =
        alignTo(OutputSection->FileSize, Alignment) + Size;
  }
  return OutputSection;
}

// add section to layout
// ~~~~~~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind SKind>
Contribution *LayoutBuilder::addSectionToLayout(const Symbol::Body &Body,
                                                const StringAddress Name) {
  const FragmentPtr &F = Body.fragment();
  assert(F->has_section(SKind) &&
         "Layout can't contain a section that doesn't exist");

  SegmentKind const SegmentK = SectionToSegment_[ToRldSectionKind<SKind>::value].second;
  if (SegmentK == SegmentKind::discard) {
    llvmDebug(DebugType, Ctx_.IOMut,
              [&] { llvm::dbgs() << "    Discarding " << SKind << '\n'; });
    return nullptr;
  }

  Layout_->Segments[SegmentK].HasOutputSections = true;

  auto const & Section = F->at<SKind>();
  assert(reinterpret_cast<std::uint8_t const *>(&Section) >
         reinterpret_cast<std::uint8_t const *>(F.get()));

  auto const Size = pstore::repo::section_size(Section);
  auto const Alignment = pstore::repo::section_alignment(Section);

  OutputSection *const OutputSection =
      this->addToOutputSection(ToRldSectionKind<SKind>::value, Size, Alignment);

  const auto &RM = Body.resolveMap();

  OutputSection->Contributions.emplace_back(
      &Section, RM[SKind], OutputSection,
      alignTo(LayoutBuilder::prevSectionEnd(OutputSection->Contributions),
              Alignment),
      Size, Alignment, Name, Body.inputOrdinal());

  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    auto const &Entry = OutputSection->Contributions.back();
    llvm::dbgs() << "    Adding " << SKind << " section to " << SegmentK
                 << " segment (" << Entry << ")\n";
  });

  return &OutputSection->Contributions.back();
}

// recover definitions from CU map
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// We were handed the compilation's definitions when its scan completed.
// Recover it from where we stashed it in the CUs_ map.
auto LayoutBuilder::recoverDefinitionsFromCUMap(const std::size_t Ordinal)
    -> std::tuple<LocalSymbolsContainer, LocalPLTsContainer> {
  std::lock_guard<std::mutex> const _{CUsMut_};
  assert(CUs_.find(Ordinal) != CUs_.end());
  std::tuple<LocalSymbolsContainer, LocalPLTsContainer> D =
      std::move(CUs_[Ordinal]);
  CUs_.erase(Ordinal);
  return D;
}

// add symbol body
// ~~~~~~~~~~~~~~~
void LayoutBuilder::addSymbolBody(Symbol *const Sym, Symbol::Body const &Body,
                                  uint32_t Ordinal, StringAddress const Name) {
  // Record this symbol's fragment if it was defined by this compilation.
  if (Body.inputOrdinal() != Ordinal) {
    return;
  }
  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    llvm::dbgs() << "  Layout:" << loadStdString(Ctx_.Db, Name) << '\n';
  });

  Contribution *C = nullptr;
  for (pstore::repo::section_kind Section : *Body.fragment()) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    C = this->addSectionToLayout<pstore::repo::section_kind::a>(Body, Name);   \
    break;

    switch (Section) {
      PSTORE_MCREPO_SECTION_KINDS
    case pstore::repo::section_kind::last:
      llvm_unreachable("Unknown fragment section kind");
    }
#undef X
    Sym->setFirstContribution(C);
  }
}

// debug dump layout
// ~~~~~~~~~~~~~~~~~
void LayoutBuilder::debugDumpLayout() const {
  auto &OS = llvm::dbgs();

  auto EmitSegment = makeOnce([this, &OS](SegmentKind Seg) {
    auto const &Segment = Layout_->Segments[Seg];
    OS << Seg << '\t' << format_hex(Segment.VirtualAddr) << '\t'
       << format_hex(Segment.VirtualSize) << '\t'
       << format_hex(Segment.MaxAlign) << '\n';
  });
  auto EmitSection =
      makeOnce([&OS](SectionKind Scn) { OS << '\t' << Scn << '\n'; });

  pstore::shared_sstring_view Owner;
  forEachSegmentKind([&](SegmentKind SegmentK) {
    forEachSectionKind([&](SectionKind SectionK) {
      if (OutputSection const *const Scn =
              Layout_->Segments[SegmentK].Sections[SectionK]) {
        for (Contribution const &C : Scn->Contributions) {
          EmitSegment(SegmentK);
          EmitSection(SectionK);
          OS << "\t\t" << format_hex(C.Size) << '\t' << format_hex(C.Align)
             << '\t' << stringViewAsRef(loadString(Ctx_.Db, C.Name, &Owner))
             << '\t' << format_hex(C.Align) << '\t'
             << format_hex(C.InputOrdinal) << '\n';
        }
      }
      EmitSection.reset();
    });
    EmitSegment.reset();
  });
}

using ELFT = llvm::object::ELFType<llvm::support::little, true>;
using Elf_Rela = llvm::object::ELFFile<ELFT>::Elf_Rela;

// run
// ~~~
void LayoutBuilder::run() {
  llvm::set_thread_name("LayoutBuilder");

  for (auto Ordinal = uint32_t{0}; Ordinal < NumCompilations_; ++Ordinal) {
    CompilationWaiter_.waitFor(Ordinal);

    llvmDebug(DebugType, Ctx_.IOMut, [&] {
      llvm::dbgs() << "Finished scanning #" << Ordinal << '\n';
    });

    std::tuple<LocalSymbolsContainer, LocalPLTsContainer> PerCompilation =
        recoverDefinitionsFromCUMap(Ordinal);

    for (auto const &Definition :
         std::get<LocalSymbolsContainer>(PerCompilation)) {
      StringAddress const Name = Definition.first;
      auto *const Sym = std::get<Symbol *>(Definition.second);

      llvmDebug(DebugType, Ctx_.IOMut, [&] {
        llvm::dbgs() << "Examining symbol:" << loadStdString(Ctx_.Db, Name)
                     << '\n';
      });

      // Get the symbol definition and a lock on the symbol table entry.
      auto const SymDef = Sym->definition();

      assert(std::get<Symbol::DefinitionIndex>(SymDef).hasValue() &&
             "Symbols that reach layout must be defined");

      auto const &Bodies = *std::get<Symbol::DefinitionIndex>(SymDef);
      assert(Bodies.size() >= 1U &&
             "A defined symbol must have a least 1 body");

      if (Bodies.size() == 1U) {
        auto const &B = Bodies.front();
        if (B.inputOrdinal() == Ordinal) {
          this->addSymbolBody(Sym, B, Ordinal, Name);
        }
      } else {
        auto const First = std::begin(Bodies);
        auto const Last = std::end(Bodies);

        assert(std::all_of(First, Last,
                           [](Symbol::Body const &B) {
                             return B.linkage() ==
                                    pstore::repo::linkage::append;
                           }) &&
               "Multi-body symbols must all have append linkage");
        assert(std::is_sorted(First, Last,
                              [](Symbol::Body const &A, Symbol::Body const &B) {
                                return A.inputOrdinal() < B.inputOrdinal();
                              }) &&
               "Multi-body symbols must be sorted by input ordinal");

        auto const Pos = std::lower_bound(
            First, Last, Ordinal, [](Symbol::Body const &A, uint32_t B) {
              return A.inputOrdinal() < B;
            });
        assert(Pos != Last && Pos->inputOrdinal() == Ordinal);
        this->addSymbolBody(Sym, *Pos, Ordinal, Name);
      }
    }

    LocalPLTsContainer const &PLTs =
        std::get<LocalPLTsContainer>(PerCompilation);
    PLTs_->reserve(PLTs_->size() + PLTs.size());
    std::copy(std::begin(PLTs), std::end(PLTs), std::back_inserter(*PLTs_));
  } // input file loop.

  if (const unsigned PLTEntries = Ctx_.PLTEntries.load()) {
    assert(PLTEntries == PLTs_->size());
    std::sort(std::begin(*PLTs_), std::end(*PLTs_),
              [](const Symbol *const A, const Symbol *const B) {
                return A->name() < B->name();
              });
    auto Count = 0U;
    for (Symbol *const S : *PLTs_) {
      S->setPLTIndex(Count++);
    }

    llvmDebug(DebugType, Ctx_.IOMut, [&] {
      llvm::dbgs() << "PLT symbols:" << '\n';
      for (Symbol const *const PLTSym : *PLTs_) {
        llvm::dbgs() << "  " << loadStdString(Ctx_.Db, PLTSym->name()) << '\n';
      }
    });

    const size_t PLTSize = (size_t{PLTEntries} + 1U) * 16U;
    this->addToOutputSection(rld::SectionKind::plt, PLTSize, 8U);
    this->addToOutputSection(rld::SectionKind::gotplt, PLTSize, 8U);

    OutputSection *const rela_plt = this->addToOutputSection(
        rld::SectionKind::rela_plt, PLTEntries * sizeof(Elf_Rela), 8U);
    rela_plt->Link = rld::SectionKind::symtab;
    rela_plt->Info = rld::SectionKind::gotplt;
  }
}

static uint64_t positionOutputSections(uint64_t Base, Segment *const Seg,
                                       uint64_t VirtualSize,
                                       uint64_t FileSize) {
  auto A = Base;
  forEachSectionKind(
      [Seg, &A, &VirtualSize, &FileSize](const SectionKind SectionK) {
        if (OutputSection *const Scn = Seg->Sections[SectionK]) {
          const auto Alignment = Scn->MaxAlign;
          assert(Alignment <= Seg->MaxAlign);
          assert(Seg->HasOutputSections && "Segment HasOutputSections must be "
                                           "true if there are output sections");
          assert(Scn->VirtualAddr == 0);
          A = alignTo(A, Alignment);
          Scn->VirtualAddr = A;
          A += Scn->VirtualSize;
          VirtualSize = alignTo(VirtualSize, Alignment) + Scn->VirtualSize;
          if (hasFileData(SectionK)) {
            FileSize = alignTo(FileSize, Alignment) + Scn->FileSize;
          }
        }
      });

  assert(Seg->VirtualSize == 0);
  Seg->VirtualSize = VirtualSize;
  assert(Seg->FileSize == 0);
  Seg->FileSize = FileSize;
  return VirtualSize;
}

namespace {

template <SegmentKind SegmentK>
uint64_t positionSegment(uint64_t Base, Layout *const Layout) {
  Segment &Seg = Layout->Segments[SegmentK];
  assert(Seg.MaxAlign >= PageSize);
  Base = alignTo(Base, Seg.MaxAlign);
  assert(Seg.VirtualAddr == 0);
  Seg.VirtualAddr = Base;
  return Base + positionOutputSections(Base, &Seg, 0U, 0U);
}

template <>
uint64_t positionSegment<SegmentKind::phdr>(uint64_t Base,
                                            Layout *const Layout) {
  Segment &Seg = Layout->Segments[SegmentKind::phdr];
  Seg.VirtualAddr = Base;
  Seg.VirtualSize = Layout->HeaderBlockSize;
  Seg.FileSize = Layout->HeaderBlockSize;
  Seg.MaxAlign = 1;
  return Base;
}

template <>
uint64_t positionSegment<SegmentKind::rodata>(uint64_t Base,
                                              Layout *const Layout) {
  Segment &Seg = Layout->Segments[SegmentKind::rodata];
  assert(Base == alignTo(Base, Seg.MaxAlign));
  assert(Seg.MaxAlign >= PageSize);
  Seg.VirtualAddr = Base;
  return Base + positionOutputSections(Base + Layout->HeaderBlockSize, &Seg,
                                       Layout->HeaderBlockSize,
                                       Layout->HeaderBlockSize);
}

} // end anonymous namespace

// flatten segments
// ~~~~~~~~~~~~~~~~
std::tuple<std::unique_ptr<Layout>, std::unique_ptr<LocalPLTsContainer>>
LayoutBuilder::flattenSegments(uint64_t Base, const uint64_t HeaderBlockSize) {
  assert(Base % PageSize == 0U);
  Layout_->HeaderBlockSize = HeaderBlockSize;
#define X(SegK) Base = positionSegment<SegmentKind::SegK>(Base, Layout_.get());
  RLD_SEGMENT_KIND
#undef X
  llvmDebug("rld-Layout", Ctx_.IOMut, [this] { debugDumpLayout(); });
  return {std::move(Layout_), std::move(PLTs_)};
}

} // namespace rld
