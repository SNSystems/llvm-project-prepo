//*  _                            _   ____        _ _     _            *
//* | |    __ _ _   _  ___  _   _| |_| __ ) _   _(_) | __| | ___ _ __  *
//* | |   / _` | | | |/ _ \| | | | __|  _ \| | | | | |/ _` |/ _ \ '__| *
//* | |__| (_| | |_| | (_) | |_| | |_| |_) | |_| | | | (_| |  __/ |    *
//* |_____\__,_|\__, |\___/ \__,_|\__|____/ \__,_|_|_|\__,_|\___|_|    *
//*             |___/                                                  *
//===- lib/LayoutBuilder.cpp ----------------------------------------------===//
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
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>

namespace {

constexpr auto DebugType = "rld-LayoutBuilder";

} // end anonymous namespace

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
    Visited_.resize(NumCompilations + NumCompilations / 2U);
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
    {SectionKind::interp, SegmentKind::interp},
    {SectionKind::linked_definitions, SegmentKind::discard},
    {SectionKind::shstrtab, SegmentKind::discard}, // TODO:An unnecessary entry?
                                                   // Use the repo section enum?
    {SectionKind::strtab, SegmentKind::discard},   // TODO:An unnecessary entry?
    {SectionKind::symtab, SegmentKind::discard},   // TODO:An unnecessary entry?
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

constexpr auto PageSize = 0x1000U;

// (ctor)
// ~~~~~~
LayoutBuilder::LayoutBuilder(Context &Ctx,
                             const NotNull<GlobalsStorage *> Globals,
                             uint32_t NumCompilations)
    : Ctx_{Ctx}, Globals_{Globals}, NumCompilations_{NumCompilations},
      CompilationWaiter_{NumCompilations}, CUsMut_{}, CUs_{},
      Layout_{std::make_unique<Layout>()} {

  for (auto SegmentK = firstSegmentKind(); SegmentK < SegmentKind::last;
       ++SegmentK) {
    Layout_->Segments[SegmentK].MaxAlign = PageSize;
  }

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
std::uint64_t LayoutBuilder::prevSectionEnd(
    OutputSection::ContributionVector const &Contributions) {
  // TODO: initialize SI with an empty section to avoid this test?
  if (Contributions.empty()) {
    return 0;
  }
  Contribution const &last = Contributions.back();
  return last.Offset + last.Size;
}

template <SectionKind SK> struct ToPstoreSectionKind {};

#define X(a)                                                                   \
  template <> struct ToPstoreSectionKind<SectionKind::a> {                     \
    static constexpr auto value = pstore::repo::section_kind::a;               \
  };
PSTORE_MCREPO_SECTION_KINDS
#undef X

// has file data
// ~~~~~~~~~~~~~
template <pstore::repo::section_kind Kind> struct HasFileData {};
template <> struct HasFileData<pstore::repo::section_kind::text> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::data> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::rel_ro> {
  static constexpr bool value = true;
};
template <>
struct HasFileData<pstore::repo::section_kind::mergeable_1_byte_c_string> {
  static constexpr bool value = true;
};
template <>
struct HasFileData<pstore::repo::section_kind::mergeable_2_byte_c_string> {
  static constexpr bool value = true;
};
template <>
struct HasFileData<pstore::repo::section_kind::mergeable_4_byte_c_string> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::mergeable_const_4> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::mergeable_const_8> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::mergeable_const_16> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::mergeable_const_32> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::read_only> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::thread_data> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::debug_line> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::debug_string> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::debug_ranges> {
  static constexpr bool value = true;
};
template <> struct HasFileData<pstore::repo::section_kind::interp> {
  static constexpr bool value = true;
};

template <> struct HasFileData<pstore::repo::section_kind::bss> {
  static constexpr bool value = false;
};
template <> struct HasFileData<pstore::repo::section_kind::thread_bss> {
  static constexpr bool value = false;
};
template <> struct HasFileData<pstore::repo::section_kind::linked_definitions> {
  static constexpr bool value = false;
};
template <> struct HasFileData<pstore::repo::section_kind::last> {
  static constexpr bool value = false;
};

// add section to layout
// ~~~~~~~~~~~~~~~~~~~~~
template <pstore::repo::section_kind SKind>
uint64_t LayoutBuilder::addSectionToLayout(const FragmentPtr &F,
                                           const FragmentAddress FAddr,
                                           const StringAddress Name,
                                           const unsigned InputOrdinal) {
  assert(F->has_section(SKind) &&
         "Layout can't contain a section that doesn't exist");

  SegmentKind const SegmentK = SectionToSegment_[ToRldSectionKind<SKind>::value].second;
  if (SegmentK == SegmentKind::discard) {
    llvmDebug(DebugType, Ctx_.IOMut,
              [&] { llvm::dbgs() << "    Discarding " << SKind << '\n'; });
    return 0;
  }

  auto const & Section = F->at<SKind>();
  assert(reinterpret_cast<std::uint8_t const *>(&Section) >
         reinterpret_cast<std::uint8_t const *>(F.get()));

  auto const Alignment = pstore::repo::section_alignment(Section);
  auto const Size = pstore::repo::section_size(Section);

  Segment &Seg = Layout_->Segments[SegmentK];
  Seg.MaxAlign = std::max (Seg.MaxAlign, Alignment);
  Seg.VirtualSize = alignTo(Seg.VirtualSize, Alignment) + Size;
  if (HasFileData<SKind>::value) {
    Seg.FileSize = alignTo(Seg.FileSize, Alignment) + Size;
  }

  OutputSection *const OutputSection =
      &Layout_->Sections[ToRldSectionKind<SKind>::value];
  Seg.Sections[ToRldSectionKind<SKind>::value] = OutputSection;
  OutputSection->MaxAlign = std::max(OutputSection->MaxAlign, Alignment);
  OutputSection->VirtualSize =
      alignTo(OutputSection->VirtualSize, Alignment) + Size;
  if (HasFileData<SKind>::value) {
    OutputSection->FileSize =
        alignTo(OutputSection->FileSize, Alignment) + Size;
  }

  auto const SectionAddress = UintptrAddress{
      FAddr.to_address() + (reinterpret_cast<std::uint8_t const *>(&Section) -
                            reinterpret_cast<std::uint8_t const *>(F.get()))};

  // Compute the offset of this section within the output section.
  auto const Offset = alignTo(
      LayoutBuilder::prevSectionEnd(OutputSection->Contributions), Alignment);
  OutputSection->Contributions.emplace_back(&Section, SectionAddress,
                                            OutputSection, Offset, Size,
                                            Alignment, Name, InputOrdinal);

  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    auto const &Entry = OutputSection->Contributions.back();
    llvm::dbgs() << "    Adding " << SKind << " section to " << SegmentK
                 << " segment (" << Entry << ")\n";
  });
  return Offset;
}

// recover definitions from CU map
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// We were handed the compilation's definitions when its scan completed.
// Recover it from where we stashed it in the CUs_ map.
auto LayoutBuilder::recoverDefinitionsFromCUMap(const std::size_t Ordinal)
    -> LocalSymbolsContainer {
  std::lock_guard<std::mutex> const CUsLock{CUsMut_};
  assert(CUs_.find(Ordinal) != CUs_.end());
  LocalSymbolsContainer D = std::move(CUs_[Ordinal]);
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
    llvm::dbgs() << "  " << loadStdString(Ctx_.Db, Name) << '\n';
  });

  auto Value = uint64_t{0};
  for (pstore::repo::section_kind Section : *Body.fragment()) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    Value = this->addSectionToLayout<pstore::repo::section_kind::a>(           \
        Body.fragment(), Body.fragmentAddress(), Name, Body.inputOrdinal());   \
    break;

    switch (Section) {
      PSTORE_MCREPO_SECTION_KINDS
    case pstore::repo::section_kind::last:
      llvm_unreachable("Unknown fragment section kind");
    }
#undef X
    Sym->setValue(Value);
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

// run
// ~~~
void LayoutBuilder::run() {
  llvm::set_thread_name("LayoutBuilder");

  // Produce a GNU_STACK segment even though it will contain no data.
  Layout_->Segments[SegmentKind::gnu_stack].AlwaysEmit = true;

  for (auto Ordinal = uint32_t{0}; Ordinal < NumCompilations_; ++Ordinal) {
    CompilationWaiter_.waitFor(Ordinal);

    llvmDebug(DebugType, Ctx_.IOMut, [&] {
      llvm::dbgs() << "Finished scanning #" << Ordinal << '\n';
    });

    for (auto const &Definition : recoverDefinitionsFromCUMap(Ordinal)) {
      StringAddress const Name = Definition.first;
      Symbol *const Sym = Definition.second;

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

#if 0
{
    auto const & Fragment = Bodies.front().fragment();
    // TODO: make this a method of the fragment type.
    auto const FirstSection = static_cast <pstore::repo::section_kind> (Fragment->members().get_indices().front());
    assert (Fragment->has_section(FirstSection));
//Sym->Value = Fragment->members().get_indices().front();
}
#endif
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

        auto const Pos = std::lower_bound(
            First, Last, Ordinal, [](Symbol::Body const &A, uint32_t B) {
              return A.inputOrdinal() < B;
            });
        assert(Pos != Last && Pos->inputOrdinal() == Ordinal);
        this->addSymbolBody(Sym, *Pos, Ordinal, Name);
      }
    }
  }
}

// flatten segments
// ~~~~~~~~~~~~~~~~
std::unique_ptr<Layout> LayoutBuilder::flattenSegments() {
  auto Base = uint64_t{0x0000000000200000};
  assert(Base % PageSize == 0U);

  Layout_->forEachSegment([&Base](SegmentKind SegmentK, Segment &Seg) {
    assert(Seg.MaxAlign >= PageSize);
    Base = alignTo(Base, Seg.MaxAlign);
    assert(Seg.VirtualAddr == 0);
    Seg.VirtualAddr = Base;
    Base += Seg.VirtualSize;

    auto A = Seg.VirtualAddr;
    forEachSectionKind([&](SectionKind SectionK) {
      if (OutputSection *const Scn = Seg.Sections[SectionK]) {
        Seg.HasOutputSections = true;
        assert(Scn->VirtualAddr == 0);
        A = alignTo(A, Scn->MaxAlign);
        Scn->VirtualAddr = A;
        A += Scn->VirtualSize;
      }
    });
    assert(A == Base);
  });

  llvmDebug("rld-Layout", Ctx_.IOMut, [this] { debugDumpLayout(); });
  return std::move(Layout_);
}

} // namespace rld
