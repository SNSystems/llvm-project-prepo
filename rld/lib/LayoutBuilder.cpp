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
#include "rld/SpecialNames.h"
#include "rld/context.h"

#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/raw_ostream.h"

#include <bitset>
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
  llvm_unreachable("Unknown pstore section kind");
}

#define X(a)                                                                   \
  case rld::SectionKind::a:                                                    \
    return hasFileData(rld::ToPstoreSectionKind<rld::SectionKind::a>::value);

static constexpr bool hasFileData(rld::SectionKind Kind) {
  switch (Kind) {
    PSTORE_MCREPO_SECTION_KINDS
  case rld::SectionKind::fini_array:
  case rld::SectionKind::got:
  case rld::SectionKind::gotplt:
  case rld::SectionKind::init_array:
  case rld::SectionKind::plt:
  case rld::SectionKind::rela_plt:
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

#define RLD_X(x) x,
const std::array<SectionKind const,
                 static_cast<std::underlying_type_t<SectionKind>>(
                     SectionKind::last)>
    SectionFileOrder = {{RLD_SECTION_FILE_ORDER}};
#undef RLD_X

void checkSectionFileOrder() {
#ifndef NDEBUG
  const auto member = [](std::bitset<SectionFileOrder.size()> S,
                         SectionKind SectionK) {
    const auto Index =
        static_cast<std::underlying_type_t<SectionKind>>(SectionK);
    assert(!S.test(Index));
    return S.set(Index);
  };
  std::bitset<SectionFileOrder.size()> SO;
#define X(a) SO = member(SO, SectionKind::a);
#define RLD_X(a) X(a)
  RLD_ALL_SECTION_KINDS
#undef RLD_X
#undef X
  assert(SO.all());
#endif // NDEBUG
}

//*  _                       _    *
//* | |   __ _ _  _ ___ _  _| |_  *
//* | |__/ _` | || / _ \ || |  _| *
//* |____\__,_|\_, \___/\_,_|\__| *
//*            |__/               *
// (ctor)
// ~~~~~~
#define X(x) OutputSection{SectionKind::x},
#define RLD_X(x) X(x)
Layout::Layout() : Sections{{RLD_ALL_SECTION_KINDS}} {}
#undef RLD_X
#undef X

//* __   ___    _ _          _  *
//* \ \ / (_)__(_) |_ ___ __| | *
//*  \ V /| (_-< |  _/ -_) _` | *
//*   \_/ |_/__/_|\__\___\__,_| *
//*                             *
// file completed
// ~~~~~~~~~~~~~~
void LayoutBuilder::Visited::fileCompleted(const uint32_t Ordinal) {
  const std::lock_guard<decltype(Mut_)> _{Mut_};
  assert(!Done_ && "Must not call fileCompleted() after done()");
  assert(Visited_.insert(Ordinal).second &&
         "Ordinal must not have been previously visted");
  Waiting_.push(Ordinal);
  CV_.notify_one();
}

// next
// ~~~~
llvm::Optional<uint32_t> LayoutBuilder::Visited::next() {
  std::unique_lock<decltype(Mut_)> Lock{Mut_};
  for (;;) {
    const auto IsEmpty = Waiting_.empty();
    if ((Done_ && IsEmpty) || Error_) {
      return llvm::None;
    }
    if (!IsEmpty && Waiting_.top() == ConsumerOrdinal_) {
      Waiting_.pop();
      return {ConsumerOrdinal_++};
    }
    CV_.wait(Lock);
  }
}

// done
// ~~~~
void LayoutBuilder::Visited::done() {
  const std::lock_guard<decltype(Mut_)> _{Mut_};
  Done_ = true;
  CV_.notify_all();
}

// error
// ~~~~~
void LayoutBuilder::Visited::error() {
  const std::lock_guard<decltype(Mut_)> _{Mut_};
  Error_ = true;
  CV_.notify_all();
}

// has error
// ~~~~~~~~~
bool LayoutBuilder::Visited::hasError() const {
  const std::lock_guard<decltype(Mut_)> _{Mut_};
  return Error_;
}

//*  _                       _     ___      _ _    _          *
//* | |   __ _ _  _ ___ _  _| |_  | _ )_  _(_) |__| |___ _ _  *
//* | |__/ _` | || / _ \ || |  _| | _ \ || | | / _` / -_) '_| *
//* |____\__,_|\_, \___/\_,_|\__| |___/\_,_|_|_\__,_\___|_|   *
//*            |__/                                           *

// For the most part, the type of the section in the input is mapped directly to
// the output. In the case of the mergeable... sections, these are translated to
// the read_only output section.
//
// Note that a target segment-kind of 'last' indicates that we don't expecting
// to be emitting output sections of this type.

LayoutBuilder::SectionToSegmentArray const LayoutBuilder::SectionToSegment_{{
    // The pstore section-kinds.
    SectionMapping{SectionKind::text, SegmentKind::text},
    {SectionKind::data, SegmentKind::data},
    {SectionKind::bss, SegmentKind::data},
    {SectionKind::rel_ro, SegmentKind::data},
    {SectionKind::mergeable_1_byte_c_string, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::mergeable_2_byte_c_string, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::mergeable_4_byte_c_string, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::mergeable_const_4, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::mergeable_const_8, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::mergeable_const_16, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::mergeable_const_32, SectionKind::read_only,
     SegmentKind::rodata},
    {SectionKind::read_only, SegmentKind::rodata},
    {SectionKind::thread_data, SegmentKind::tls},
    {SectionKind::thread_bss, SegmentKind::tls},
    {SectionKind::debug_line, SegmentKind::discard},
    {SectionKind::debug_string, SegmentKind::discard},
    {SectionKind::debug_ranges, SegmentKind::discard},
    {SectionKind::linked_definitions, SegmentKind::discard},
    // section-kinds added by rld.
    {SectionKind::init_array, SegmentKind::data},
    {SectionKind::fini_array, SegmentKind::data},
    {SectionKind::got, SegmentKind::data},
    {SectionKind::gotplt, SegmentKind::data},
    {SectionKind::plt, SegmentKind::text},
    {SectionKind::rela_plt, SegmentKind::rodata},
    {SectionKind::shstrtab, SegmentKind::discard},
    {SectionKind::strtab, SegmentKind::discard},
    {SectionKind::symtab, SegmentKind::discard},
}};

// check section to segment array
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void LayoutBuilder::checkSectionToSegmentArray() {
#ifndef NDEBUG
  auto Index = 0;
  for (const SectionMapping &V : SectionToSegment_) {
    const SectionKind Section = V.InputSection;
    assert(static_cast<std::underlying_type<SectionKind>::type>(Section) ==
               Index &&
           "SectionToSegmentArray order is not correct");
    assert(V.Segment == SectionToSegment_[V.OutputSection].Segment &&
           "Segment mapping is not consistent");
    ++Index;
  }
#endif
}

constexpr auto PageSize = 0x1000U;

// (ctor)
// ~~~~~~
LayoutBuilder::LayoutBuilder(Context &Ctx,
                             const NotNull<UndefsContainer *> Undefs)
    : Ctx_{Ctx}, Undefs_{Undefs}, CUsMut_{}, CUs_{},
      Layout_{std::make_unique<Layout>()},
      GOTPLTs_{std::make_unique<GOTPLTContainer>()}, LocalEmit_{},
      GlobalEmit_{} {

  for (auto SegmentK = firstSegmentKind(); SegmentK < SegmentKind::last;
       ++SegmentK) {
    Layout_->Segments[SegmentK].MaxAlign = PageSize;
  }
  Layout_->Segments[SegmentKind::gnu_relro].MaxAlign = 1U;
  checkSectionFileOrder();

  this->checkSectionToSegmentArray();
}

// visited
// ~~~~~~~
void LayoutBuilder::visited(
    uint32_t Index,
    std::tuple<CompilationSymbolsView, GOTPLTContainer> &&Locals) {
  {
    std::lock_guard<std::mutex> const _{CUsMut_};
    assert(CUs_.find(Index) == CUs_.end() &&
           "Should not complete work on a CU twice");

    auto const Res = CUs_.try_emplace(Index, std::move(Locals));
    (void)Res;
    assert(Res.second); // Ensure that the insertion happened.
  }
  CompilationWaiter_.fileCompleted(Index);
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
  assert(SectionToSegment_[SKind].InputSection == SKind &&
         "InputSection->OutputSection table error");
  assert(SectionToSegment_[SKind].Segment ==
         SectionToSegment_[SectionToSegment_[SKind].OutputSection].Segment);

  // Map from the input-section to segment.
  Segment &Seg = Layout_->Segments[SectionToSegment_[SKind].Segment];
  Seg.HasOutputSections = true;

  // Map from the input-section to the output-section. For most this has no
  // effect but sections such as mergeable_const_4 are translated to the
  // read_only section.
  SKind = SectionToSegment_[SKind].OutputSection;

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
template <pstore::repo::section_kind InSection, SectionKind OutSection>
Contribution *
LayoutBuilder::addSectionToLayout(const StringAddress Name,
                                  const Symbol::Body &Body,
                                  ContributionSpArray *const IfxContributions) {
  const FragmentPtr &F = Body.fragment();
  assert(F->has_section(InSection) &&
         "Layout can't contain a section that doesn't exist");

  const SegmentKind SegmentK = SectionToSegment_[OutSection].Segment;
  assert(SegmentK != SegmentKind::last);
  if (SegmentK == SegmentKind::discard) {
    llvmDebug(DebugType, Ctx_.IOMut,
              [&] { llvm::dbgs() << "    Discarding " << InSection << '\n'; });
    return nullptr;
  }

  const auto &Section = F->at<InSection>();
  assert(reinterpret_cast<const uint8_t *>(&Section) >
         reinterpret_cast<const uint8_t *>(F.get()));

  const auto Size = pstore::repo::section_size(Section);
  const auto Alignment = pstore::repo::section_alignment(Section);

  OutputSection *const OutputSection =
      this->addToOutputSection(OutSection, Size, Alignment);

  const auto &RM = Body.resolveMap();

  OutputSection->Contributions.emplace_back(
      &Section,      // The contribution's fragment section.
      RM[InSection], // The array of external fixups to be applied when copying
                     // the contribution.
      IfxContributions, // The contributions for the sections of fragment 'F'.
      OutputSection,    // The output section to which the contribution belongs.
      alignTo(LayoutBuilder::prevSectionEnd(OutputSection->Contributions),
              Alignment), // Offset
      Size, Alignment, Body.inputOrdinal(), ToRldSectionKind<InSection>::value,
      Name);

  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    const auto &Entry = OutputSection->Contributions.back();
    llvm::dbgs() << "    Adding " << InSection << " section to " << SegmentK
                 << " segment (" << Entry << ")\n";
  });

  return &OutputSection->Contributions.back();
}

// recover definitions from CU map
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// We were handed the compilation's definitions when its scan completed.
// Recover it from where we stashed it in the CUs_ map.
auto LayoutBuilder::recoverDefinitionsFromCUMap(const std::size_t Ordinal)
    -> std::tuple<CompilationSymbolsView, GOTPLTContainer> {
  std::lock_guard<std::mutex> const _{CUsMut_};
  const auto Pos = CUs_.find(Ordinal);
  assert(Pos != CUs_.end() && "Ordinal must be in the CUs map");
  std::tuple<CompilationSymbolsView, GOTPLTContainer> D =
      std::move(Pos->second);
  CUs_.erase(Pos);
  return D;
}

template <SectionKind InSection, SectionKind OutSection>
std::enable_if_t<!IsPstoreSectionKind<InSection>::value,
                 ContributionSpArray::iterator>
LayoutBuilder::addSymbolSection(ContributionSpArray::iterator CIt,
                                Symbol *const Sym, const StringAddress Name,
                                const Symbol::Body &Body,
                                ContributionSpArray *const IfxContributions) {
  return CIt;
}

template <SectionKind InSection, SectionKind OutSection>
std::enable_if_t<IsPstoreSectionKind<InSection>::value,
                 ContributionSpArray::iterator>
LayoutBuilder::addSymbolSection(ContributionSpArray::iterator CIt,
                                Symbol *const Sym, const StringAddress Name,
                                const Symbol::Body &Body,
                                ContributionSpArray *const IfxContributions) {
  constexpr auto InPstoreKind = ToPstoreSectionKind<InSection>::value;
  if (Body.fragment()->has_section(InPstoreKind)) {
    Contribution *const C = this->addSectionToLayout<InPstoreKind, OutSection>(
        Name, Body, IfxContributions);
    Sym->setFirstContribution(C);

    if (IfxContributions != nullptr) {
      // Now that we've got a contribution record for this section, we can
      // record it in the fragment's sparse array.
      assert(IfxContributions->has_index(InPstoreKind) &&
             "The array does not have an index for one of the fragment's "
             "sections");
      //      assert(&(*IfxContributions)[PstoreKind] == &(*CIt) &&
      //             "The IfxContribution iterator was not correct");
      //      *CIt = C;
      //      ++CIt;

      assert((*IfxContributions)[InPstoreKind] == ContributionEntry{});
      (*IfxContributions)[InPstoreKind] = ContributionEntry{C, InPstoreKind};
    }
  }
  return CIt;
}

// add symbol body
// ~~~~~~~~~~~~~~~
void LayoutBuilder::addSymbolBody(Symbol *const Sym, const Symbol::Body &Body,
                                  const uint32_t Ordinal,
                                  const StringAddress Name,
                                  const SpecialNames &Magics,
                                  ContributionSpArray *const IfxContributions) {
  // Record this symbol's fragment if it was defined by this compilation.
  if (Body.inputOrdinal() != Ordinal) {
    return;
  }
  llvmDebug(DebugType, Ctx_.IOMut, [&] {
    llvm::dbgs() << "  Layout:" << loadStdString(Ctx_.Db, Name) << '\n';
  });

  if (isLocalLinkage(Body.linkage())) {
    LocalEmit_.append(Sym);
  } else {
    GlobalEmit_.append(Sym);
  }

  ContributionSpArray::iterator CIt{};
  if (IfxContributions != nullptr) {
    CIt = IfxContributions->begin();
  }

  if (Name == Magics.GlobalCtors) {
    this->addSymbolSection<SectionKind::read_only, SectionKind::init_array>(
        CIt, Sym, Name, Body, IfxContributions);
    return;
  }

#define X(x)                                                                   \
  CIt = this->addSymbolSection<SectionKind::x>(CIt, Sym, Name, Body,           \
                                               IfxContributions);
#define RLD_X(x) X(x)
  RLD_ALL_SECTION_KINDS
#undef RLD_X
#undef X
}

// debug dump layout
// ~~~~~~~~~~~~~~~~~
void LayoutBuilder::debugDumpLayout() const {
  auto &OS = llvm::dbgs();

  auto EmitSegment = makeOnce([this, &OS](const SegmentKind Seg) {
    const auto &Segment = Layout_->Segments[Seg];
    OS << Seg << '\t' << format_hex(Segment.VirtualAddr) << '\t'
       << format_hex(Segment.VirtualSize) << '\t'
       << format_hex(Segment.MaxAlign) << '\n';
  });
  auto EmitSection = makeOnce([&OS, this](const SectionKind Scn) {
    const auto &Section = Layout_->Sections[Scn];
    OS << '\t' << Scn << '\t' << format_hex(Section.VirtualSize) << '\t'
       << format_hex(Section.MaxAlign) << '\n';
  });

  pstore::shared_sstring_view Owner;
  forEachSegmentKind([&](const SegmentKind SegmentK) {
    auto VAddr = Layout_->Segments[SegmentK].VirtualAddr;
    forEachSectionKindInFileOrder([&, this](const SectionKind SectionK) {
      if (const OutputSection *const Scn =
              Layout_->Segments[SegmentK].Sections[SectionK]) {
        for (const Contribution &C : Scn->Contributions) {
          EmitSegment(SegmentK);
          EmitSection(SectionK);
          VAddr = alignTo(VAddr, C.Align);
          OS << "\t\t" << format_hex(VAddr) << '\t' << format_hex(C.Size)
             << '\t' << format_hex(C.Align) << '\t'
             << stringViewAsRef(loadString(Ctx_.Db, C.Name, &Owner)) << '\t'
             << Ctx_.ordinalName(C.InputOrdinal) << '\n';
          VAddr += C.Size;
        }
      }
      EmitSection.reset();
    });
    EmitSegment.reset();
  });
}


void LayoutBuilder::addAliasSymbol(const StringAddress Alias,
                                   const StringAddress Aliasee,
                                   const SectionKind SectionK,
                                   const bool Start) {
#if 0
  if (Alias == StringAddress::null() || Aliasee == StringAddress::null()) {
    return;
  }
  setSymbolShadow(
      symbolShadow(Ctx_, Alias),
      []() -> Symbol * {
        return nullptr; // Symbol doesn't exist. Do nothing.
      },
      [&](Symbol *Sym) -> Symbol * {
        if (Sym->hasDefinition()) {
          // We already have a definition. Do nothing.
          return Sym;
        }
        auto *const Ctors = symbolShadow(Ctx_, Aliasee);
        // The shadow memory pointer may be null if the string is known, but
        // isn't used as the name of a symbol.
        if (const Symbol *const AliaseeSym = *Ctors) {
          const auto CtorsDef = AliaseeSym->definition();
          if (const auto &Bodies =
                  std::get<const Symbol::OptionalBodies &>(CtorsDef)) {
            const Symbol::Body &FirstBody = Bodies->front();

            constexpr auto InputOrdinal = std::numeric_limits<uint32_t>::max();
            SymbolResolver Resolver{Ctx_};
            Sym = Resolver.updateSymbol(Sym, Undefs_, FirstBody.definition(),
                                        InputOrdinal);
            assert(Sym != nullptr);
            if (Start) {
              Sym->setFirstContribution(
                  &Layout_->Sections[SectionK].Contributions.front());
            } else {
              unsigned Alignment = 1;
              unsigned Size = 0;

              OutputSection *const OutputSection = &Layout_->Sections[SectionK];
              OutputSection->Contributions.emplace_back(
                  nullptr, // The contribution's fragment section.
                  nullptr, // The array of external fixups to be applied when
                           // copying the contribution.
                  nullptr, // The contributions for the sections.
                  OutputSection, // The output section to which the contribution
                                 // belongs.
                  alignTo(LayoutBuilder::prevSectionEnd(
                              OutputSection->Contributions),
                          Alignment), // Offset
                  Size, Alignment, InputOrdinal, SectionK, Alias);
              Sym->setFirstContribution(&OutputSection->Contributions.back());
            }

            // c.f. addSymbolBody
            if (isLocalLinkage(FirstBody.linkage())) {
              LocalEmit_.append(Sym);
            } else {
              GlobalEmit_.append(Sym);
            }
          }
        }
        return Sym;
      });
#endif
}

// error
// ~~~~~
void LayoutBuilder::error() { CompilationWaiter_.error(); }

// run
// ~~~
void LayoutBuilder::run() {
  llvm::set_thread_name("LayoutBuilder");

  SpecialNames Magics;
  Magics.initialize(Ctx_.Db);

  while (const llvm::Optional<uint32_t> InputOrdinal =
             CompilationWaiter_.next()) {
    const uint32_t Ordinal = *InputOrdinal;

    llvmDebug(DebugType, Ctx_.IOMut, [&] {
      llvm::dbgs() << "Starting layout for #" << Ordinal << '\n';
    });

    std::tuple<CompilationSymbolsView, GOTPLTContainer> PerCompilation =
        recoverDefinitionsFromCUMap(Ordinal);

    for (auto const &Definition :
         std::get<CompilationSymbolsView>(PerCompilation).Map) {
      StringAddress const Name = Definition.first;
      Symbol *const Sym = Definition.second.Sym;
      ContributionSpArray *const IfxContributions = Definition.second.Ifx;

      // TODO: optimize so that this is nullptr if there are no internal fixups.
      llvmDebug(DebugType, Ctx_.IOMut, [&] {
        llvm::dbgs() << "Examining symbol:" << loadStdString(Ctx_.Db, Name)
                     << '\n';
      });

      // Get the symbol definition and a lock on the symbol table entry.
      auto const SymDef = Sym->definition();

      assert(std::get<Symbol::OptionalBodies &>(SymDef).hasValue() &&
             "Symbols that reach layout must be defined");

      auto const &Bodies = *std::get<Symbol::OptionalBodies &>(SymDef);
      assert(Bodies.size() >= 1U &&
             "A defined symbol must have a least 1 body");

      if (Bodies.size() == 1U) {
        auto const &B = Bodies.front();
        assert(!B.hasLocalLinkage() || B.inputOrdinal() == Ordinal);
        this->addSymbolBody(Sym, B, Ordinal, Name, Magics, IfxContributions);
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
        this->addSymbolBody(Sym, *Pos, Ordinal, Name, Magics, IfxContributions);
      }
    }

    GOTPLTs_->append(std::get<GOTPLTContainer>(PerCompilation));
  } // input file loop.

  if (CompilationWaiter_.hasError()) {
    // An error was signalled.
    llvmDebug(DebugType, Ctx_.IOMut,
              [&] { llvm::dbgs() << "An error was encountered. Stopping.\n"; });
    return;
  }

#if 0
  using ELFT = llvm::object::ELF64LE;
  using Elf_Rela = llvm::object::ELFFile<ELFT>::Elf_Rela;
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
      for (const Symbol *const PLTSym : *PLTs_) {
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
#endif // PLT
  if (const unsigned GOTEntries = Ctx_.GOTEntries.load()) {
    assert(GOTEntries == GOTPLTs_->GOT.size());
    // FIXME: performing a sort on the layout thread will be slow. Use something
    // other than a crude vector to record the symbol pointers.
    std::sort(std::begin(GOTPLTs_->GOT), std::end(GOTPLTs_->GOT),
              [](const Symbol *const A, const Symbol *const B) {
                return A->name() < B->name();
              });
    auto Count = 0U;
    for (Symbol *const S : GOTPLTs_->GOT) {
      S->setGOTIndex(Count++);
    }

    llvmDebug(DebugType, Ctx_.IOMut, [&] {
      llvm::dbgs() << "GOT symbols:" << '\n';
      std::for_each(std::begin(GOTPLTs_->GOT), std::end(GOTPLTs_->GOT),
                    [this](const Symbol *const Sym) {
                      llvm::dbgs()
                          << "  " << loadStdString(Ctx_.Db, Sym->name())
                          << '\n';
                    });
    });

    // Each entry is a 64-bit address. The zeroth element is reserved to hold
    // the address of the dynamic structure, referenced with the symbol
    // _DYNAMIC.
    this->addToOutputSection(rld::SectionKind::got,
                             (size_t{GOTEntries} + 1U) * 8U, 8U);
  }

  this->addAliasSymbol(Magics.InitArrayStart, Magics.GlobalCtors,
                       SectionKind::init_array, true);
  this->addAliasSymbol(Magics.InitArrayEnd, Magics.GlobalCtors,
                       SectionKind::init_array, false);

  this->addAliasSymbol(Magics.FiniArrayStart, Magics.GlobalDtors,
                       SectionKind::fini_array, true);
  this->addAliasSymbol(Magics.FiniArrayEnd, Magics.GlobalDtors,
                       SectionKind::fini_array, false);

  LocalEmit_.last();
  GlobalEmit_.last();
}

// compute segment size
// ~~~~~~~~~~~~~~~~~~~~
static std::pair<uint64_t, uint64_t> computeSegmentSize(const Segment &Seg,
                                                        uint64_t VirtualSize,
                                                        uint64_t FileSize) {
  forEachSectionKindInFileOrder([&](const SectionKind SectionK) {
    if (const OutputSection *const Scn = Seg.Sections[SectionK]) {
      assert(Seg.HasOutputSections && "Segment HasOutputSections must be true "
                                      "if there are output sections");
      const auto Alignment = Scn->MaxAlign;
      VirtualSize = alignTo(VirtualSize, Alignment) + Scn->VirtualSize;
      if (hasFileData(SectionK)) {
        FileSize = alignTo(FileSize, Alignment) + Scn->FileSize;
      }
    }
  });
  return std::make_pair(VirtualSize, FileSize);
}

static std::pair<uint64_t, uint64_t> computeSegmentSize(const Segment &Seg,
                                                        uint64_t Bias) {
  return computeSegmentSize(Seg, Bias, Bias);
}

// position output sections
// ~~~~~~~~~~~~~~~~~~~~~~~~
static uint64_t positionOutputSections(uint64_t Addr, Segment *const Seg) {
  forEachSectionKindInFileOrder([&](const SectionKind SectionK) {
    if (OutputSection *const Scn = Seg->Sections[SectionK]) {
      assert(Seg->HasOutputSections && "Segment HasOutputSections must be true "
                                       "if there are output sections");
      const auto Alignment = Scn->MaxAlign;
      assert(Scn->VirtualAddr == 0);
      Addr = alignTo(Addr, Alignment);
      Scn->VirtualAddr = Addr;
      Addr += Scn->VirtualSize;
    }
  });
  return Addr;
}

namespace {

template <SegmentKind SegmentK>
uint64_t positionSegment(uint64_t Base, Layout *const Layout) {
  Segment &Seg = Layout->Segments[SegmentK];
  Base = alignTo(Base, Seg.MaxAlign);
  Seg.VirtualAddr = Base;
  std::tie(Seg.VirtualSize, Seg.FileSize) = computeSegmentSize(Seg, 0U);
  return positionOutputSections(Base, &Seg);
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
  Base = alignTo(Base, Seg.MaxAlign);
  Seg.VirtualAddr = Base;
  std::tie(Seg.VirtualSize, Seg.FileSize) =
      computeSegmentSize(Seg, Layout->HeaderBlockSize);
  return positionOutputSections(Base + Layout->HeaderBlockSize, &Seg);
}

template <>
uint64_t positionSegment<SegmentKind::data>(uint64_t Base,
                                            Layout *const Layout) {

  Segment &DataSegment = Layout->Segments[SegmentKind::data];
  Segment &RelROSegment = Layout->Segments[SegmentKind::gnu_relro];

  DataSegment.MaxAlign = std::max(DataSegment.MaxAlign, RelROSegment.MaxAlign);
  Base = alignTo(Base, DataSegment.MaxAlign);
  DataSegment.VirtualAddr = Base;

  std::tie(RelROSegment.VirtualSize, RelROSegment.FileSize) =
      computeSegmentSize(RelROSegment, 0U);
  std::tie(DataSegment.VirtualSize, DataSegment.FileSize) = computeSegmentSize(
      DataSegment, RelROSegment.VirtualSize, RelROSegment.FileSize);

  return positionOutputSections(Base, &DataSegment);
}

template <>
uint64_t positionSegment<SegmentKind::gnu_relro>(uint64_t Base,
                                                 Layout *const Layout) {
  static_assert(SegmentKind::gnu_relro > SegmentKind::data,
                "Expected gnu_relro to be positioned after the data segment");
  Segment &Seg = Layout->Segments[SegmentKind::gnu_relro];

  const Segment &DataSegment = Layout->Segments[SegmentKind::data];

  Seg.VirtualAddr = DataSegment.VirtualAddr;
  assert(std::make_pair(Seg.VirtualSize, Seg.FileSize) ==
         computeSegmentSize(Seg, 0U));
  return positionOutputSections(Base, &Seg);
}

} // end anonymous namespace

// flatten segments
// ~~~~~~~~~~~~~~~~
std::tuple<std::unique_ptr<Layout>, std::unique_ptr<GOTPLTContainer>>
LayoutBuilder::flattenSegments(uint64_t Base, const uint64_t HeaderBlockSize) {
  assert(Base % PageSize == 0U);
  Layout_->HeaderBlockSize = HeaderBlockSize;
#define X(SegK) Base = positionSegment<SegmentKind::SegK>(Base, Layout_.get());
  RLD_SEGMENT_KIND
#undef X
  llvmDebug("rld-Layout", Ctx_.IOMut, [this] { debugDumpLayout(); });
  return {std::move(Layout_), std::move(GOTPLTs_)};
}

} // namespace rld
