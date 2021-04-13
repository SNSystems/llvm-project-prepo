//===- lib/ElfOutput.cpp --------------------------------------------------===//
//*  _____ _  __    ___        _               _    *
//* | ____| |/ _|  / _ \ _   _| |_ _ __  _   _| |_  *
//* |  _| | | |_  | | | | | | | __| '_ \| | | | __| *
//* | |___| |  _| | |_| | |_| | |_| |_) | |_| | |_  *
//* |_____|_|_|    \___/ \__,_|\__| .__/ \__,_|\__| *
//*                               |_|               *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ElfOutput.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/FileOutputBuffer.h"
#include "llvm/Support/Timer.h"

#include "rld/LayoutBuilder.h"
#include "rld/MathExtras.h"
#include "rld/SectionKind.h"
#include "rld/copy.h"
#include "rld/elf.h"

#include "pstore/core/hamt_set.hpp"

using namespace rld;

namespace {
auto DebugType = "rld-ElfOutput";
}

static constexpr auto NumImplicitSections = 1U; // The null section.

// section name table writer
// ~~~~~~~~~~~~~~~~~~~~~~~~~
// Produce the section names string table.
static void sectionNameTableWriter(Context &, const OutputSection &OScn,
                                   uint8_t *Data, const Layout &Lout) {
  auto *SectionNameBegin = reinterpret_cast<char *>(Data);
  auto *SectionNamePtr = SectionNameBegin;

  // First entry is the empty string.
  *(SectionNamePtr++) = '\0';
  forEachSectionKind([&](SectionKind SectionK) {
    if (Lout.Sections[SectionK].shouldEmit()) {
      std::pair<char const *, std::size_t> const NameAndLength =
          elf::elfSectionNameAndLength(SectionK);
      std::memcpy(SectionNamePtr, NameAndLength.first, NameAndLength.second);
      assert(NameAndLength.second == std::strlen(NameAndLength.first));
      SectionNamePtr += NameAndLength.second;
      *(SectionNamePtr++) = '\0';
    }
  });

  assert(SectionNamePtr > SectionNameBegin &&
         static_cast<uint64_t>(SectionNamePtr - SectionNameBegin) ==
             OScn.FileSize);
}

// build section name string table
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static SectionIndexedArray<uint64_t>
buildSectionNameStringTable(Layout *const Lout) {
  OutputSection &ShStrTab = Lout->Sections[SectionKind::shstrtab];
  ShStrTab.AlwaysEmit = true;

  SectionIndexedArray<uint64_t> NameOffsets{{uint64_t{0}}};

  auto SectionNameSize = uint64_t{1}; // Initial null.
  forEachSectionKind([&](SectionKind SectionK) {
    if (Lout->Sections[SectionK].shouldEmit()) {
      NameOffsets[SectionK] = SectionNameSize;
      SectionNameSize += elf::elfSectionNameAndLength(SectionK).second + 1U;
    }
  });

  ShStrTab.FileSize = SectionNameSize;
  ShStrTab.MaxAlign = 1U;
  ShStrTab.Writer = sectionNameTableWriter;
  return NameOffsets;
}

// prepare string table
// ~~~~~~~~~~~~~~~~~~~~
uint64_t prepareStringTable(rld::Context &Ctxt, rld::Layout *const Lout,
                            const GlobalSymbolsContainer &Globals) {
  const auto StringTableSize = Ctxt.ELFStringTableSize.load();
  assert(StringTableSize ==
         std::accumulate(std::begin(Globals), std::end(Globals), uint64_t{1},
                         [&Ctxt](const uint64_t Acc, const Symbol &Sym) {
                           return Acc + stringLength(Ctxt.Db, Sym.name()) + 1U;
                         }));

  OutputSection &StrTab = Lout->Sections[SectionKind::strtab];
  StrTab.AlwaysEmit = true;
  StrTab.FileSize = StringTableSize;
  StrTab.MaxAlign = 1U;
  StrTab.Writer = nullptr;
  return StringTableSize;
}

// prepare symbol table
// ~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
static uint64_t prepareSymbolTable(rld::Layout *const Lout,
                                   const GlobalSymbolsContainer &Globals) {
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;
  // FIXME: this shouldn't include the symbols with internal_no_symbol linkage.
  const uint64_t NumSymbols = Globals.size();

  OutputSection &StrTab = Lout->Sections[SectionKind::symtab];
  StrTab.AlwaysEmit = true;
  StrTab.FileSize = NumSymbols * sizeof(Elf_Sym);
  StrTab.MaxAlign = alignof(Elf_Sym);
  StrTab.Link = SectionKind::strtab;
  StrTab.Writer = nullptr;
  return NumSymbols;
}

// first segment alignment
// ~~~~~~~~~~~~~~~~~~~~~~~
static unsigned firstSegmentAlignment(const rld::Layout &Lout) {
  auto FirstSegment =
      std::find_if(std::begin(Lout.Segments), std::end(Lout.Segments),
                   [](Segment const &S) { return S.shouldEmit(); });
  assert(FirstSegment != std::end(Lout.Segments));
  return FirstSegment->MaxAlign;
}

// build section to index table
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static SectionIndexedArray<unsigned>
buildSectionToIndexTable(const Layout &Lout) {
  SectionIndexedArray<unsigned> SectionToIndex;
  auto Index = NumImplicitSections;
  forEachSectionKind([&Lout, &Index, &SectionToIndex](SectionKind SectionK) {
    SectionToIndex[SectionK] =
        Lout.Sections[SectionK].shouldEmit()
            ? Index++
            : static_cast<unsigned>(llvm::ELF::SHN_UNDEF);
  });
  return SectionToIndex;
}

// compute section file offsets
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compute the file offsets for the segments and the sections that they contain.
// The result is returned in the two output-parameters: SegmentFileOffsets and
// SectionFileOffsets. Note that these values are all 0 based: that is, no
// allowance has been made for ELF header/ segment table on the front of the
// file.
static int64_t computeSectionFileOffsets(
    rld::Context &Ctxt, const rld::Layout &Lout,
    rld::SegmentIndexedArray<llvm::Optional<int64_t>>
        *const SegmentFileOffsets,                                       // out
    rld::SectionArray<llvm::Optional<int64_t>> *const SectionFileOffsets // out
) {
  using namespace rld;
  auto FileOffset = int64_t{0};

  Lout.forEachSegment([&](const SegmentKind SegmentK, const Segment &Seg) {
    assert(FileOffset >= 0);
    FileOffset = alignTo(FileOffset, Seg.MaxAlign);

    forEachSectionKind([&](SectionKind SectionK) {
      if (const OutputSection *const OutScn = Seg.Sections[SectionK]) {
        assert(OutScn->SectionK == SectionK &&
               "The output-section's SectionKind should match");
        if (OutScn->shouldEmit()) {
          assert(!(*SectionFileOffsets)[SectionK] &&
                 "Layout should not have assigned a section type to more than "
                 "one segment");

          FileOffset = alignTo(FileOffset, OutScn->MaxAlign);
          {
            // If this is the first section contributing to this segment then
            // its offset it the start of the segment.
            auto &SFO = (*SegmentFileOffsets)[SegmentK];
            if (!SFO) {
              SFO = FileOffset;
            }
          }
          (*SectionFileOffsets)[SectionK] = FileOffset;
          FileOffset += OutScn->FileSize;
        }
      }
    });
  });

  // Some sections still need to be written to the output file even though they
  // don't map to a segment.
  forEachSectionKind([&](SectionKind SectionK) {
    llvm::Optional<int64_t> &Offset = (*SectionFileOffsets)[SectionK];
    const OutputSection &OScn = Lout.Sections[SectionK];
    if (OScn.shouldEmit() && !Offset.hasValue()) {
      FileOffset = alignTo(FileOffset, OScn.MaxAlign);
      (*SectionFileOffsets)[SectionK] = FileOffset;
      FileOffset += OScn.FileSize;
    }
  });

  return FileOffset;
}

enum class Region { FileHeader, SegmentTable, SectionData, SectionTable, Last };

using ELFT = llvm::object::ELFType<llvm::support::little, true>;
using Elf_Ehdr = llvm::object::ELFFile<ELFT>::Elf_Ehdr;
using Elf_Shdr = llvm::object::ELFFile<ELFT>::Elf_Shdr;
using Elf_Phdr = llvm::object::ELFFile<ELFT>::Elf_Phdr;
using Elf_Sym = llvm::object::ELFFile<ELFT>::Elf_Sym;
using Elf_Addr = llvm::object::ELFFile<ELFT>::Elf_Addr;
using Elf_Word = llvm::object::ELFFile<ELFT>::Elf_Word;

// flatten segments
// ~~~~~~~~~~~~~~~~
/// Flattens the layout so that sections and segments don't overlap and
/// establishes their position in the SectionData region of the final file.
/// Data here is 0 based.
void flattenSegments(
    const Layout &Lout,
    const EnumIndexedArray<Region, Region::Last, FileRegion> &FileRegions,
    SegmentIndexedArray<llvm::Optional<uint64_t>> *const SegmentFileOffsets) {
  Lout.forEachSegment([&](const SegmentKind Kind, const Segment &Segment) {
    if (!Segment.shouldEmit()) {
      return;
    }
    uint64_t SegmentDataOffset = (*SegmentFileOffsets)[Kind].getValueOr(0U);
    if (Segment.HasOutputSections) {
      const auto &TargetDataRegion = FileRegions[Region::SectionData];
      SegmentDataOffset += TargetDataRegion.offset();
      assert(SegmentDataOffset >= TargetDataRegion.offset() &&
             SegmentDataOffset + Segment.FileSize < TargetDataRegion.end());
    }
    (*SegmentFileOffsets)[Kind] = SegmentDataOffset;
  });
}

static void overlapPhdrAndRODataSegments(
    Layout *const Lout,
    rld::SegmentIndexedArray<llvm::Optional<int64_t>> *const SegmentFileOffsets,
    const uint64_t PhdrSegmentSize,
    rld::EnumIndexedArray<Region, Region::Last, rld::FileRegion> const
        &FileRegions) {

  const int64_t SectionDataOffset = FileRegions[Region::SectionData].offset();
  {
    Segment &phdr = Lout->Segments[SegmentKind::phdr];
    phdr.FileSize = PhdrSegmentSize;
    phdr.VirtualSize = PhdrSegmentSize;
    phdr.VirtualAddr += sizeof(Elf_Ehdr);
    phdr.MaxAlign = 1U;
  }
  (*SegmentFileOffsets)[SegmentKind::phdr] =
      -SectionDataOffset + sizeof(Elf_Ehdr);

  // Now adjust the ROData segment so that it covers the ELF header and segment
  // table as well as its actual content.
  static_assert(
      static_cast<std::underlying_type_t<SegmentKind>>(SegmentKind::phdr) + 1 ==
          static_cast<std::underlying_type_t<SegmentKind>>(SegmentKind::rodata),
      "Expected the ROData segment to follow the PHDR segment");

  Lout->Segments[SegmentKind::rodata].VirtualSize +=
      SectionDataOffset; // RODataOffset;
  Lout->Segments[SegmentKind::rodata].FileSize +=
      SectionDataOffset; // RODataOffset;

  const int64_t RODataOffset =
      (*SegmentFileOffsets)[SegmentKind::rodata].getValueOr(0);
  (*SegmentFileOffsets)[SegmentKind::rodata] = RODataOffset - SectionDataOffset;
}

// FIXME: there is an almost identical function in R2OSymbolTable.h
static constexpr unsigned char sectionToSymbolType(const SectionKind T) {
  using namespace pstore::repo;
  switch (T) {
  case SectionKind::text:
    return llvm::ELF::STT_FUNC;
  case SectionKind::bss:
  case SectionKind::data:
  case SectionKind::rel_ro:
  case SectionKind::mergeable_1_byte_c_string:
  case SectionKind::mergeable_2_byte_c_string:
  case SectionKind::mergeable_4_byte_c_string:
  case SectionKind::mergeable_const_4:
  case SectionKind::mergeable_const_8:
  case SectionKind::mergeable_const_16:
  case SectionKind::mergeable_const_32:
  case SectionKind::read_only:
    return llvm::ELF::STT_OBJECT;
  case SectionKind::thread_bss:
  case SectionKind::thread_data:
    return llvm::ELF::STT_TLS;
  case SectionKind::debug_line:
  case SectionKind::debug_ranges:
  case SectionKind::debug_string:
  case SectionKind::interp:
    return llvm::ELF::STT_NOTYPE;
  case SectionKind::linked_definitions:
  case SectionKind::plt:
  case SectionKind::shstrtab:
  case SectionKind::strtab:
  case SectionKind::symtab:
  case SectionKind::last:
    break;
  }
  llvm_unreachable("invalid section type");
}

// FIXME: this function was lifted from R2OSymbolTable.h
static constexpr unsigned char
linkageToELFBinding(const pstore::repo::linkage L) {
  switch (L) {
  case pstore::repo::linkage::internal_no_symbol:
  case pstore::repo::linkage::internal:
    return llvm::ELF::STB_LOCAL;
  case pstore::repo::linkage::link_once_any:
  case pstore::repo::linkage::link_once_odr:
  case pstore::repo::linkage::weak_any:
  case pstore::repo::linkage::weak_odr:
    return llvm::ELF::STB_WEAK;
  default:
    return llvm::ELF::STB_GLOBAL;
  }
}

static Elf_Addr symbolValue(const Symbol &Sym) {
  const Contribution *const C = Sym.contribution();
  assert(C != nullptr);
  return static_cast<Elf_Addr>(C->OScn->VirtualAddr + C->Offset);
}

static Elf_Addr defaultEntryPoint() {
  // TODO: The first address of the executable segment seems like a reasonable
  // choice.
  return Elf_Addr{0x0000000000200000};
}

static Elf_Addr entryPoint(const Context &Ctxt) {
  const llvm::StringRef Name = Ctxt.entryPoint();
  const auto NamesIndex =
      pstore::index::get_index<pstore::trailer::indices::name>(Ctxt.Db, false);
  if (!NamesIndex) {
    return defaultEntryPoint();
  }
  const auto Start = pstore::make_sstring_view(
      pstore::gsl::make_span(std::begin(Name), std::end(Name)));
  const auto Pos =
      NamesIndex->find(Ctxt.Db, pstore::indirect_string{Ctxt.Db, &Start});
  if (Pos == NamesIndex->end(Ctxt.Db)) {
    return defaultEntryPoint();
  }

  assert(Pos->is_in_store());
  const std::atomic<const Symbol *> *const EntrySymbol = rld::symbolShadow(
      Ctxt, pstore::typed_address<pstore::address>(Pos.get_address()));
  assert(EntrySymbol != nullptr);
  Symbol const *const Sym = *EntrySymbol;
  // The shadow memory pointer may be null if the string is known, but isn't
  // used as the name of a symbol.
  return (Sym != nullptr) ? symbolValue(*Sym) : defaultEntryPoint();
}

static unsigned numberOfSegments(Layout const &Lout) {
  return std::accumulate(std::begin(Lout.Segments), std::end(Lout.Segments), 0U,
                         [](const unsigned Acc, const rld::Segment &Segment) {
                           return Acc +
                                  static_cast<unsigned>(Segment.shouldEmit());
                         });
}

llvm::Error rld::elfOutput(const llvm::StringRef &OutputFileName, Context &Ctxt,
                           const GlobalSymbolsContainer &Globals,
                           llvm::ThreadPool &WorkPool, Layout *const Lout,
                           const LocalPLTsContainer &PLTs) {
  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription);

  const uint64_t StringTableSize = prepareStringTable(Ctxt, Lout, Globals);
  const uint64_t SymbolTableSize = prepareSymbolTable<ELFT>(Lout, Globals);
  const SectionIndexedArray<uint64_t> NameOffsets =
      buildSectionNameStringTable(Lout);

  // After this point, don't do anything that might cause additional sections to
  // be emitted. Once we've decided which names are going into the section name
  // string table, it's too late to add any more!

  const SectionIndexedArray<unsigned> SectionToIndex =
      buildSectionToIndexTable(*Lout);

  // The call to overlapPhdrAndRODataSegments will mean that we always need to
  // produce a (loadable) read-only segment.
  Lout->Segments[SegmentKind::phdr].AlwaysEmit = true;

  // Flattens the layout so that sections and segments don't overlap and
  // establishes their position in the SectionData region of the final file.
  // Data here is 0 based.
  rld::SectionArray<llvm::Optional<int64_t>> SectionFileOffsets;
  rld::SegmentIndexedArray<llvm::Optional<int64_t>> SegmentFileOffsets;

  const int64_t LayoutEnd = computeSectionFileOffsets(
      Ctxt, *Lout, &SegmentFileOffsets, &SectionFileOffsets);
  assert(LayoutEnd >= 0);
  llvmDebug(DebugType, Ctxt.IOMut, [&] {
    auto &OS = llvm::dbgs();
    OS << "After computeSectionFileOffsets\n";
    forEachSegmentKind([&](SegmentKind SegmentK) {
      OS << "  Segment " << SegmentK
         << " offset: " << SegmentFileOffsets[SegmentK] << '\n';
    });
    // forEachSectionKind ([&] (SectionKind SectionK) { OS << "  Section " <<
    // SectionK << " offset: " << SectionFileOffsets[SectionK] << '\n'; });
  });

  assert(SectionFileOffsets[rld::SectionKind::shstrtab].hasValue() &&
         "Expected the section names table to have been assigned an offset");
  assert(SectionFileOffsets[rld::SectionKind::strtab].hasValue() &&
         "Expected the strings table to have been assigned an offset");

  Lout->Segments[SegmentKind::rodata].AlwaysEmit = true;

  const unsigned NumSections = std::accumulate(
      std::begin(Lout->Sections), std::end(Lout->Sections), NumImplicitSections,
      [](const unsigned Acc, const rld::OutputSection &OutScn) {
        return Acc + static_cast<unsigned>(OutScn.shouldEmit());
      });
  const unsigned NumSegments = numberOfSegments(*Lout);

  rld::EnumIndexedArray<Region, Region::Last, rld::FileRegion> FileRegions;
  FileRegions[Region::FileHeader] = rld::FileRegion{0, sizeof(Elf_Ehdr)};
  FileRegions[Region::SegmentTable] = rld::FileRegion{
      FileRegions[Region::FileHeader].end(), sizeof(Elf_Phdr) * NumSegments};
  FileRegions[Region::SectionData] = rld::FileRegion{
      alignTo(FileRegions[Region::SegmentTable].end(),
              NumSegments > 0U ? firstSegmentAlignment(*Lout) : 1U),
      static_cast<uint64_t>(LayoutEnd)};
  FileRegions[Region::SectionTable] = rld::FileRegion{
      FileRegions[Region::SectionData].end(), NumSections * sizeof(Elf_Shdr)};

  //  flattenSegments(*Lout, FileRegions, &SegmentFileOffsets);

  overlapPhdrAndRODataSegments(Lout, &SegmentFileOffsets,
                               FileRegions[Region::SegmentTable].size(),
                               FileRegions);
  llvmDebug(DebugType, Ctxt.IOMut, [&] {
    auto &OS = llvm::dbgs();
    OS << "After overlap\n";
    forEachSegmentKind([&](SegmentKind SegmentK) {
      OS << "  Segment " << SegmentK
         << " offset: " << SegmentFileOffsets[SegmentK] << '\n';
    });
    // forEachSectionKind ([&] (SectionKind SectionK) { OS << "  Section " <<
    // SectionK << " offset: " << SectionFileOffsets[SectionK] << '\n'; });
  });
  // The number of output segments must not changed from the space that we
  // previously allocated for Region::SegmentTable!
  assert(NumSegments == numberOfSegments(*Lout));

  uint64_t const TotalSize = FileRegions.back().end();

  llvm::Expected<std::unique_ptr<llvm::FileOutputBuffer>> Out =
      llvm::FileOutputBuffer::create(
          OutputFileName, TotalSize,
          llvm::FileOutputBuffer::
              F_executable /*| llvm::FileOutputBuffer::F_modify*/);
  if (!Out) {
    return Out.takeError();
  }

  uint8_t *const BufferStart = (*Out)->getBufferStart();

  WorkPool.async([&SectionToIndex, &Ctxt, BufferStart, FileRegions, NumSegments,
                  NumSections]() {
    // Write the ELF file header.
    auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(
        BufferStart + FileRegions[Region::FileHeader].offset());
    *Ehdr = rld::elf::initELFHeader<ELFT>(llvm::ELF::EM_X86_64);
    Ehdr->e_type = llvm::ELF::ET_EXEC;
    Ehdr->e_entry = entryPoint(Ctxt); // Address of the program entry point.
    Ehdr->e_phnum = NumSegments;
    Ehdr->e_phoff = FileRegions[Region::SegmentTable].offset();
    Ehdr->e_shnum = NumSections;
    Ehdr->e_shoff = FileRegions[Region::SectionTable].offset();
    Ehdr->e_shstrndx = SectionToIndex[SectionKind::shstrtab];
  });

  WorkPool.async([&Ctxt, BufferStart, Lout, &FileRegions, &SegmentFileOffsets
#ifndef NDEBUG
                  ,
                  NumSegments
#endif
  ]() {
    // Produce the program header table.
    auto *const Start = reinterpret_cast<Elf_Phdr *>(
        BufferStart + FileRegions[Region::SegmentTable].offset());
    auto *const End = elf::emitProgramHeaders<ELFT>(
        Start, Ctxt, FileRegions[Region::SectionData], *Lout,
        SegmentFileOffsets);

    (void)End;
    assert(End - Start == NumSegments);
    assert(reinterpret_cast<uint8_t *>(Start) +
               FileRegions[Region::SegmentTable].size() ==
           reinterpret_cast<uint8_t *>(End));
  });

  WorkPool.async([Lout, BufferStart, &SectionFileOffsets, &NameOffsets,
                  &FileRegions
#ifndef NDEBUG
                  ,
                  NumSections
#endif
  ]() {
    // Produce the section header table.
    auto *const Start = reinterpret_cast<Elf_Shdr *>(
        BufferStart + FileRegions[Region::SectionTable].offset());
    auto *const End = elf::emitSectionHeaders<ELFT>(
        Start, *Lout, SectionFileOffsets, NameOffsets,
        FileRegions[Region::SectionData].offset());

    (void)End;
    assert(End - Start == NumSections && "The expected number of sections were "
                                         "not produced by emitSectionheader()");
    assert(reinterpret_cast<uint8_t *>(Start) +
               FileRegions[Region::SectionTable].size() ==
           reinterpret_cast<uint8_t *>(End));
  });

  WorkPool.async([&Ctxt, Lout, &FileRegions, BufferStart,
                  &SectionFileOffsets]() {
    assert(SectionFileOffsets[SectionKind::shstrtab].hasValue() &&
           "The shstrtab section should have been assigned an offset");
    auto *const Data = BufferStart + FileRegions[Region::SectionData].offset() +
                       *SectionFileOffsets[SectionKind::shstrtab];
    Lout->Sections[SectionKind::shstrtab].Writer(
        Ctxt, Lout->Sections[SectionKind::shstrtab], Data, *Lout);
  });

  WorkPool.async([&Ctxt, &FileRegions, BufferStart, &Globals, StringTableSize,
                  &SectionFileOffsets]() {
    // Produce the string table.
    llvm::NamedRegionTimer StringTableTimer(
        "String Table", "Write the symbol name section", rld::TimerGroupName,
        rld::TimerGroupDescription);

    assert(SectionFileOffsets[SectionKind::strtab].hasValue() &&
           "The strtab section should have been assigned an offset");

    auto *const StringData = BufferStart +
                             FileRegions[Region::SectionData].offset() +
                             *SectionFileOffsets[SectionKind::strtab];
    auto *StringOut = StringData;
    *(StringOut++) = '\0'; // The string table's initial null entry.
    pstore::shared_sstring_view Owner;
    for (const Symbol &Sym : Globals) {
      // Copy a string.
      const pstore::raw_sstring_view Str = pstore::get_sstring_view(
          Ctxt.Db, Sym.name(), Sym.nameLength(), &Owner);
      StringOut = std::copy(std::begin(Str), std::end(Str), StringOut);
      *(StringOut++) = '\0';
    }
    (void)StringTableSize;
    assert(StringOut > StringData &&
           static_cast<size_t>(StringOut - StringData) == StringTableSize);
  });

  WorkPool.async([&SectionToIndex, &FileRegions, BufferStart, &Globals,
                  SymbolTableSize, &SectionFileOffsets]() {
    llvm::NamedRegionTimer StringTableTimer(
        "Symbol Table", "Write the symbol table", rld::TimerGroupName,
        rld::TimerGroupDescription);

    assert(SectionFileOffsets[SectionKind::symtab].hasValue() &&
           "The symbol section should have been assigned an offset");

    auto *const SymbolData = reinterpret_cast<Elf_Sym *>(
        BufferStart + FileRegions[Region::SectionData].offset() +
        *SectionFileOffsets[SectionKind::symtab]);
    auto *SymbolOut = SymbolData;

    auto NameOffset =
        Elf_Word::value_type{1}; // 1 to allow for the initial '\0'.
    static_assert(std::is_unsigned<Elf_Word::value_type>::value,
                  "Expected ELF_Word to be unsigned");
    for (const Symbol &Sym : Globals) {
      // Build a symbol.
      const std::tuple<const Symbol::OptionalBodies &,
                       std::unique_lock<Symbol::Mutex>>
          Def = Sym.definition();
      const Symbol::OptionalBodies &Bodies = std::get<0>(Def);
      SymbolOut->st_name = Elf_Word{NameOffset};
      if (!Bodies) {
        // this is an undefined symbol.
        std::memset(SymbolOut, 0, sizeof(*SymbolOut));
      } else {
        // If there are multiple bodies associated with a symbol then the ELF
        // symbol simply points to the first.
        const Symbol::Body &B = Bodies->front();
        const Contribution *const C = Sym.contribution();
        assert(C != nullptr);

        SymbolOut->st_value = C->OScn->VirtualAddr + C->Offset;
        SymbolOut->st_shndx = SectionToIndex[C->OScn->SectionK];
        SymbolOut->st_size =
            0; // FIXME: the sum of the sizes of all of the bodies.
        SymbolOut->setVisibility(elf::elfVisibility<ELFT>(B.visibility()));
        SymbolOut->setBindingAndType(linkageToELFBinding(B.linkage()),
                                     sectionToSymbolType(C->OScn->SectionK));
      }

      ++SymbolOut;
      // Pass our lock to nameLength() so that it doesn't try to take one of its
      // own.
      NameOffset += Sym.nameLength(std::get<1>(Def)) +
                    1U; // +1 to allow for the final '\0'.
    }

    (void)SymbolTableSize;
    assert(SymbolOut >= SymbolData &&
           static_cast<size_t>(SymbolOut - SymbolData) == SymbolTableSize);
  });

  copyToOutput(Ctxt, WorkPool, BufferStart, *Lout, PLTs, SectionFileOffsets,
               FileRegions[Region::SectionData].offset());
  WorkPool.wait();

  return (*Out)->commit();
}
