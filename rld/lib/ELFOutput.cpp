//===- lib/ELFOutput.cpp --------------------------------------------------===//
//*  _____ _     _____ ___        _               _    *
//* | ____| |   |  ___/ _ \ _   _| |_ _ __  _   _| |_  *
//* |  _| | |   | |_ | | | | | | | __| '_ \| | | | __| *
//* | |___| |___|  _|| |_| | |_| | |_| |_) | |_| | |_  *
//* |_____|_____|_|   \___/ \__,_|\__| .__/ \__,_|\__| *
//*                                  |_|               *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELFOutput.h"

#include "rld/Copy.h"
#include "rld/ELF.h"
#include "rld/ELFProgramHeaders.h"
#include "rld/ELFSectionHeaders.h"
#include "rld/ELFSectionNames.h"
#include "rld/ELFStringTable.h"
#include "rld/ELFSymbolTable.h"
#include "rld/MathExtras.h"
#include "rld/SectionKind.h"
#include "rld/Shadow.h"

#include "pstore/core/hamt_set.hpp"

#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/FileOutputBuffer.h"
#include "llvm/Support/Timer.h"

#include <bitset>

using namespace rld;

namespace {
auto DebugType = "rld-ElfOutput";
}

static constexpr auto NumImplicitSections = 1U; // The null section.

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
// allowance has been made for ELF header/segment table on the front of the
// file.
static int64_t computeSectionFileOffsets(
    Context &Ctxt, const Layout &Lout,
    SegmentIndexedArray<llvm::Optional<int64_t>>
        *const SegmentFileOffsets,                                  // out
    SectionArray<llvm::Optional<int64_t>> *const SectionFileOffsets // out
) {

  uint64_t FileOffset = Lout.HeaderBlockSize;

  Lout.forEachSegment([&](const SegmentKind SegmentK, const Segment &Seg) {
    if (SegmentK == SegmentKind::phdr) {
      return;
    }
    forEachSectionKindInFileOrder([&](const SectionKind SectionK) {
      if (const OutputSection *const OutScn = Seg.Sections[SectionK]) {
        assert(OutScn->SectionK == SectionK &&
               "The OutputSection SectionKind should match");
        if (OutScn->shouldEmit()) {
          assert(!(*SectionFileOffsets)[SectionK] &&
                 "Layout should not have assigned a section type to more than "
                 "one segment");
          FileOffset = alignTo(FileOffset, OutScn->MaxAlign);

          // If this is the first section contributing to this segment then
          // its offset is the start of the segment.
          if (!(*SegmentFileOffsets)[SegmentK]) {
            (*SegmentFileOffsets)[SegmentK] = FileOffset;
          }
          (*SectionFileOffsets)[SectionK] = FileOffset;
          FileOffset += OutScn->FileSize;
        }
      }
    });
    FileOffset = alignTo(FileOffset, Seg.MaxAlign);
  });

  // Some sections still need to be written to the output file even though they
  // don't map to a segment.
  forEachSectionKindInFileOrder([&](const SectionKind SectionK) {
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

template <typename ELFT>
static typename llvm::object::ELFFile<ELFT>::Elf_Addr addressCast(uint64_t A) {
  using Elf_Addr = typename llvm::object::ELFFile<ELFT>::Elf_Addr;
  return Elf_Addr{static_cast<typename Elf_Addr::value_type>(A)};
}

// Returns the start address of the first executable segment.
template <typename ELFT>
static typename llvm::object::ELFFile<ELFT>::Elf_Addr
defaultEntryPoint(const Context &Context, const Layout &Layout) {
  for (auto SegK = firstSegmentKind(); SegK != SegmentKind::last; ++SegK) {
    if (Layout.Segments[SegK].shouldEmit() &&
        (rld::elf::elfSegmentFlags<ELFT>(SegK) & llvm::ELF::PF_X) != 0U) {
      return addressCast<ELFT>(Layout.Segments[SegK].VirtualAddr);
    }
  }
  // If we didn't find anything executable, just use the layout base-address.
  return addressCast<ELFT>(
      Context.baseAddress()); // Elf_Addr{Context.baseAddress()};
}

template <typename ELFT>
static typename llvm::object::ELFFile<ELFT>::Elf_Addr
entryPoint(const Context &Ctxt, const Layout &Layout) {
  if (const auto NamesIndex =
          pstore::index::get_index<pstore::trailer::indices::name>(Ctxt.Db,
                                                                   false)) {
    const llvm::StringRef Name = Ctxt.entryPoint();
    const auto Start = pstore::make_sstring_view(
        pstore::gsl::make_span(std::begin(Name), std::end(Name)));
    const auto Pos =
        NamesIndex->find(Ctxt.Db, pstore::indirect_string{Ctxt.Db, &Start});
    if (Pos != NamesIndex->end(Ctxt.Db)) {
      assert(Pos->is_in_store());
      const shadow::AtomicTaggedPointer *const EntrySymbol = Ctxt.shadowPointer(
          pstore::typed_address<pstore::address>(Pos.get_address()));
      assert(EntrySymbol != nullptr);
      // The shadow memory pointer may be null if the string is known, but isn't
      // used as the name of a symbol.
      if (const auto *const Sym = EntrySymbol->load().get_if<Symbol *>()) {
        return addressCast<ELFT>(elf::symbolValue(*Sym));
      }
    }
  }

  return defaultEntryPoint<ELFT>(Ctxt, Layout);
}

enum class Region { FileHeader, SegmentTable, SectionData, SectionTable, Last };

template <typename ELFT>
llvm::Error
rld::elfOutput(const llvm::StringRef &OutputFileName, Context &Context,
               const GlobalSymbolsContainer &Globals,
               const SymbolOrder &SymOrder, const UndefsContainer &Undefs,
               llvm::ThreadPool &WorkPool, Layout *const Layout,
               const GOTPLTContainer &GOTPLTs) {
  using Elf_Ehdr = typename llvm::object::ELFFile<ELFT>::Elf_Ehdr;
  using Elf_Shdr = typename llvm::object::ELFFile<ELFT>::Elf_Shdr;
  using Elf_Phdr = typename llvm::object::ELFFile<ELFT>::Elf_Phdr;
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;

  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription,
                               Context.TimersEnabled);

  const uint64_t StringTableSize =
      elf::prepareStringTable(Layout, Context, Globals);
  const uint64_t SymbolTableSize =
      elf::prepareSymbolTableSection<ELFT>(Layout, Globals);
  const SectionIndexedArray<uint64_t> NameOffsets =
      elf::buildSectionNameStringTable(Layout);

  // After this point, don't do anything that might cause additional sections to
  // be emitted. Once we've decided which names are going into the section name
  // string table, it's too late to add any more!

  const SectionIndexedArray<unsigned> SectionToIndex =
      buildSectionToIndexTable(*Layout);

  // Flattens the layout so that sections and segments don't overlap and
  // establishes their position in the SectionData region of the final file.
  // Data here is 0 based.
  rld::SectionArray<llvm::Optional<int64_t>> SectionFileOffsets;
  rld::SegmentIndexedArray<llvm::Optional<int64_t>> SegmentFileOffsets;

  const int64_t LayoutEnd = computeSectionFileOffsets(
      Context, *Layout, &SegmentFileOffsets, &SectionFileOffsets);
  assert(LayoutEnd >= 0);
  llvmDebug(DebugType, Context.IOMut, [&] {
    auto &OS = llvm::dbgs();
    OS << "After computeSectionFileOffsets\n";
    forEachSegmentKind([&](SegmentKind SegmentK) {
      OS << "  Segment " << SegmentK
         << " offset: " << SegmentFileOffsets[SegmentK] << '\n';
    });
  });

  assert(SectionFileOffsets[rld::SectionKind::shstrtab].hasValue() &&
         "Expected the section names table to have been assigned an offset");
  assert(SectionFileOffsets[rld::SectionKind::strtab].hasValue() &&
         "Expected the strings table to have been assigned an offset");

  const unsigned NumSections = Layout->numberOfSections(NumImplicitSections);
  const unsigned NumSegments = Layout->numberOfSegments();

  EnumIndexedArray<Region, Region::Last, FileRegion> FileRegions;
  FileRegions[Region::FileHeader] = rld::FileRegion{0, sizeof(Elf_Ehdr)};
  FileRegions[Region::SegmentTable] = rld::FileRegion{
      FileRegions[Region::FileHeader].end(), sizeof(Elf_Phdr) * NumSegments};
  FileRegions[Region::SectionData] =
      rld::FileRegion{0, static_cast<uint64_t>(LayoutEnd)};
  FileRegions[Region::SectionTable] = rld::FileRegion{
      FileRegions[Region::SectionData].end(), NumSections * sizeof(Elf_Shdr)};

  const uint64_t TotalSize = FileRegions.back().end();

  llvm::Expected<std::unique_ptr<llvm::FileOutputBuffer>> Out =
      llvm::FileOutputBuffer::create(
          OutputFileName, TotalSize,
          llvm::FileOutputBuffer::
              F_executable /*| llvm::FileOutputBuffer::F_modify*/);
  if (!Out) {
    return Out.takeError();
  }

  uint8_t *const BufferStart = (*Out)->getBufferStart();

  WorkPool.async([&SectionToIndex, &Context, BufferStart, Layout, FileRegions,
                  NumSegments, NumSections] {
    // Write the ELF file header.
    auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(
        BufferStart + FileRegions[Region::FileHeader].offset());
    *Ehdr = rld::elf::initELFHeader<ELFT>(llvm::ELF::EM_X86_64);
    Ehdr->e_type = llvm::ELF::ET_EXEC;
    Ehdr->e_entry = entryPoint<ELFT>(Context, *Layout);
    Ehdr->e_phnum = NumSegments;
    Ehdr->e_phoff = FileRegions[Region::SegmentTable].offset();
    Ehdr->e_shnum = NumSections;
    Ehdr->e_shoff = FileRegions[Region::SectionTable].offset();
    Ehdr->e_shstrndx = SectionToIndex[SectionKind::shstrtab];
  });

  WorkPool.async(
      [&Context, BufferStart, Layout, &FileRegions, &SegmentFileOffsets] {
        // Produce the program header table.
        auto *const Start = reinterpret_cast<Elf_Phdr *>(
            BufferStart + FileRegions[Region::SegmentTable].offset());
        auto *const End = elf::emitProgramHeaders<ELFT>(
            Start, Context, FileRegions[Region::SectionData],
            FileRegions[Region::SegmentTable], *Layout, SegmentFileOffsets);

        (void)End;
        assert(End - Start == Layout->numberOfSegments());
        assert(reinterpret_cast<uint8_t *>(Start) +
                   FileRegions[Region::SegmentTable].size() ==
               reinterpret_cast<uint8_t *>(End));
      });

  WorkPool.async([Layout, BufferStart, &SectionFileOffsets, &NameOffsets,
                  &FileRegions, &SymOrder
#ifndef NDEBUG
                  ,
                  NumSections
#endif
  ] {
    // Produce the section header table.
    auto *const Start = reinterpret_cast<Elf_Shdr *>(
        BufferStart + FileRegions[Region::SectionTable].offset());
    auto *const End = elf::emitSectionHeaders<ELFT>(
        Start, *Layout, SectionFileOffsets, NameOffsets, SymOrder.LocalsSize,
        FileRegions[Region::SectionData].offset());

    (void)End;
    assert(End - Start == NumSections && "The expected number of sections were "
                                         "not produced by emitSectionheader()");
    assert(reinterpret_cast<uint8_t *>(Start) +
               FileRegions[Region::SectionTable].size() ==
           reinterpret_cast<uint8_t *>(End));
  });
  {
    assert(SectionFileOffsets[SectionKind::shstrtab].hasValue() &&
           "The shstrtab section should have been assigned an offset");
    auto *const NamesStart = BufferStart +
                             FileRegions[Region::SectionData].offset() +
                             *SectionFileOffsets[SectionKind::shstrtab];

    WorkPool.async([NamesStart, Layout] {
      uint8_t *const NamesEnd =
          elf::sectionNameTableWriter(NamesStart, *Layout);
      (void)NamesEnd;
      assert(NamesEnd > NamesStart &&
             static_cast<size_t>(NamesEnd - NamesStart) ==
                 Layout->Sections[SectionKind::shstrtab].FileSize);
    });
  }
  {
    assert(SectionFileOffsets[SectionKind::strtab].hasValue() &&
           "The strtab section should have been assigned an offset");
    auto *const StringStart = BufferStart +
                              FileRegions[Region::SectionData].offset() +
                              *SectionFileOffsets[SectionKind::strtab];

    WorkPool.async([StringStart, &Context, &SymOrder, &Undefs,
                    StringTableSize] {
      // Produce the string table.
      llvm::NamedRegionTimer StringTableTimer{
          "String Table", "Write the symbol name section", rld::TimerGroupName,
          rld::TimerGroupDescription, Context.TimersEnabled};

      auto *const StringEnd =
          elf::writeStrings(StringStart, Context, SymOrder, Undefs);
      (void)StringTableSize;
      (void)StringEnd;
      assert(StringEnd > StringStart &&
             static_cast<size_t>(StringEnd - StringStart) == StringTableSize);
    });
  }
  {
    assert(SectionFileOffsets[SectionKind::symtab].hasValue() &&
           "The symbol section should have been assigned an offset");
    auto *const SymbolStart = reinterpret_cast<Elf_Sym *>(
        BufferStart + FileRegions[Region::SectionData].offset() +
        *SectionFileOffsets[SectionKind::symtab]);

    WorkPool.async([SymbolStart, &Context, &SymOrder, &Undefs, &SectionToIndex,
                    SymbolTableSize] {
      llvm::NamedRegionTimer _{"Symbol Table", "Write the symbol table",
                               rld::TimerGroupName, rld::TimerGroupDescription,
                               Context.TimersEnabled};

      Elf_Sym *const SymbolEnd = elf::writeSymbolTable<ELFT>(
          SymbolStart, SymOrder, Undefs, SectionToIndex);
      (void)SymbolTableSize;
      (void)SymbolEnd;
      assert(SymbolEnd >= SymbolStart &&
             static_cast<size_t>(SymbolEnd - SymbolStart) == SymbolTableSize);
    });
  }

  copyToOutput(Context, WorkPool, BufferStart, *Layout, GOTPLTs,
               SectionFileOffsets, FileRegions[Region::SectionData].offset());
  WorkPool.wait();

  return (*Out)->commit();
}

template llvm::Error rld::elfOutput<llvm::object::ELF64LE>(
    const llvm::StringRef &OutputFileName, Context &Ctxt,
    const GlobalSymbolsContainer &Globals, const SymbolOrder &SymOrder,
    const UndefsContainer &Undefs, llvm::ThreadPool &WorkPool,
    Layout *const Lout, const GOTPLTContainer &GOTPLTs);
template llvm::Error rld::elfOutput<llvm::object::ELF64BE>(
    const llvm::StringRef &OutputFileName, Context &Ctxt,
    const GlobalSymbolsContainer &Globals, const SymbolOrder &SymOrder,
    const UndefsContainer &Undefs, llvm::ThreadPool &WorkPool,
    Layout *const Lout, const GOTPLTContainer &GOTPLTs);
template llvm::Error rld::elfOutput<llvm::object::ELF32LE>(
    const llvm::StringRef &OutputFileName, Context &Ctxt,
    const GlobalSymbolsContainer &Globals, const SymbolOrder &SymOrder,
    const UndefsContainer &Undefs, llvm::ThreadPool &WorkPool,
    Layout *const Lout, const GOTPLTContainer &GOTPLTs);
template llvm::Error rld::elfOutput<llvm::object::ELF32BE>(
    const llvm::StringRef &OutputFileName, Context &Ctxt,
    const GlobalSymbolsContainer &Globals, const SymbolOrder &SymOrder,
    const UndefsContainer &Undefs, llvm::ThreadPool &WorkPool,
    Layout *const Lout, const GOTPLTContainer &GOTPLTs);
