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
using OrderedSymbolsContainer = std::vector<const Symbol *>;

uint64_t prepareStringTable(rld::Context &Ctxt, rld::Layout *const Lout,
                            const GlobalSymbolsContainer &Globals) {
  (void)Globals;
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

template <typename Function>
static void walkOrderedSymbolTable(const SymbolOrder &SymOrder,
                                   const UndefsContainer &Undefs, Function F) {
  auto WalkList = [&F](const Symbol *S) {
    for (; S != nullptr; S = S->NextEmit) {
      F(*S);
    }
  };
  WalkList(SymOrder.Local);
  WalkList(SymOrder.Global);
  std::for_each(std::begin(Undefs), std::end(Undefs), F);
}

// prepare symbol table section
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
template <typename ELFT>
static uint64_t
prepareSymbolTableSection(rld::Layout *const Lout,
                          const GlobalSymbolsContainer &Globals) {
  using Elf_Sym = typename llvm::object::ELFFile<ELFT>::Elf_Sym;
  // TODO: this shouldn't include the symbols with internal_no_symbol linkage.
  const uint64_t NumSymbols = Globals.size() + 1;

  OutputSection &StrTab = Lout->Sections[SectionKind::symtab];
  StrTab.AlwaysEmit = true;
  StrTab.FileSize = NumSymbols * sizeof(Elf_Sym);
  StrTab.MaxAlign = alignof(Elf_Sym);
  StrTab.Link = SectionKind::strtab;
  StrTab.Writer = nullptr;
  return NumSymbols;
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
  auto FileOffset = Lout.HeaderBlockSize;

  Lout.forEachSegment([&](const SegmentKind SegmentK, const Segment &Seg) {
    assert(FileOffset >= 0);
    if (SegmentK == SegmentKind::phdr) {
      return;
    }
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
    FileOffset = alignTo(FileOffset, Seg.MaxAlign);
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
    return llvm::ELF::STT_NOTYPE;
  case SectionKind::linked_definitions:
  case SectionKind::rela_plt:
  case SectionKind::gotplt:
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

// Returns the start address of the first executable segment.
static Elf_Addr defaultEntryPoint(const Context &Context,
                                  const Layout &Layout) {
  for (auto SegK = firstSegmentKind(); SegK != SegmentKind::last; ++SegK) {
    if (Layout.Segments[SegK].shouldEmit() &&
        (rld::elf::elfSegmentFlags<ELFT>(SegK) & llvm::ELF::PF_X) != 0U) {
      return Elf_Addr{Layout.Segments[SegK].VirtualAddr};
    }
  }
  // If we didn't fine anything executable, just use the layout base-address.
  return Elf_Addr{Context.baseAddress()};
}

static Elf_Addr entryPoint(const Context &Ctxt, const Layout &Layout) {
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
      auto *const EntrySymbol = rld::symbolShadow(
          Ctxt, pstore::typed_address<pstore::address>(Pos.get_address()));
      assert(EntrySymbol != nullptr);
      // The shadow memory pointer may be null if the string is known, but isn't
      // used as the name of a symbol.
      if (const Symbol *const Sym = *EntrySymbol) {
        return symbolValue(*Sym);
      }
    }
  }

  return defaultEntryPoint(Ctxt, Layout);
}

llvm::Error rld::elfOutput(const llvm::StringRef &OutputFileName,
                           Context &Context,
                           const GlobalSymbolsContainer &Globals,
                           const SymbolOrder &SymOrder,
                           const UndefsContainer &Undefs,
                           llvm::ThreadPool &WorkPool, Layout *const Layout,
                           const LocalPLTsContainer &PLTs) {
  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription);

  const uint64_t StringTableSize = prepareStringTable(Context, Layout, Globals);
  const uint64_t SymbolTableSize =
      prepareSymbolTableSection<ELFT>(Layout, Globals);
  const SectionIndexedArray<uint64_t> NameOffsets =
      buildSectionNameStringTable(Layout);

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

  rld::EnumIndexedArray<Region, Region::Last, rld::FileRegion> FileRegions;
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
                  NumSegments, NumSections]() {
    // Write the ELF file header.
    auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(
        BufferStart + FileRegions[Region::FileHeader].offset());
    *Ehdr = rld::elf::initELFHeader<ELFT>(llvm::ELF::EM_X86_64);
    Ehdr->e_type = llvm::ELF::ET_EXEC;
    Ehdr->e_entry =
        entryPoint(Context, *Layout); // Address of the program entry point.
    Ehdr->e_phnum = NumSegments;
    Ehdr->e_phoff = FileRegions[Region::SegmentTable].offset();
    Ehdr->e_shnum = NumSections;
    Ehdr->e_shoff = FileRegions[Region::SectionTable].offset();
    Ehdr->e_shstrndx = SectionToIndex[SectionKind::shstrtab];
  });

  WorkPool.async([&Context, BufferStart, Layout, &FileRegions,
                  &SegmentFileOffsets]() {
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
  ]() {
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

  WorkPool.async([&Context, Layout, &FileRegions, BufferStart,
                  &SectionFileOffsets]() {
    assert(SectionFileOffsets[SectionKind::shstrtab].hasValue() &&
           "The shstrtab section should have been assigned an offset");
    auto *const Data = BufferStart + FileRegions[Region::SectionData].offset() +
                       *SectionFileOffsets[SectionKind::shstrtab];
    Layout->Sections[SectionKind::shstrtab].Writer(
        Context, Layout->Sections[SectionKind::shstrtab], Data, *Layout);
  });

  WorkPool.async([&Context, &FileRegions, BufferStart, &SymOrder, &Undefs,
                  StringTableSize, &SectionFileOffsets]() {
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

    walkOrderedSymbolTable(SymOrder, Undefs, [&](const Symbol &Sym) {
      // Copy a string.
      const pstore::raw_sstring_view Str = pstore::get_sstring_view(
          Context.Db, Sym.name(), Sym.nameLength(), &Owner);
      StringOut = std::copy(std::begin(Str), std::end(Str), StringOut);
      *(StringOut++) = '\0';
    });
    (void)StringTableSize;
    assert(StringOut > StringData &&
           static_cast<size_t>(StringOut - StringData) == StringTableSize);
  });

  WorkPool.async([&SectionToIndex, &FileRegions, BufferStart, &SymOrder,
                  &Undefs, SymbolTableSize, &SectionFileOffsets]() {
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
    // The initial null symbol.
    std::memset(SymbolOut, 0, sizeof(*SymbolOut));
    ++SymbolOut;

    walkOrderedSymbolTable(SymOrder, Undefs, [&](const Symbol &Sym) {
      // Build a symbol.
      const std::tuple<const Symbol::OptionalBodies &,
                       std::unique_lock<Symbol::Mutex>>
          Def = Sym.definition();
      const Symbol::OptionalBodies &Bodies =
          std::get<const Symbol::OptionalBodies &>(Def);
      if (!Bodies) {
        // This is an undefined symbol.
        std::memset(SymbolOut, 0, sizeof(*SymbolOut));
        assert(
            Sym.allReferencesAreWeak(
                std::get<std::unique_lock<Symbol::Mutex>>(Def)) &&
            "Undefined entries in the symbol table must be weakly referenced");
        SymbolOut->setBinding(llvm::ELF::STB_WEAK);
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
      SymbolOut->st_name = Elf_Word{NameOffset};

      ++SymbolOut;
      // Pass our lock to nameLength() so that it doesn't try to take one of its
      // own.
      NameOffset += Sym.nameLength(std::get<1>(Def)) +
                    1U; // +1 to allow for the final '\0'.
    });

    (void)SymbolTableSize;
    assert(SymbolOut >= SymbolData &&
           static_cast<size_t>(SymbolOut - SymbolData) == SymbolTableSize);
  });

  copyToOutput(Context, WorkPool, BufferStart, *Layout, PLTs,
               SectionFileOffsets, FileRegions[Region::SectionData].offset());
  WorkPool.wait();

  return (*Out)->commit();
}
