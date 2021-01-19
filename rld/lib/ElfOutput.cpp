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

using namespace rld;

static constexpr auto NumImplicitSections = 1U; // The null section.

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

static EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
buildSectionNameStringTable(Layout *const Lout) {
  OutputSection &ShStrTab = Lout->Sections[SectionKind::shstrtab];
  ShStrTab.AlwaysEmit = true;

  EnumIndexedArray<SectionKind, SectionKind::last, uint64_t> NameOffsets;

  auto SectionNameSize = uint64_t{1}; // Initial null.
  forEachSectionKind([&](SectionKind SectionK) {
    if (Lout->Sections[SectionK].shouldEmit()) {
      NameOffsets[SectionK] = SectionNameSize;
      SectionNameSize +=
          rld::elf::elfSectionNameAndLength(SectionK).second + 1U;
    }
  });

  ShStrTab.FileSize = SectionNameSize;
  ShStrTab.MaxAlign = 1U;
  ShStrTab.Writer = sectionNameTableWriter;
  return NameOffsets;
}

// section string table index
// ~~~~~~~~~~~~~~~~~~~~~~~~~~
static unsigned sectionStringTableIndex(const Layout &Lout) {
  auto Result = NumImplicitSections;
  for (auto SectionK = rld::firstSectionKind();
       SectionK != rld::SectionKind::last; ++SectionK) {
    if (SectionK == SectionKind::shstrtab) {
      return Result;
    }
    if (Lout.Sections[SectionK].shouldEmit()) {
      ++Result;
    }
  }
  llvm_unreachable("The shstrtab section was not found");
  return 0U;
}

// prepare string table
// ~~~~~~~~~~~~~~~~~~~~
uint64_t prepareStringTable(rld::Context &Ctxt, rld::Layout *const Lout,
                            const GlobalSymbolsContainer &Globals) {
  const uint64_t StringTableSize = std::accumulate(
      std::begin(Globals), std::end(Globals), uint64_t{1},
      [&Ctxt](const uint64_t Acc, const Symbol &Sym) {
        pstore::shared_sstring_view Owner;
        return Acc + loadString(Ctxt.Db, Sym.name(), &Owner).length() + 1U;
      });

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

// compute section file offsets
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compute the file offsets for the segments and the sections that they contain.
// The result is returned in the two output-parameters: SegmentFileOffsets and
// SectionFileOffsets. Note that these values are all 0 based: that is, no
// allowance has been made for ELF header/ segment table on the front of the
// file.
static uint64_t computeSectionFileOffsets(
    rld::Context &Ctxt, const rld::Layout &Lout,
    rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        *const SegmentFileOffsets,                                        // out
    rld::SectionArray<llvm::Optional<uint64_t>> *const SectionFileOffsets // out
) {
  using namespace rld;
  auto FileOffset = uint64_t{0};

  Lout.forEachSegment([&](const SegmentKind SegmentK, const Segment &Seg) {
    FileOffset = alignTo(FileOffset, Seg.MaxAlign);

    forEachSectionKind([&](SectionKind SectionK) {
      if (const OutputSection *const OutScn = Seg.Sections[SectionK]) {
        if (OutScn->shouldEmit()) {
          assert(
              !OutScn->Contributions.empty() &&
              "We can't have data for the output file with no contributions!");
          assert(!(*SectionFileOffsets)[SectionK] &&
                 "Layout should not have assigned a section type to more than "
                 "one segment");

          FileOffset = alignTo(FileOffset, OutScn->MaxAlign);
          {
            // If this is the first section contributing to this segment then
            // its offset it the start of the segment.
            llvm::Optional<uint64_t> &SFO = (*SegmentFileOffsets)[SegmentK];
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
    llvm::Optional<uint64_t> &Offset = (*SectionFileOffsets)[SectionK];
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

void overlapPhdrAndRODataSegments(
    Layout *const Lout,
    rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        *const SegmentFileOffsets,
    const uint64_t PhdrSegmentSize) {
  {
    Segment &phdr = Lout->Segments[SegmentKind::phdr];
    phdr.FileSize = PhdrSegmentSize;
    phdr.VirtualSize = PhdrSegmentSize;
    phdr.VirtualAddr += sizeof(Elf_Ehdr);
    phdr.MaxAlign = 1U;
  }
  (*SegmentFileOffsets)[SegmentKind::phdr] = sizeof(Elf_Ehdr);

  // Now adjust the ROData segment so that it covers the ELF header and segment
  // table as well as its actual content.
  static_assert(
      static_cast<std::underlying_type_t<SegmentKind>>(SegmentKind::phdr) + 1 ==
          static_cast<std::underlying_type_t<SegmentKind>>(SegmentKind::rodata),
      "Expected the ROData segment to follow the PHDR segment");
  auto const RODataOffset =
      (*SegmentFileOffsets)[SegmentKind::rodata].getValueOr(0U);
  Lout->Segments[SegmentKind::rodata].VirtualSize += RODataOffset;
  Lout->Segments[SegmentKind::rodata].FileSize += RODataOffset;

  (*SegmentFileOffsets)[SegmentKind::rodata] = 0;
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

llvm::Error rld::elfOutput(const llvm::StringRef &OutputFileName, Context &Ctxt,
                           const GlobalSymbolsContainer &Globals,
                           llvm::ThreadPool &WorkPool, Layout *const Lout) {
  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription);

  const uint64_t StringTableSize = prepareStringTable(Ctxt, Lout, Globals);
  const uint64_t SymbolTableSize = prepareSymbolTable<ELFT>(Lout, Globals);
  const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t> NameOffsets =
      buildSectionNameStringTable(Lout);

  // After this point, don't do anything that might cause additional sections to
  // be emitted. Once we've decided which names are going into the section name
  // string table, it's too late to add any more!

  Lout->Segments[SegmentKind::phdr].AlwaysEmit = true;

  // Flattens the layout so that sections and segments don't overlap and
  // establishes their position in the SectionData region of the final file.
  // Data here is 0 based.
  rld::SectionArray<llvm::Optional<uint64_t>> SectionFileOffsets;
  rld::SegmentIndexedArray<llvm::Optional<uint64_t>> SegmentFileOffsets;

  const uint64_t LayoutEnd = computeSectionFileOffsets(
      Ctxt, *Lout, &SegmentFileOffsets, &SectionFileOffsets);

  assert(SectionFileOffsets[rld::SectionKind::shstrtab].hasValue() &&
         "Expected the section names table to have been assigned an offset");
  assert(SectionFileOffsets[rld::SectionKind::strtab].hasValue() &&
         "Expected the strings table to have been assigned an offset");

  const unsigned NumSections = std::accumulate(
      std::begin(Lout->Sections), std::end(Lout->Sections), NumImplicitSections,
      [](const unsigned Acc, const rld::OutputSection &OutScn) {
        return Acc + static_cast<unsigned>(OutScn.shouldEmit());
      });
  const unsigned NumSegments = std::accumulate(
      std::begin(Lout->Segments), std::end(Lout->Segments), 0U,
      [](const unsigned Acc, const rld::Segment &Segment) {
        return Acc + static_cast<unsigned>(Segment.shouldEmit());
      });

  // emitSymbolTable (Ctxt, Globals);

  rld::EnumIndexedArray<Region, Region::Last, rld::FileRegion> FileRegions;
  FileRegions[Region::FileHeader] = rld::FileRegion{0, sizeof(Elf_Ehdr)};
  FileRegions[Region::SegmentTable] = rld::FileRegion{
      FileRegions[Region::FileHeader].end(), sizeof(Elf_Phdr) * NumSegments};
  FileRegions[Region::SectionData] = rld::FileRegion{
      alignTo(FileRegions[Region::SegmentTable].end(),
              NumSegments > 0U ? firstSegmentAlignment(*Lout) : 1U),
      LayoutEnd};
  FileRegions[Region::SectionTable] = rld::FileRegion{
      FileRegions[Region::SectionData].end(), NumSections * sizeof(Elf_Shdr)};

  flattenSegments(*Lout, FileRegions, &SegmentFileOffsets);
  overlapPhdrAndRODataSegments(Lout, &SegmentFileOffsets,
                               FileRegions[Region::SegmentTable].size());

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

  WorkPool.async([BufferStart, FileRegions, NumSegments, NumSections, Lout]() {
    // Write the ELF file header.
    auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(
        BufferStart + FileRegions[Region::FileHeader].offset());
    *Ehdr = rld::elf::initELFHeader<ELFT>(llvm::ELF::EM_X86_64);
    Ehdr->e_type = llvm::ELF::ET_EXEC;
    // FIXME: lookup symbol "_start" to get this value!
    Ehdr->e_entry = 0x0000000000200000; // Address of the program entry point.
    Ehdr->e_phnum = NumSegments;
    Ehdr->e_phoff = FileRegions[Region::SegmentTable].offset();
    Ehdr->e_shnum = NumSections;
    Ehdr->e_shoff = FileRegions[Region::SectionTable].offset();
    Ehdr->e_shstrndx = sectionStringTableIndex(
        *Lout); // TODO: need a general version which can quickly convert kind
                // to index for an section.
  });

  WorkPool.async([&Ctxt, BufferStart, NumSegments, Lout, &FileRegions,
                  &SegmentFileOffsets]() {
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
                  NumSections, &FileRegions]() {
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
                  SymbolTableSize, &SectionFileOffsets]() {
    // Produce the string table.
    assert(SectionFileOffsets[SectionKind::strtab].hasValue() &&
           "The strtab section should have been assigned an offset");
    assert(SectionFileOffsets[SectionKind::symtab].hasValue() &&
           "The symbol section should have been assigned an offset");

    auto *const StringData = BufferStart +
                             FileRegions[Region::SectionData].offset() +
                             *SectionFileOffsets[SectionKind::strtab];
    auto *StringOut = StringData;
    auto *const SymbolData = reinterpret_cast<Elf_Sym *>(
        BufferStart + FileRegions[Region::SectionData].offset() +
        *SectionFileOffsets[SectionKind::symtab]);
    auto *SymbolOut = SymbolData;

    *(StringOut++) = '\0'; // The string table's initial null entry.
    for (const Symbol &Sym : Globals) {
      // Build a symbol.
      {
        std::tuple<const Symbol::OptionalBodies &,
                   std::unique_lock<Symbol::Mutex>>
            Def = Sym.definition();
        const Symbol::OptionalBodies &Bodies = std::get<0>(Def);
        std::memset(SymbolOut, 0, sizeof(*SymbolOut));
        SymbolOut->st_name = StringOut - StringData;
        if (!Bodies) {
          // this ia an undefined symbol.
        } else {
          //          assert(Bodies->size() == 1);

          const Symbol::Body &B = Bodies->front();
          SymbolOut->st_value = Sym.value();
          SymbolOut->st_shndx = llvm::ELF::SHN_UNDEF;
          SymbolOut->st_size =
              0; // FIXME: the sum of the sizes of all of the bodies.
          SymbolOut->setVisibility(elf::elfVisibility<ELFT>(B.visibility()));
          SymbolOut->setBindingAndType(
              linkageToELFBinding(B.linkage()),
              sectionToSymbolType(SectionKind::data /*FIXME*/));
        }
        ++SymbolOut;
      }
      {
        // Copy a string.
        pstore::shared_sstring_view Owner;
        const pstore::raw_sstring_view Str =
            loadString(Ctxt.Db, Sym.name(), &Owner);
        StringOut = std::copy(std::begin(Str), std::end(Str), StringOut);
        *(StringOut++) = '\0';
      }
    }
    (void)StringTableSize;
    assert(StringOut > StringData &&
           static_cast<size_t>(StringOut - StringData) == StringTableSize);

    (void)SymbolTableSize;
    assert(SymbolOut >= SymbolData &&
           static_cast<size_t>(SymbolOut - SymbolData) == SymbolTableSize);
  });

  copyToOutput(Ctxt, WorkPool, BufferStart, *Lout, SectionFileOffsets,
               FileRegions[Region::SectionData].offset());
  WorkPool.wait();

  return (*Out)->commit();
}
