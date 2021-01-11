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
buildSectionStringTable(Layout *const Lout) {
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

// first segment alignment
// ~~~~~~~~~~~~~~~~~~~~~~~
static unsigned firstSegmentAlignment(const rld::Layout &Lout) {
  auto FirstSegment =
      std::find_if(std::begin(Lout.Segments), std::end(Lout.Segments),
                   [](Segment const &S) { return S.shouldEmit(); });
  assert(FirstSegment != std::end(Lout.Segments));
  return FirstSegment->MaxAlign;
}

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

using ELFT = llvm::object::ELFType<llvm::support::little, true>;
using Elf_Ehdr = llvm::object::ELFFile<ELFT>::Elf_Ehdr;
using Elf_Shdr = llvm::object::ELFFile<ELFT>::Elf_Shdr;
using Elf_Phdr = llvm::object::ELFFile<ELFT>::Elf_Phdr;
using Elf_Sym = llvm::object::ELFFile<ELFT>::Elf_Sym;

enum class Region { FileHeader, SegmentTable, SectionData, SectionTable, Last };

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
    Layout *const Lout, rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
                            *const SegmentFileOffsets) {
  {
    Segment &phdr = Lout->Segments[SegmentKind::phdr];
    phdr.FileSize = sizeof(Elf_Phdr) * NumSegments;
    phdr.VirtualSize = sizeof(Elf_Phdr) * NumSegments;
    phdr.VirtualAddr += sizeof(Elf_Ehdr);
    phdr.MaxAlign = 1U;
  }
  (*SegmentFileOffsets)[SegmentKind::phdr] = sizeof(Elf_Ehdr);

  // Now adjust the ROData segment so that it covers the ELF header and segment
  // table as well as its actual content.
  auto const RODataOffset =
      (*SegmentFileOffsets)[SegmentKind::rodata].getValueOr(0U);
  Lout->Segments[SegmentKind::rodata].VirtualSize += RODataOffset;
  Lout->Segments[SegmentKind::rodata].FileSize += RODataOffset;

  (*SegmentFileOffsets)[SegmentKind::rodata] = 0;
}

#if 0
void emitSymbolTable (Context &Ctxt, const GlobalSymbolsContainer &Globals) {
    auto &OS = llvm::dbgs();
    OS << "There are " << Globals.size() << " symbols\n";
struct Elf64_Sym {
        Elf64_Word st_name;
        unsigned char st_info;
        unsigned char st_other;
        Elf64_Half st_shndx;
        Elf64_Addr st_value;
        Elf64_Xword st_size;
};

using namespace rld;
    for (auto const &S : Globals) {
Elf_Sym symbol;

      // Note that requesting a symbol's definition returns an owned lock on the
      // object. That means that we need to get the name first since accessing
      // both will try to acquire the same lock.
      StringAddress const Name = S.name();
      std::tuple<Symbol::OptionalBodies const &, std::unique_lock<Symbol::Mutex>> const X = S.definition();
      Symbol::OptionalBodies const &Def = std::get<Symbol::DefinitionIndex>(X);

std::memset(&symbol, 0, sizeof(symbol));
symbol.st_name = 0;///name offset; // how to compute?

      bool const IsDefined = Def.hasValue();
      pstore::shared_sstring_view Owner;
      OS << "  " << stringViewAsRef(loadString(Ctxt.Db, Name, &Owner)) << ": defined: " << (IsDefined ? "yes" : "no");
      if (IsDefined) {
        assert (Def->size () > 0);
#if 0
        unsigned char st_info;
        unsigned char st_other;
        Elf64_Half st_shndx;
        Elf64_Addr st_value;
        Elf64_Xword st_size;
#endif
      } else {
        // an undef symbol.
      }

      OS << "\n";
    }
}
#endif

llvm::Error rld::elfOutput(const llvm::StringRef &OutputFileName, Context &Ctxt,
                           const GlobalSymbolsContainer &Globals,
                           llvm::ThreadPool &WorkPool, Layout *const Lout) {
  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription);

  const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t> NameOffsets =
      buildSectionStringTable(Lout);
  const uint64_t StringTableSize = prepareStringTable(Ctxt, Lout, Globals);

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
      FileRegions[Region::FileHeader].end(), NumSegments * sizeof(Elf_Phdr)};
  FileRegions[Region::SectionData] = rld::FileRegion{
      alignTo(FileRegions[Region::SegmentTable].end(),
              NumSegments > 0U ? firstSegmentAlignment(*Lout) : 1U),
      LayoutEnd};
  FileRegions[Region::SectionTable] = rld::FileRegion{
      FileRegions[Region::SectionData].end(), NumSections * sizeof(Elf_Shdr)};

  flattenSegments(*Lout, FileRegions, &SegmentFileOffsets);
  overlapPhdrAndRODataSegments(Lout, &SegmentFileOffsets);

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
    assert(End - Start == NumSections);
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
    assert(SectionFileOffsets[SectionKind::strtab].hasValue() &&
           "The strtab section should have been assigned an offset");
    auto *const Data = BufferStart + FileRegions[Region::SectionData].offset() +
                       *SectionFileOffsets[SectionKind::strtab];

    auto *Out = Data;
    *(Out++) = '\0';
    for (const Symbol &Sym : Globals) {
      pstore::shared_sstring_view Owner;
      const pstore::raw_sstring_view Str =
          loadString(Ctxt.Db, Sym.name(), &Owner);
      Out = std::copy(std::begin(Str), std::end(Str), Out);
      *(Out++) = '\0';
    }
    (void)StringTableSize;
    assert(Out > Data && static_cast<size_t>(Out - Data) == StringTableSize);
  });

  copyToOutput(Ctxt, WorkPool, BufferStart, *Lout, SectionFileOffsets,
               FileRegions[Region::SectionData].offset());
  WorkPool.wait();

  return (*Out)->commit();
}
