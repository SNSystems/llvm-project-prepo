//*       _     _  *
//*  _ __| | __| | *
//* | '__| |/ _` | *
//* | |  | | (_| | *
//* |_|  |_|\__,_| *
//*                *
//===- tools/rld/rld.cpp --------------------------------------------------===//
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

#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileOutputBuffer.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/Threading.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/ToolOutputFile.h"

#include "pstore/core/database.hpp"
#include "pstore/core/hamt_map.hpp"
#include "pstore/core/hamt_set.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/support/array_elements.hpp"
#include "pstore/support/uint128.hpp"

#include "rld/ErrorCode.h"
#include "rld/Identify.h"
#include "rld/IdentifyPass.h"
#include "rld/LayoutBuilder.h"
#include "rld/MathExtras.h"
#include "rld/SectionArray.h"
#include "rld/copy.h"
#include "rld/elf.h"
#include "rld/scanner.h"
#include "rld/types.h"

#include <bitset>
#include <cassert>
#include <cstdlib>
#include <thread>

namespace {

constexpr auto DebugType = "rld-main";

llvm::cl::opt<std::string> RepoPath("repo", llvm::cl::Optional,
                                    llvm::cl::desc("Program repository path"),
                                    llvm::cl::init("./clang.db"));
llvm::cl::list<std::string> InputPaths(llvm::cl::Positional,
                                       llvm::cl::desc("<ticket path>"));
llvm::cl::opt<std::string> EntryPoint("entry-point",
                                      llvm::cl::desc("The entry point"),
                                      llvm::cl::value_desc("symbol"),
                                      llvm::cl::init("_start"));
llvm::cl::alias EntryPointA("E", llvm::cl::desc("Alias for --entry-point"),
                            llvm::cl::aliasopt(EntryPoint));

llvm::cl::opt<std::string> OutputFileName("o",
                                          llvm::cl::desc("Output filename"),
                                          llvm::cl::value_desc("filename"),
                                          llvm::cl::init("./a.out"));
llvm::cl::opt<unsigned> NumWorkers(
    "workers", llvm::cl::desc("Number of worker threads"),
    llvm::cl::init(llvm::heavyweight_hardware_concurrency().compute_thread_count()));

} // end anonymous namespace

static std::string getRepoPath() {
  if (RepoPath.getNumOccurrences() == 0) {
    // TODO: remove this environment variable once the matching behavior is
    // removed from the compiler.
    if (auto File = getenv("REPOFILE")) {
      return {File};
    }
  }
  return RepoPath;
}

template <typename T> class AsHex {
public:
  explicit AsHex(T t) : t_{t} {}
  T value() const noexcept { return t_; }

private:
  T t_;
};

template <typename T> AsHex<T> toHex(T t) { return AsHex<T>{t}; }

template <typename T>
llvm::raw_ostream &operator<<(llvm::raw_ostream &os, AsHex<T> const &v) {
  os << "0x";
  return os.write_hex(v.value());
}


static llvm::ExitOnError ExitOnErr;

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, rld::SegmentKind kind) {
#define X(x)                                                                   \
  case rld::SegmentKind::x:                                                    \
    os << #x;                                                                  \
    break;

  switch (kind) {
    RLD_SEGMENT_KIND
  default:
    llvm_unreachable("Bad segment kind");
  }
#undef X
  return os;
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, rld::SectionKind SKind) {
#define X(x)                                                                   \
  case rld::SectionKind::x:                                                    \
    os << #x;                                                                  \
    break;

  switch (SKind) {
    PSTORE_MCREPO_SECTION_KINDS
  case rld::SectionKind::shstrtab:
    os << "shstrtab";
    break;
  case rld::SectionKind::strtab:
    os << "strtab";
    break;
  case rld::SectionKind::last:
    os << "last"; // Never used. Always last.
    break;
  }
#undef X
  return os;
}



#if ADD_IDENT_SEGMENT
constexpr pstore::index::digest interp_fragment_digest{
    0x31415926,
    0x53589793,
};
constexpr pstore::index::digest interp_compilation{
    0x31415926,
    0x53589793,
};

static auto
makeInterpFragment(pstore::transaction<pstore::transaction_lock> &Transaction,
                   std::shared_ptr<pstore::index::fragment_index> const &Index)
    -> pstore::extent<pstore::repo::fragment> {

  using pstore::repo::fragment;
  using pstore::repo::section_content;
  using pstore::repo::section_kind;

  static constexpr char Interp[] = "/lib/ld-linux.so.2";

  section_content Content{section_kind::interp, std::uint8_t{1} /*alignment*/};
  std::copy(Interp, Interp + pstore::array_elements(Interp),
            std::back_inserter(Content.data));

  std::array<pstore::repo::generic_section_creation_dispatcher, 1> Dispatchers{
      {{section_kind::interp, &Content}}};

  pstore::extent<fragment> const FragmentExtent = fragment::alloc(
      Transaction, std::begin(Dispatchers), std::end(Dispatchers));
  Index->insert(Transaction,
                std::make_pair(interp_fragment_digest, FragmentExtent));
  return FragmentExtent;
}

static auto makeInterpCompilation(
    pstore::transaction<pstore::transaction_lock> &Transaction,
    llvm::Triple const &Triple,
    std::shared_ptr<pstore::index::compilation_index> const &Compilations,
    std::shared_ptr<pstore::index::name_index> const &Names,
    pstore::extent<pstore::repo::fragment> const &FragmentDxtent)
    -> pstore::extent<pstore::repo::compilation> {

  using pstore::indirect_string;
  using pstore::typed_address;
  using pstore::repo::compilation;
  using pstore::repo::compilation_member;
  using pstore::repo::linkage;

  // Use the string adder to insert a string into the index and flush it to the
  // store.
  pstore::indirect_string_adder adder;

  std::string const NormTriple = Triple.normalize();
  auto const TripleView = pstore::make_sstring_view(NormTriple);
  auto const TripleAddr = typed_address<indirect_string>::make(
      adder.add(Transaction, Names, &TripleView).first.get_address());

  auto const SymbolNameView = pstore::make_sstring_view("/ident");
  auto const SymbolAddr = typed_address<indirect_string>::make(
      adder.add(Transaction, Names, &SymbolNameView).first.get_address());

  auto const PathNameView = pstore::make_sstring_view("/ident/");
  auto const PathAddr = typed_address<indirect_string>::make(
      adder.add(Transaction, Names, &PathNameView).first.get_address());

  compilation_member Symbol{interp_fragment_digest, FragmentDxtent, SymbolAddr,
                            linkage::internal_no_symbol};
  pstore::extent<compilation> const CompilationExtent = compilation::alloc(
      Transaction, PathAddr, TripleAddr, &Symbol, &Symbol + 1);

  Compilations->insert(Transaction,
                       std::make_pair(interp_compilation, CompilationExtent));

  adder.flush(Transaction);
  return CompilationExtent;
}

static auto generateFixedContent(pstore::database &Db,
                                 llvm::Triple const &Triple)
    -> llvm::ErrorOr<pstore::extent<pstore::repo::compilation>> {
  auto Compilations =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  if (Compilations == nullptr) {
    return rld::ErrorCode::CompilationIndexNotFound;
  }

  auto Fragments =
      pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
  if (Fragments == nullptr) {
    return rld::ErrorCode::FragmentIndexNotFound;
  }

  auto names = pstore::index::get_index<pstore::trailer::indices::name>(Db);
  if (Compilations == nullptr) {
    return rld::ErrorCode::NamesIndexNotFound;
  }

  auto const InterpFragmentPos = Fragments->find(Db, interp_fragment_digest);
  auto const InterpCompilationPos = Compilations->find(Db, interp_compilation);
  auto const HaveInterpFragment = InterpFragmentPos != Fragments->end(Db);
  auto const HaveInterpCompilation =
      InterpCompilationPos != Compilations->end(Db);
  if (HaveInterpFragment && HaveInterpCompilation) {
    return InterpCompilationPos->second;
  }

  auto Transaction = pstore::begin(Db);
  auto const FragmentExtent = HaveInterpFragment
                                  ? InterpFragmentPos->second
                                  : makeInterpFragment(Transaction, Fragments);
  pstore::extent<pstore::repo::compilation> const CompilationExtent =
      HaveInterpCompilation
          ? InterpCompilationPos->second
          : makeInterpCompilation(Transaction, Triple, Compilations, names,
                                  FragmentExtent);
  Transaction.commit();

  return CompilationExtent;
}
#endif

static llvm::ErrorOr<std::unique_ptr<pstore::database>> openRepository() {
  std::string const FilePath = getRepoPath();
  if (!llvm::sys::fs::exists(FilePath)) {
    return rld::ErrorCode::DatabaseNotFound;
  }
  return std::make_unique<pstore::database>(
      FilePath, pstore::database::access_mode::writable);
}

uint64_t computeSectionFileOffsets(
    rld::Context &Ctxt, const rld::Layout &Lout,
    rld::SegmentIndexedArray<llvm::Optional<uint64_t>>
        *const SegmentFileOffsets,                                        // out
    rld::SectionArray<llvm::Optional<uint64_t>> *const SectionFileOffsets // out
) {
  using namespace rld;
  auto FileOffset = uint64_t{0};

  forEachSegmentKind([&](SegmentKind SegmentK) {
    const Segment &Seg = Lout.Segments[SegmentK];

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

          llvm::Optional<uint64_t> &SFO = (*SegmentFileOffsets)[SegmentK];
          if (!SFO) {
            SFO = FileOffset;
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

using namespace rld;

static constexpr auto NumImplicitSections = 1U; // The null section.

// Produce the section names string table.
void stringTableWriter(Context &, const OutputSection &OScn, uint8_t *Data,
                       const Layout &Lout) {
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

auto buildSectionStringTable(Layout *const Lout)
    -> EnumIndexedArray<SectionKind, SectionKind::last, uint64_t> {
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
  ShStrTab.Writer = stringTableWriter;
  return NameOffsets;
}

unsigned SectionStringTableIndex(const Layout &Lout) {
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

unsigned firstSegmentAlignment(const rld::Layout &Lout) {
  auto FirstSegment =
      std::find_if(std::begin(Lout.Segments), std::end(Lout.Segments),
                   [](Segment const &S) { return S.shouldEmit(); });
  assert(FirstSegment != std::end(Lout.Segments));
  return FirstSegment->MaxAlign;
}

void elfOutput(rld::Context &Ctxt, llvm::ThreadPool &WorkPool,
               rld::Layout *const Lout) {
  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription);

  using ELFT = llvm::object::ELFType<llvm::support::little, true>;
  using Elf_Ehdr = llvm::object::ELFFile<ELFT>::Elf_Ehdr;
  using Elf_Shdr = llvm::object::ELFFile<ELFT>::Elf_Shdr;
  using Elf_Phdr = llvm::object::ELFFile<ELFT>::Elf_Phdr;

  const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t> NameOffsets =
      buildSectionStringTable(Lout);

  // Flattens the layout so that sections and segments don't overlap and
  // establishes their position in the SectionData region of the final file.
  // Data here is 0 based.
  rld::SectionArray<llvm::Optional<uint64_t>> SectionFileOffsets;
  rld::SegmentIndexedArray<llvm::Optional<uint64_t>> SegmentFileOffsets;

  const uint64_t LayoutEnd = computeSectionFileOffsets(
      Ctxt, *Lout, &SegmentFileOffsets, &SectionFileOffsets);
  assert(SectionFileOffsets[rld::SectionKind::shstrtab].hasValue());

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

  enum class Region {
    FileHeader,
    SegmentTable,
    SectionData,
    SectionTable,
    Last
  };
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

  uint64_t const TotalSize = FileRegions.back().end();
  rld::llvmDebug(DebugType, Ctxt.IOMut, [TotalSize]() {
    llvm::dbgs() << "Output file size=" << TotalSize << '\n';
  });

  auto Out = ExitOnErr(llvm::FileOutputBuffer::create(
      OutputFileName, TotalSize,
      llvm::FileOutputBuffer::
          F_executable /*| llvm::FileOutputBuffer::F_modify*/));

  uint8_t *const BufferStart = Out->getBufferStart();

  WorkPool.async([BufferStart, FileRegions, NumSegments, NumSections, Lout]() {
    // Write the ELF file header.
    auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(
        BufferStart + FileRegions[Region::FileHeader].offset());
    *Ehdr = rld::elf::initELFHeader<ELFT>(llvm::ELF::EM_X86_64);
    Ehdr->e_type = llvm::ELF::ET_EXEC;
    Ehdr->e_entry =
        0x0000000000200000; // Address to jump to in order to start program
    Ehdr->e_phnum = NumSegments;
    Ehdr->e_phoff = FileRegions[Region::SegmentTable].offset();
    Ehdr->e_shnum = NumSections;
    Ehdr->e_shoff = FileRegions[Region::SectionTable].offset();
    Ehdr->e_shstrndx = SectionStringTableIndex(
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

  copyToOutput(Ctxt, WorkPool, BufferStart, *Lout, SectionFileOffsets,
               FileRegions[Region::SectionData].offset());
  WorkPool.wait();

  ExitOnErr(Out->commit());
}

int main(int Argc, char *Argv[]) {
  llvm::cl::ParseCommandLineOptions(Argc, Argv);

  ExitOnErr.setBanner(std::string{Argv[0]} + ": error: ");

  llvm::ErrorOr<std::unique_ptr<pstore::database>> Db = openRepository();
  if (!Db) {
    llvm::errs() << "Error: " << Db.getError().message() << '\n';
    return EXIT_FAILURE;
  }

  rld::Context Ctxt{*Db.get()};

  auto CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Ctxt.Db);
  if (!CompilationIndex) {
    llvm::errs()
        << "Error: "
        << make_error_code(rld::ErrorCode::CompilationIndexNotFound).message()
        << '\n';
    return EXIT_FAILURE;
  }

  llvm::ThreadPool WorkPool(llvm::heavyweight_hardware_concurrency(NumWorkers));
  llvm::ErrorOr<rld::IdentifyResult> Identified =
      rld::identifyPass(Ctxt, WorkPool, CompilationIndex, InputPaths);
  if (!Identified) {
    llvm::errs() << "Error: " << Identified.getError().message() << '\n';
    std::exit(EXIT_FAILURE);
  }

  rld::UndefsContainer Undefs;
  auto GlobalSymbs =
      std::make_unique<rld::GlobalsStorage>(NumWorkers.getValue());

  std::unique_ptr<rld::Layout> LO;
  {
    llvm::NamedRegionTimer LayoutTimer("Layout", "Output file layout",
                                       rld::TimerGroupName,
                                       rld::TimerGroupDescription);

    // The plus one here allows for the linker-generated /interp/ file.
#if ADD_IDENT_SEGMENT
    auto const NumCompilations = InputPaths.size() + std::size_t{1};
#else
    auto const NumCompilations = InputPaths.size();
#endif
    if (NumCompilations > std::numeric_limits<uint32_t>::max()) {
      llvm::errs() << "Error: Too many input files\n";
      std::exit(EXIT_FAILURE);
    }
    rld::LayoutBuilder Layout{Ctxt, GlobalSymbs.get(),
                              static_cast<uint32_t>(NumCompilations)};
    std::thread LayoutThread{&rld::LayoutBuilder::run, &Layout};

    llvm::Optional<llvm::Triple> Triple;

    {
      llvm::NamedRegionTimer ScanTimer("Scan", "Input file scanning",
                                       rld::TimerGroupName,
                                       rld::TimerGroupDescription);

      rld::Scanner Scan{Ctxt, Layout, &Undefs};
      for (const auto &C : Identified->Compilations) {
        WorkPool.async(
            [&Scan, &GlobalSymbs](
                const rld::Identifier::CompilationVector::value_type &V) {
              Scan.run(std::get<std::string>(V), // path
                       GlobalSymbs->getThreadSymbols(),
                       std::get<pstore::extent<pstore::repo::compilation>>(
                           V),             // compilation extent
                       std::get<size_t>(V) // input ordinal
              );
            },
            std::cref(C));
      }

      WorkPool.wait();

      Triple = Ctxt.triple();
      if (!Triple) {
        llvm::errs() << "Error: The output triple could not be determined.\n";
        return EXIT_FAILURE;
      }
#if ADD_IDENT_SEGMENT
      auto const FixedCompilationExtent =
          generateFixedContent(Ctxt.Db, *Triple);
      if (!FixedCompilationExtent) {
        llvm::errs() << "Error: " << FixedCompilationExtent.getError().message()
                     << '\n';
        return EXIT_FAILURE;
      }
      Scan.run("/ident/", // path
               GlobalSymbs->getThreadSymbols(),
               *FixedCompilationExtent,        // compilation extent
               Identified->Compilations.size() // input ordinal
      );
#endif
    }
    LayoutThread.join();
    LO = Layout.flattenSegments();
  }

  rld::llvmDebug(DebugType, Ctxt.IOMut, [&Ctxt] {
    llvm::dbgs() << "Output triple: " << Ctxt.triple()->normalize() << '\n';
  });

  // Now we set about emitting an ELF executable...
  elfOutput(Ctxt, WorkPool, LO.get());

  // Avoid calling the destructors of some of our global objects. We can simply
  // let the O/S do that instead. Remove these calls if looking for memoryleaks,
  // though!
  GlobalSymbs.release();
  LO.release();
  std::cout << "All done\n";
}
