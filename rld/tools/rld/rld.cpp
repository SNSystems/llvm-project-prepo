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
    llvm::cl::init(std::max(llvm::heavyweight_hardware_concurrency(), 1U)));

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

template <pstore::repo::section_kind SKind,
          typename SType = typename pstore::repo::enum_to_section<SKind>::type>
void copy_section(rld::Context &Ctxt, rld::SectionInfo const &S,
                  std::uint8_t *Dest) {
  auto *const Section = reinterpret_cast<SType const *>(S.Section);

  rld::llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "copy to " << toHex(reinterpret_cast<std::uintptr_t>(Dest))
                 << '\n';
  });

  auto const d = Section->payload();
  std::memcpy(Dest, d.begin(), d.size());

  // auto * const shadow = Ctxt.shadow();

#if 0
  for (auto const &XFixup : Section->xfixups()) {

      switch (XFixup.type) {
      // target-dependent relocations here...
            typed_address<indirect_string> name;
            relocation_type type;
            std::uint8_t padding1 = 0;
            std::uint16_t padding2 = 0;
            std::uint32_t padding3 = 0;
            std::uint64_t offset;
            std::uint64_t addend;
    }
  }
#endif
}

template <>
void copy_section<pstore::repo::section_kind::bss>(rld::Context &Ctxt,
                                                   rld::SectionInfo const &S,
                                                   std::uint8_t *Dest) {
  rld::llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "bss fill " << reinterpret_cast<std::uintptr_t>(Dest)
                 << '\n';
  });
  static_assert(
      std::is_same<
          pstore::repo::enum_to_section<pstore::repo::section_kind::bss>::type,
          pstore::repo::bss_section>::value,
      "BSS section kind must map to BSS section type");
}

template <>
void copy_section<pstore::repo::section_kind::dependent>(
    rld::Context &Ctxt, rld::SectionInfo const &S, std::uint8_t *Dest) {
  // discard
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

void copyToOutput(rld::Context &Ctxt, llvm::ThreadPool &WorkPool,
                  rld::LayoutOutput const &LO,
                  std::uint8_t *const Data /*output buffer*/,
                  std::uint64_t const StartOffset) {
  for (auto SegmentIndex = std::size_t{0}, NumSegments = LO.size();
       SegmentIndex < NumSegments; ++SegmentIndex) {
    rld::Segment const &Segment = LO[SegmentIndex];

    assert(Segment.Sections.size() ==
           static_cast<std::underlying_type<rld::SectionKind>::type>(
               rld::SectionKind::last));
    for (auto SectionIndex = std::size_t{0},
              NumSections = Segment.Sections.size();
         SectionIndex < NumSections; ++SectionIndex) {

      WorkPool.async([=, &Segment, &Ctxt]() {
        for (rld::SectionInfo const &Section : Segment.Sections[SectionIndex]) {
          rld::llvmDebug(DebugType, Ctxt.IOMut, [SectionIndex, SegmentIndex]() {
            llvm::dbgs() << static_cast<rld::SegmentKind>(SegmentIndex) << '/'
                         << static_cast<rld::SectionKind>(SectionIndex) << ": ";
          });

          auto Off = rld::alignTo(Section.Offset, Section.Align);
          auto *const Dest = Data + Off + StartOffset;
          switch (static_cast<pstore::repo::section_kind>(SectionIndex)) {
#define X(a)                                                                   \
  case pstore::repo::section_kind::a:                                          \
    copy_section<pstore::repo::section_kind::a>(Ctxt, Section, Dest);          \
    break;

            PSTORE_MCREPO_SECTION_KINDS
#undef X
          case pstore::repo::section_kind::last:
            llvm_unreachable("Bad section kind");
            break;
          }
        }
      });
    }
  }
}

template <typename ELFT>
void initELFHeader(
    rld::NotNull<typename llvm::object::ELFFile<ELFT>::Elf_Ehdr *> Header,
    unsigned Machine) {
  using namespace llvm::ELF;
  using ElfDetails = llvm::object::ELFFile<ELFT>;

  std::memset(Header, 0, sizeof(*Header));
  Header->e_ident[EI_MAG0] = 0x7f;
  Header->e_ident[EI_MAG1] = 'E';
  Header->e_ident[EI_MAG2] = 'L';
  Header->e_ident[EI_MAG3] = 'F';
  Header->e_ident[EI_CLASS] = ELFT::Is64Bits ? ELFCLASS64 : ELFCLASS32;
  Header->e_ident[EI_DATA] = ELFT::TargetEndianness == llvm::support::little
                                 ? ELFDATA2LSB
                                 : ELFDATA2MSB;
  Header->e_ident[EI_VERSION] = EV_CURRENT;
  Header->e_ident[EI_OSABI] = 0;      // Doc.Header.OSABI;
  Header->e_ident[EI_ABIVERSION] = 0; // Doc.Header.ABIVersion;
  Header->e_type = 0;                 // Doc.Header.Type;
  Header->e_machine = Machine;
  Header->e_version = EV_CURRENT;
  Header->e_entry = 0; // Doc.Header.Entry;
  Header->e_phoff = sizeof(Header);
  Header->e_flags = 0; // Doc.Header.Flags;
  Header->e_ehsize = sizeof(typename ElfDetails::Elf_Ehdr);
  Header->e_phentsize = sizeof(typename ElfDetails::Elf_Phdr);
  Header->e_phnum = 0; // Doc.ProgramHeaders.size();
  Header->e_shentsize = sizeof(typename ElfDetails::Elf_Shdr);
  Header->e_shoff = 0; // sizeof(Header) + sizeof(typename ElfDetails::Elf_Phdr)
                       // * 0;//Doc.ProgramHeaders.size();
  Header->e_shnum = 0; // getSectionCount();
  Header->e_shstrndx = 0; // getDotShStrTabSecNo();
}

template <typename Function>
void for_each_section(rld::LayoutOutput const &LO, Function f) {
  for (rld::Segment const &Segment : LO) {
    assert(Segment.Sections.size() ==
           static_cast<std::underlying_type<rld::SectionKind>::type>(
               rld::SectionKind::last));

    for (rld::SectionKind SKind = rld::firstSectionKind();
         SKind != rld::SectionKind::last; ++SKind) {
      for (rld::SectionInfo const &SI :
           Segment.Sections[static_cast<size_t>(SKind)]) {
        if (SI.Size > 0) {
          f(SKind, SI);
        }
      }
    }
  }
}

constexpr pstore::index::digest interp_fragment_digest{
    0x31415926,
    0x53589793,
};
constexpr pstore::index::digest interp_compilation{
    0x31415926,
    0x53589793,
};

auto makeInterpFragment(
    pstore::transaction<pstore::transaction_lock> &Transaction,
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

auto makeInterpCompilation(
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

auto generateFixedContent(pstore::database &Db, llvm::Triple const &Triple)
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

static llvm::ErrorOr<std::unique_ptr<pstore::database>> openRepository() {
  std::string const FilePath = getRepoPath();
  if (!llvm::sys::fs::exists(FilePath)) {
    return rld::ErrorCode::DatabaseNotFound;
  }
  return std::make_unique<pstore::database>(
      FilePath, pstore::database::access_mode::writable);
}

void elfOutput(rld::Context &Ctxt, llvm::ThreadPool &WorkPool,
               rld::LayoutOutput const &Layout) {
  llvm::NamedRegionTimer Timer("ELF Output", "Binary Output Phase",
                               rld::TimerGroupName, rld::TimerGroupDescription);

  using ELFT = llvm::object::ELFType<llvm::support::little, true>;
  using Elf_Ehdr = llvm::object::ELFFile<ELFT>::Elf_Ehdr;
  using Elf_Shdr = llvm::object::ELFFile<ELFT>::Elf_Shdr;
  using Elf_Phdr = llvm::object::ELFFile<ELFT>::Elf_Phdr;

  rld::SectionArray<rld::elf::ElfSectionInfo> ElfSections;
  static constexpr auto NumImplicitSections = 1U; // The null section.

  ElfSections[rld::SectionKind::shstrtab].Emit = true;
  ElfSections[rld::SectionKind::shstrtab].Align = 1;

  for_each_section(Layout, [&ElfSections](rld::SectionKind Kind,
                                          rld::SectionInfo const &SI) {
    if (SI.Size > 0) {
      auto &ESI = ElfSections[Kind];
      ESI.Emit = true;
      // ESI.Offset = std::min(ESI.Offset, SI.Offset);
      ESI.Size += SI.Size; // FIXME: alignment?
      ESI.Align = std::max(ESI.Align, SI.Align);
    }
  });

  auto NumSections = NumImplicitSections;
  auto SectionNameSize = std::uint64_t{1};
  auto SectionHeaderNamesSectionIndex = 0;
  for (rld::SectionKind SectionK = rld::firstSectionKind();
       SectionK != rld::SectionKind::last; ++SectionK) {
    auto &ESI = ElfSections[SectionK];
    if (ESI.Emit) {
      if (SectionK == rld::SectionKind::shstrtab) {
        SectionHeaderNamesSectionIndex = NumSections;
      }
      ++NumSections;

      assert(ESI.NameOffset == 0);
      ESI.NameOffset = SectionNameSize;
      SectionNameSize +=
          rld::elf::elfSectionNameAndLength(SectionK).second + 1U;
    }
  }

  // Count the number of segments to be emitted and assign the address of each
  // output section.
  auto NumSegments = 0U;
  rld::for_each_segment(Layout, [&ElfSections, &Layout,
                                 &NumSegments](rld::SegmentKind SegmentK,
                                               rld::Segment const &Segment) {
    if (!Segment.shouldEmit()) {
      return;
    }

    ++NumSegments;

    if (rld::elf::hasPhysicalAddress(SegmentK)) {
      auto const SegmentIndex =
          static_cast<std::underlying_type<decltype(SegmentK)>::type>(SegmentK);
      auto Offset = uint64_t{0};

      for (rld::SectionKind SectionK = rld::firstSectionKind();
           SectionK != rld::SectionKind::last; ++SectionK) {
        const auto SectionIndex =
            static_cast<std::underlying_type<decltype(SectionK)>::type>(
                SectionK);
        if (!Layout[SegmentIndex].Sections[SectionIndex].empty()) {
          auto &ESI = ElfSections[SectionK];
          const auto Start = rld::alignTo(Offset, ESI.Align);
          ESI.Address = Segment.VirtualAddr + Start;
          Offset = Start + ESI.Size;
        }
      }
    }
  });

  assert(NumSections == std::count_if(std::begin(ElfSections),
                                      std::end(ElfSections),
                                      [](rld::elf::ElfSectionInfo const &ESI) {
                                        return ESI.Emit;
                                      }) +
                            NumImplicitSections);

  uint64_t const TargetDataSize =
      std::accumulate(std::begin(Layout), std::end(Layout), uint64_t{0},
                      [](uint64_t RunningTotal, rld::Segment const &Segment) {
                        return rld::alignTo(RunningTotal, Segment.MaxAlign) +
                               Segment.VirtualSize;
                      });

  enum Region {
    FileHeader,
    SegmentTable,
    TargetData,
    SectionTable,
    SectionNames,
    Last // TODO: add symtab and strings to this collection.
  };
  // Meaning of the two fields in RegionTuple.
  enum LocationIndexes {
    OffsetIndex,
    SizeIndex
  };
  using RegionTuple = std::tuple<uint64_t, uint64_t>;
  std::array<RegionTuple, Region::Last> FileRegions = {{
      RegionTuple{0, sizeof(Elf_Ehdr)},
  }};

  auto RegionOffset = [&FileRegions](Region R) {
    return std::get<OffsetIndex>(FileRegions[static_cast<size_t>(R)]);
  };
  auto RegionSize = [&FileRegions](Region R) {
    return std::get<SizeIndex>(FileRegions[static_cast<size_t>(R)]);
  };
  auto RegionEnd = [&](Region R) { return RegionOffset(R) + RegionSize(R); };

  FileRegions[Region::SegmentTable] = RegionTuple{
      RegionEnd(Region::FileHeader), NumSegments * sizeof(Elf_Phdr)};
  FileRegions[Region::TargetData] =
      RegionTuple{RegionEnd(Region::SegmentTable), TargetDataSize};

  FileRegions[Region::SectionTable] = RegionTuple{
      RegionEnd(Region::TargetData), NumSections * sizeof(Elf_Shdr)};
  FileRegions[Region::SectionNames] =
      RegionTuple{RegionEnd(Region::SectionTable), SectionNameSize};

  ElfSections[rld::SectionKind::shstrtab].Offset =
      RegionOffset(Region::SectionNames);
  ElfSections[rld::SectionKind::shstrtab].Size =
      RegionSize(Region::SectionNames);

  {
    constexpr auto Kind = rld::SectionKind::shstrtab;
    const auto &ESI = ElfSections[Kind];
    llvm::dbgs() << "section:" << Kind
                 << ", Offset:" << rld::format_hex(ESI.Offset)
                 << ", Size:" << rld::format_hex(ESI.Size)
                 << ", Align:" << rld::format_hex(ESI.Align) << '\n';
  }

  {
    // We now know where the major blocks are going to end up in the output
    // file. Now adjust each of the target data sections so that they don't
    // overlap one anther and all lie inside the target data's region
    // (Region::TargetData).
    auto NextOffset = RegionOffset(Region::TargetData);
    for (rld::SectionKind Kind = rld::firstSectionKind();
         Kind != rld::SectionKind::last; ++Kind) {
      auto &ESI = ElfSections[Kind];
      if (ESI.Emit && ESI.Offset == 0) {
        ESI.Offset = NextOffset;

        NextOffset = ESI.Offset + ESI.Size; // FIXME: alignment?
        llvm::dbgs() << "section:" << Kind
                     << ", Offset:" << rld::format_hex(ESI.Offset)
                     << ", Size:" << rld::format_hex(ESI.Size)
                     << ", Align:" << rld::format_hex(ESI.Align) << '\n';
      }
    }
    assert(NextOffset <= RegionEnd(Region::TargetData));
    llvm::dbgs() << "target data end=" << NextOffset
                 << ", allocated space end=" << RegionEnd(Region::TargetData)
                 << '\n';
  }

  uint64_t const TotalSize = RegionEnd(Region::SectionNames);

  rld::llvmDebug(DebugType, Ctxt.IOMut, [TotalSize]() {
    llvm::dbgs() << "Output file size=" << TotalSize << '\n';
  });

  auto Out = ExitOnErr(llvm::FileOutputBuffer::create(
      OutputFileName, TotalSize,
      llvm::FileOutputBuffer::
          F_executable /*| llvm::FileOutputBuffer::F_modify*/));

  std::uint8_t *const BufferStart = Out->getBufferStart();

  WorkPool.async([BufferStart, RegionOffset, NumSegments, NumSections,
                  SectionHeaderNamesSectionIndex]() {
    auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(
        BufferStart + RegionOffset(Region::FileHeader));
    initELFHeader<ELFT>(Ehdr, llvm::ELF::EM_X86_64);
    Ehdr->e_type = llvm::ELF::ET_EXEC;
    Ehdr->e_phnum = NumSegments;
    Ehdr->e_phoff = RegionOffset(Region::SegmentTable);
    Ehdr->e_shnum = NumSections;
    Ehdr->e_shoff = RegionOffset(Region::SectionTable);
    Ehdr->e_shstrndx = SectionHeaderNamesSectionIndex;
  });

  WorkPool.async(
      [BufferStart, RegionOffset, RegionSize, NumSegments, &Layout]() {
        auto *const PhdrStartPtr = reinterpret_cast<Elf_Phdr *>(
            BufferStart + RegionOffset(Region::SegmentTable));
        auto *const PhdrEndPtr = rld::elf::emitProgramHeaders<ELFT>(
            PhdrStartPtr, RegionOffset(Region::TargetData), Layout);
        (void)PhdrEndPtr;
        assert(PhdrEndPtr - PhdrStartPtr == NumSegments);
        assert(reinterpret_cast<std::uint8_t *>(PhdrStartPtr) +
                   RegionSize(Region::SegmentTable) ==
               reinterpret_cast<std::uint8_t *>(PhdrEndPtr));
      });

  WorkPool.async(
      [BufferStart, RegionOffset, &ElfSections, NumSections, RegionSize]() {
        auto *const ShdrStartPtr = reinterpret_cast<Elf_Shdr *>(
            BufferStart + RegionOffset(Region::SectionTable));
        auto *const ShdrEndPtr =
            rld::elf::emitSectionHeaders<ELFT>(ShdrStartPtr, ElfSections);
        (void)ShdrEndPtr;
        assert(ShdrEndPtr - ShdrStartPtr == NumSections);
        assert(reinterpret_cast<std::uint8_t *>(ShdrStartPtr) +
                   RegionSize(Region::SectionTable) ==
               reinterpret_cast<std::uint8_t *>(ShdrEndPtr));
      });

  WorkPool.async(
      [BufferStart, RegionOffset, RegionSize, &ElfSections, SectionNameSize]() {
        // Produce the section names string table.
        auto *SectionNameStartPtr = reinterpret_cast<char *>(
            BufferStart + RegionOffset(Region::SectionNames));
        auto *SectionNameEndPtr =
            emitSectionHeaderStringTable(SectionNameStartPtr, ElfSections);
        (void)SectionNameEndPtr;
        assert(SectionNameStartPtr + SectionNameSize == SectionNameEndPtr);
        assert(reinterpret_cast<std::uint8_t *>(SectionNameStartPtr) +
                   RegionSize(Region::SectionNames) ==
               reinterpret_cast<std::uint8_t *>(SectionNameEndPtr));
      });

  copyToOutput(Ctxt, WorkPool, Layout, BufferStart,
               RegionOffset(Region::TargetData));
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

  llvm::ThreadPool WorkPool{NumWorkers};
  llvm::ErrorOr<rld::IdentifyResult> Identified =
      rld::identifyPass(Ctxt, WorkPool, CompilationIndex, InputPaths);
  if (!Identified) {
    llvm::errs() << "Error: " << Identified.getError().message() << '\n';
    std::exit(EXIT_FAILURE);
  }

  rld::UndefsContainer Undefs;
  auto GlobalSymbs =
      std::make_unique<rld::GlobalsStorage>(NumWorkers.getValue());

  std::unique_ptr<rld::LayoutOutput> LO;
  {
    llvm::NamedRegionTimer LayoutTimer("Layout", "Output file layout",
                                       rld::TimerGroupName,
                                       rld::TimerGroupDescription);

    // The plus one here allows for the linker-generated /interp/ file.
    auto const NumCompilations = InputPaths.size() + std::size_t{1};
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
    }
    LayoutThread.join();
    LO = Layout.flattenSegments();
  }

  rld::llvmDebug(DebugType, Ctxt.IOMut, [&Ctxt] {
    llvm::dbgs() << "Output triple: " << Ctxt.triple()->normalize() << '\n';
  });

  // Now we set about emitting an ELF executable...
  elfOutput(Ctxt, WorkPool, *LO);

  // Avoid calling the destructors of some of our global objects. We can simply
  // let the O/S do that instead. Remove these calls if looking for memoryleaks,
  // though!
  GlobalSymbs.release();
  LO.release();
  std::cout << "All done\n";
}
