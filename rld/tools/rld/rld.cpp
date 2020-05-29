//*       _     _  *
//*  _ __| | __| | *
//* | '__| |/ _` | *
//* | |  | | (_| | *
//* |_|  |_|\__,_| *
//*                *
//===- tools/rld/rld.cpp --------------------------------------------------===//
// Copyright (c) 2017-2018 by Sony Interactive Entertainment, Inc.
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
    llvm::cl::init(std::max(std::thread::hardware_concurrency(), 1U)));

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


template <typename T> class as_hex {
public:
  explicit as_hex(T t) : t_{t} {}
  T value() const noexcept { return t_; }

private:
  T t_;
};

template <typename T> as_hex<T> to_hex(T t) { return as_hex<T>{t}; }

template <typename T>
llvm::raw_ostream &operator<<(llvm::raw_ostream &os, as_hex<T> const &v) {
  os << "0x";
  return os.write_hex(v.value());
}

template <pstore::repo::section_kind SKind,
          typename SType = typename pstore::repo::enum_to_section<SKind>::type>
void copy_section(rld::Context &Ctxt, rld::SectionInfo const &S,
                  std::uint8_t *Dest) {
  auto *const Section = reinterpret_cast<SType const *>(S.Section);

  rld::llvmDebug(DebugType, Ctxt.IOMut, [Dest]() {
    llvm::dbgs() << "copy to " << to_hex(reinterpret_cast<std::uintptr_t>(Dest))
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
                  std::uint64_t const StartOffset,
                  std::uint64_t const TotalSize) {
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

  WorkPool.wait();
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
void for_each_segment(rld::LayoutOutput const &LO, Function f) {
  const auto NumSegments = LO.size();
  for (auto Seg = std::size_t{0}; Seg < NumSegments; ++Seg) {
    auto const Kind = static_cast<rld::SegmentKind>(Seg);
    if (Kind != rld::SegmentKind::discard) {
      f(Kind, LO[Seg]);
    }
  }
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
    return make_error_code(rld::ErrorCode::cannot_load_the_compilation_index);
  }

  auto fragments =
      pstore::index::get_index<pstore::trailer::indices::fragment>(Db);
  if (fragments == nullptr) {
    return make_error_code(rld::ErrorCode::cannot_load_the_fragment_index);
  }

  auto names = pstore::index::get_index<pstore::trailer::indices::name>(Db);
  if (Compilations == nullptr) {
    return make_error_code(rld::ErrorCode::cannot_load_the_names_index);
  }

  auto const InterpFragmentPos = fragments->find(Db, interp_fragment_digest);
  auto const InterpCompilationPos = Compilations->find(Db, interp_compilation);
  auto const HaveInterpFragment = InterpFragmentPos != fragments->end(Db);
  auto const HaveInterpCompilation =
      InterpCompilationPos != Compilations->end(Db);
  if (HaveInterpFragment && HaveInterpCompilation) {
    return InterpCompilationPos->second;
  }

  auto Transaction = pstore::begin(Db);
  auto const FragmentExtent = HaveInterpFragment
                                  ? InterpFragmentPos->second
                                  : makeInterpFragment(Transaction, fragments);
  pstore::extent<pstore::repo::compilation> const CompilationExtent =
      HaveInterpCompilation
          ? InterpCompilationPos->second
          : makeInterpCompilation(Transaction, Triple, Compilations, names,
                                  FragmentExtent);
  Transaction.commit();

  return CompilationExtent;
}

enum class RldErrorCode : int {
  none,
  DatabaseNotFound,
  CompilationIndexNotFound,
};

class RldErrorCategory : public std::error_category {
public:
  // The need for this constructor was removed by CWG defect 253 but Clang
  // (prior to 3.9.0) and GCC (before 4.6.4) require its presence.
  RldErrorCategory() noexcept {} // NOLINT
  char const *name() const noexcept override;
  std::string message(int error) const override;
};

char const *RldErrorCategory::name() const noexcept { return "rld category"; }

std::string RldErrorCategory::message(int const error) const {
  auto *result = "unknown error";
  switch (static_cast<RldErrorCode>(error)) {
  case RldErrorCode::none:
    result = "";
    break;
  case RldErrorCode::DatabaseNotFound:
    result = "Repository was not found";
    break;
  case RldErrorCode::CompilationIndexNotFound:
    result = "Compilation index was not found";
    break;
  }
  return result;
}

RldErrorCategory const &getRldErrorCategory() noexcept {
  static RldErrorCategory const cat;
  return cat;
}

std::error_code make_error_code(RldErrorCode const e) {
  static_assert(
      std::is_same<std::underlying_type<decltype(e)>::type, int>::value,
      "base type of error_code must be int to permit safe static cast");
  return {static_cast<int>(e), getRldErrorCategory()};
}

static llvm::ErrorOr<std::unique_ptr<pstore::database>> openRepository() {
  std::string const FilePath = getRepoPath();
  if (!llvm::sys::fs::exists(FilePath)) {
    return make_error_code(RldErrorCode::DatabaseNotFound);
  }
  return llvm::make_unique<pstore::database>(
      FilePath, pstore::database::access_mode::writable);
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
        << make_error_code(RldErrorCode::CompilationIndexNotFound).message()
        << '\n';
    return EXIT_FAILURE;
  }

  const unsigned NumWorkerThreads = NumWorkers;

  llvm::ThreadPool WorkPool{NumWorkerThreads};
  llvm::ErrorOr<rld::IdentifyResult> R = rld::identifyPass(
      Ctxt, WorkPool, CompilationIndex, InputPaths, NumWorkerThreads);
  if (!R) {
    llvm::errs() << "Error: " << R.getError().message() << '\n';
    std::exit(EXIT_FAILURE);
  }

  rld::UndefsContainer Undefs;
  auto GlobalSymbs = std::make_unique<rld::GlobalsStorage>(NumWorkerThreads);

  // The plus one here allows for the linker-generated /interp/ file.
  auto const NumCompilations = InputPaths.size() + std::size_t{1};
  rld::LayoutBuilder Layout{Ctxt, GlobalSymbs.get(), NumCompilations};
  std::thread LayoutThread{&rld::LayoutBuilder::run, &Layout};

  llvm::Optional<llvm::Triple> Triple;

  {
    rld::Scanner Scan{Ctxt, Layout, Undefs};
    for (const auto &C : R->Compilations) {
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
    auto const FixedCompilationExtent = generateFixedContent(Ctxt.Db, *Triple);
    if (!FixedCompilationExtent) {
      llvm::errs() << "Error: " << FixedCompilationExtent.getError().message()
                   << '\n';
      return EXIT_FAILURE;
    }
    Scan.run("/ident/", // path
             GlobalSymbs->getThreadSymbols(),
             *FixedCompilationExtent, // compilation extent
             R->Compilations.size()   // input ordinal
    );
  }
  LayoutThread.join();
  std::unique_ptr<rld::LayoutOutput> LO = Layout.flattenSegments();

  rld::llvmDebug(DebugType, Ctxt.IOMut, [&Triple] {
    llvm::dbgs() << "Output triple: " << Triple->normalize() << '\n';
  });

  // Now we set about emitting an ELF executable...

  using ELF64LE = llvm::object::ELFType<llvm::support::little, true>;
  using Elf_Ehdr = llvm::object::ELFFile<ELF64LE>::Elf_Ehdr;
  using Elf_Shdr = llvm::object::ELFFile<ELF64LE>::Elf_Shdr;
  using Elf_Phdr = llvm::object::ELFFile<ELF64LE>::Elf_Phdr;

  std::array<rld::elf::ElfSectionInfo,
             static_cast<size_t>(rld::SectionKind::last)>
      ElfSections;
  static constexpr auto NumImplicitSections = 1U; // The null section.

  ElfSections[static_cast<size_t>(rld::SectionKind::shstrtab)].Emit = true;
  ElfSections[static_cast<size_t>(rld::SectionKind::shstrtab)].Align = 1;

  for_each_section(
      *LO, [&ElfSections](rld::SectionKind Kind, rld::SectionInfo const &SI) {
        if (SI.Size > 0) {
          auto &ESI = ElfSections[static_cast<size_t>(Kind)];
          ESI.Emit = true;
          ESI.Offset = std::min(ESI.Offset, SI.Offset);
          ESI.Size += SI.Size;
          ESI.Align = std::max(ESI.Align, SI.Align);
        }
      });

  auto NumSections = NumImplicitSections;
  auto SectionNameSize = std::uint64_t{1};
  auto SectionHeaderNamesSectionIndex = 0;
  for (rld::SectionKind Kind = rld::firstSectionKind();
       Kind != rld::SectionKind::last; ++Kind) {
    auto &ESI = ElfSections[static_cast<size_t>(Kind)];
    if (ESI.Emit) {
      if (Kind == rld::SectionKind::shstrtab) {
        SectionHeaderNamesSectionIndex = NumSections;
      }
      ++NumSections;

      assert(ESI.NameOffset == 0);
      ESI.NameOffset = SectionNameSize;
      SectionNameSize += rld::elf::elfSectionNameAndLength(Kind).second + 1U;
    }
  }

  auto TotalNumSegments = std::uint64_t{0};
  for_each_segment(*LO, [&TotalNumSegments](rld::SegmentKind /*Kind*/,
                                            rld::Segment const &Segment) {
    if (Segment.shouldEmit()) {
      ++TotalNumSegments;
    }
  });

  assert(NumSections == std::count_if(std::begin(ElfSections),
                                      std::end(ElfSections),
                                      [](rld::elf::ElfSectionInfo const &ESI) {
                                        return ESI.Emit;
                                      }) +
                            NumImplicitSections);

  enum ElfFileRegions {
    FileHeader,
    SegmentTable,
    SectionHeaderTable,
    SectionNames,
    TargetData
  };
  using RegionTuple = std::tuple<uint64_t, uint64_t>;
  enum LocationIndexes {
    OffsetIndex,
    SizeIndex
  }; // Meaning of the two fields in RegionTuple.
  auto RegionEnd = [](RegionTuple const &Loc) {
    return std::get<OffsetIndex>(Loc) + std::get<SizeIndex>(Loc);
  };

  uint64_t const TargetDataSize = std::accumulate(
      std::begin(*LO), std::end(*LO), uint64_t{0},
      [](uint64_t RunningTotal, rld::Segment const &Segment) {
        return rld::alignTo(RunningTotal, Segment.MaxAlign) + Segment.VSize;
      });

  auto const FileHeaderLocation = RegionTuple{0, sizeof(Elf_Ehdr)};
  auto const SegmentsLocation = RegionTuple{
      RegionEnd(FileHeaderLocation), TotalNumSegments * sizeof(Elf_Phdr)};
  auto const TargetDataLocation =
      RegionTuple{RegionEnd(SegmentsLocation), TargetDataSize};
  auto const SectionHeadersLocation = RegionTuple{
      RegionEnd(TargetDataLocation), NumSections * sizeof(Elf_Shdr)};
  auto const SectionNamesLocation =
      RegionTuple{RegionEnd(SectionHeadersLocation), SectionNameSize};

  ElfSections[static_cast<size_t>(rld::SectionKind::shstrtab)].Offset =
      std::get<OffsetIndex>(SectionNamesLocation);
  ElfSections[static_cast<size_t>(rld::SectionKind::shstrtab)].Size =
      std::get<SizeIndex>(SectionNamesLocation);

  uint64_t const TotalSize = RegionEnd(SectionNamesLocation);

  rld::llvmDebug(DebugType, Ctxt.IOMut, [TotalSize]() {
    llvm::dbgs() << "Output file size=" << TotalSize << '\n';
  });

  auto Out = ExitOnErr(llvm::FileOutputBuffer::create(
      OutputFileName, TotalSize,
      llvm::FileOutputBuffer::
          F_executable /*| llvm::FileOutputBuffer::F_modify*/));

  std::uint8_t *const BufferStart = Out->getBufferStart();
  auto *const Ehdr = reinterpret_cast<Elf_Ehdr *>(BufferStart);
  initELFHeader<ELF64LE>(Ehdr, llvm::ELF::EM_X86_64);
  Ehdr->e_type = llvm::ELF::ET_EXEC;
  Ehdr->e_phnum = TotalNumSegments;
  Ehdr->e_phoff = std::get<OffsetIndex>(SegmentsLocation);
  Ehdr->e_shnum = NumSections;
  Ehdr->e_shoff = std::get<OffsetIndex>(SectionHeadersLocation);
  Ehdr->e_shstrndx = SectionHeaderNamesSectionIndex;

  Elf_Phdr *const ElfPhdrEndPtr = rld::elf::emitProgramHeaders<ELF64LE>(
      reinterpret_cast<Elf_Phdr *>(BufferStart +
                                   std::get<OffsetIndex>(SegmentsLocation)),
      std::get<OffsetIndex>(TargetDataLocation), *LO);
  (void)ElfPhdrEndPtr;
  copyToOutput(Ctxt, WorkPool, *LO, BufferStart,
               std::get<OffsetIndex>(TargetDataLocation), TotalSize);

  auto *const ElfShdrStartPtr = reinterpret_cast<Elf_Shdr *>(
      BufferStart + std::get<OffsetIndex>(SectionHeadersLocation));
  Elf_Shdr *const ElfShdrEndPtr =
      rld::elf::emitSectionHeaders<ELF64LE>(ElfShdrStartPtr, ElfSections);
  (void)ElfShdrEndPtr;
  assert(ElfShdrEndPtr - ElfShdrStartPtr == NumSections);

  // Produce the section names string table.
  char *SectionNameStartPtr = reinterpret_cast<char *>(
      BufferStart + std::get<OffsetIndex>(SectionNamesLocation));
  char *SectionNameEndPtr =
      emitSectionHeaderStringTable(SectionNameStartPtr, ElfSections);
  (void)SectionNameEndPtr;
  assert(SectionNameStartPtr + SectionNameSize == SectionNameEndPtr);

  //  assert(reinterpret_cast<uint8_t *>(SectionNameEndPtr) >= BufferStart);
  //  assert(static_cast<uint64_t>(reinterpret_cast<uint8_t
  //  *>(SectionNameEndPtr) - BufferStart) ==
  //  std::get<OffsetIndex>(TargetDataLocation)); copyToOutput(Ctxt, WorkPool,
  //  *LO, BufferStart,
  //               std::get<OffsetIndex>(TargetDataLocation), TotalSize);
  ExitOnErr(Out->commit());

  // Avoid calling the destructors of some of our global objects. We can simply
  // let the O/S do that instead. Remove these calls if looking for memoryleaks,
  // though!
  GlobalSymbs.release();
  LO.release();
  std::cout << "All done\n";
}
