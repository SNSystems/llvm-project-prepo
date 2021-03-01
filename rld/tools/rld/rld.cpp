//===- tools/rld/rld.cpp --------------------------------------------------===//
//*       _     _  *
//*  _ __| | __| | *
//* | '__| |/ _` | *
//* | |  | | (_| | *
//* |_|  |_|\__,_| *
//*                *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
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

#include "rld/ElfOutput.h"
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

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, rld::SectionKind SKind) {
#define X(x)                                                                   \
  case rld::SectionKind::x:                                                    \
    OS << #x;                                                                  \
    break;

  switch (SKind) {
    PSTORE_MCREPO_SECTION_KINDS
  case rld::SectionKind::shstrtab:
    OS << "shstrtab";
    break;
  case rld::SectionKind::strtab:
    OS << "strtab";
    break;
  case rld::SectionKind::symtab:
    OS << "symtab";
    break;
  case rld::SectionKind::last:
    OS << "last"; // Never used. Always last.
    break;
  }
#undef X
  return OS;
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
  using pstore::repo::definition;
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

  definition Symbol{interp_fragment_digest, FragmentDxtent, SymbolAddr,
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

using namespace rld;

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
      std::lock_guard<decltype(Ctxt.IOMut)> Lock{Ctxt.IOMut};
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

      if (Undefs.strongUndefCount() > 0U) {
        // TODO: show the user some useful output!
        std::lock_guard<decltype(Ctxt.IOMut)> Lock{Ctxt.IOMut};
        for (auto const &U : Undefs) {
          assert(!U.hasDefinition());
          if (!U.allReferencesAreWeak()) {
            pstore::shared_sstring_view Owner;
            llvm::errs() << "Undefined symbol: "
                         << stringViewAsRef(
                                loadString(Ctxt.Db, U.name(), &Owner))
                         << '\n';
          }
        }
        return EXIT_FAILURE;
      }

      Triple = Ctxt.triple();
      if (!Triple) {
        std::lock_guard<decltype(Ctxt.IOMut)> Lock{Ctxt.IOMut};
        llvm::errs() << "Error: The output triple could not be determined.\n";
        return EXIT_FAILURE;
      }
#if ADD_IDENT_SEGMENT
      auto const FixedCompilationExtent =
          generateFixedContent(Ctxt.Db, *Triple);
      if (!FixedCompilationExtent) {
        std::lock_guard<decltype(Ctxt.IOMut)> Lock{Ctxt.IOMut};
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

  rld::GlobalSymbolsContainer AllSymbols = GlobalSymbs->all();
  llvmDebug(DebugType, Ctxt.IOMut, [&] { debugDumpSymbols(Ctxt, AllSymbols); });

  // Now we set about emitting an ELF executable...
  ExitOnErr(
      rld::elfOutput(OutputFileName, Ctxt, AllSymbols, WorkPool, LO.get()));

  // Avoid calling the destructors of some of our global objects. We can simply
  // let the O/S do that instead. Remove these calls if looking for memoryleaks,
  // though!
  GlobalSymbs.release();
  LO.release();
  std::cout << "All done\n";
}
