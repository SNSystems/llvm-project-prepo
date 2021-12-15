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

#include "rld/Archive.h"
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
llvm::cl::list<std::string> InputFiles(llvm::cl::Positional,
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

llvm::cl::opt<bool> TimersEnabled(
    "enable-timers", llvm::cl::Hidden,
    llvm::cl::desc("Time each job, printing elapsed time for each on exit"));

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
  case rld::SectionKind::fini_array:
    OS << "fini_array";
    break;
  case rld::SectionKind::got:
    OS << "got";
    break;
  case rld::SectionKind::gotplt:
    OS << "gotplt";
    break;
  case rld::SectionKind::init_array:
    OS << "init_array";
    break;
  case rld::SectionKind::plt:
    OS << "plt";
    break;
  case rld::SectionKind::rela_plt:
    OS << "rela_plt";
    break;
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

static llvm::ErrorOr<std::unique_ptr<pstore::database>> openRepository() {
  std::string const FilePath = getRepoPath();
  if (!llvm::sys::fs::exists(FilePath)) {
    return rld::ErrorCode::DatabaseNotFound;
  }
  return std::make_unique<pstore::database>(
      FilePath, pstore::database::access_mode::writable);
}

class FileDescriptorCloser {
public:
  explicit constexpr FileDescriptorCloser(int const FD) : FD_{FD} {}

  // No move or copy.
  FileDescriptorCloser(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser(FileDescriptorCloser &&) noexcept = delete;
  FileDescriptorCloser &operator=(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser &operator=(FileDescriptorCloser &&) noexcept = delete;

  ~FileDescriptorCloser() noexcept { ::close(FD_); }

private:
  int const FD_;
};

// open
// ~~~~
static llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
open(const llvm::StringRef Path) {
  auto FD = 0;
  if (const std::error_code Erc =
          llvm::sys::fs::openFileForRead(Path, FD /*out!*/)) {
    return Erc;
  }
  const FileDescriptorCloser FDCloser{FD};
  llvm::sys::fs::file_status Status;
  if (const std::error_code Erc = llvm::sys::fs::status(FD, Status /*out!*/)) {
    return Erc;
  }

  return llvm::MemoryBuffer::getOpenFile(
      llvm::sys::fs::convertFDToNativeFile(FD), Path, Status.getSize());
}

static bool reportError(const rld::Context &Context,
                        const llvm::Twine &InputFilePath,
                        const std::error_code Error) {
  const std::lock_guard<std::mutex> _{Context.IOMut};
  llvm::errs() << InputFilePath << ": Error: " << Error.message() << '\n';
  return true;
}

using CompilationExtent = pstore::extent<pstore::repo::compilation>;

using namespace rld;

int main(int Argc, char *Argv[]) {
  llvm::cl::ParseCommandLineOptions(Argc, Argv);

  ExitOnErr.setBanner(std::string{Argv[0]} + ": error: ");

  llvm::ErrorOr<std::unique_ptr<pstore::database>> Db = openRepository();
  if (!Db) {
    llvm::errs() << "Error: " << Db.getError().message() << '\n';
    return EXIT_FAILURE;
  }

  auto Ctxt = std::make_unique<rld::Context>(*Db.get(), EntryPoint);
  Ctxt->TimersEnabled = TimersEnabled;

  auto CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Ctxt->Db);
  if (!CompilationIndex) {
    llvm::errs()
        << "Error: "
        << make_error_code(rld::ErrorCode::CompilationIndexNotFound).message()
        << '\n';
    return EXIT_FAILURE;
  }

  auto WorkPool = std::make_unique<llvm::ThreadPool>(
      llvm::heavyweight_hardware_concurrency(NumWorkers));


  rld::UndefsContainer Undefs;
  auto GlobalSymbs =
      std::make_unique<rld::GlobalsStorage>(NumWorkers.getValue());
  auto FixupStorage =
      std::make_unique<rld::FixupStorage>(NumWorkers.getValue());
  std::unique_ptr<rld::Layout> LO;
  std::unique_ptr<rld::GOTPLTContainer> GOTPLTs;
  SymbolOrder SymOrder;

  std::atomic<bool> ErrorFlag{false};

  auto sayError = [&Ctxt, &ErrorFlag](llvm::StringRef FilePath,
                                      std::error_code const &Err) {
    reportError(*Ctxt, FilePath, Err);
    ErrorFlag.store(true, std::memory_order_relaxed);
  };

  rld::LayoutBuilder Layout{*Ctxt, &Undefs};

  {
    std::thread LayoutThread{&rld::LayoutBuilder::run, &Layout};
    llvm::NamedRegionTimer LayoutTimer{
        "Layout", "Output file layout", rld::TimerGroupName,
        rld::TimerGroupDescription, Ctxt->TimersEnabled};

    rld::Scanner Scan{*Ctxt, Layout, &Undefs};
    uint32_t InputOrdinal = 0;
    uint32_t ArchiveCount = 0;

    for (std::string const &InputFilePath : InputFiles) {
      llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBuffer =
          open(InputFilePath);
      if (!FileBuffer) {
        sayError(InputFilePath, FileBuffer.getError());
        continue;
      }
      const llvm::ErrorOr<FileKind> Kind = getFileKind(**FileBuffer);
      if (!Kind) {
        sayError(InputFilePath, FileBuffer.getError());
        continue;
      }

      switch (*Kind) {
      case FileKind::Ticket: {
        llvm::ErrorOr<pstore::index::digest> CompilationDigestOrError =
            llvm::mc::repo::getDigestFromTicket(**FileBuffer, &Ctxt->Db);
        if (!CompilationDigestOrError) {
          sayError(InputFilePath, CompilationDigestOrError.getError());
          break;
        }
        auto Pos = CompilationIndex->find(Ctxt->Db, *CompilationDigestOrError);
        if (Pos == CompilationIndex->end(Ctxt->Db)) {
          sayError(InputFilePath,
                   make_error_code(rld::ErrorCode::CompilationNotFound));
          break;
        }
        Ctxt->setOrdinalName(InputOrdinal,
                             InputFilePath); // Record the input-ordinal/name
                                             // mapping in the context.
        WorkPool->async(
            [&Scan, &GlobalSymbs, &FixupStorage, &ErrorFlag](
                const llvm::StringRef P, const CompilationExtent Compilation,
                const uint32_t Ordinal) {
              if (!Scan.run(P, // file path
                            GlobalSymbs->getThreadStorage(),
                            FixupStorage->getThreadStorage(),
                            Compilation, // compilation extent
                            Ordinal      // input ordinal
                            )) {
                ErrorFlag.store(true, std::memory_order_relaxed);
              }
            },
            llvm::StringRef{InputFilePath}, Pos->second, InputOrdinal);
        ++InputOrdinal;
      } break;
      case FileKind::Archive:
        // The arguments to async() are passed to std::bind() whose arguments
        // must be copyable. This means that we can't pass the FileBuffer
        // unique_ptr<> but instead must wrap the underlying memory in a
        // shared)_ptr<>.
        WorkPool->async(
            iterateArchiveMembers, &ErrorFlag, std::ref(*Ctxt), WorkPool.get(),
            llvm::StringRef{InputFilePath}, ArchiveCount,
            std::shared_ptr<llvm::MemoryBuffer>{FileBuffer->release()},
            CompilationIndex);
        ++ArchiveCount;
        break;

      case FileKind::Unknown:
        ErrorFlag = reportError(
            *Ctxt, InputFilePath,
            make_error_code(llvm::object::object_error::invalid_file_type));
        continue;
      }
    }

    WorkPool->wait();
    Layout.endGroup();
    assert(Undefs.strongUndefCountIsCorrect() &&
           "The strong undef count must match the entries in the undefs list");
    if (Undefs.strongUndefCount() > 0U) {
      std::lock_guard<decltype(Ctxt->IOMut)> Lock{Ctxt->IOMut};
      for (auto const &U : Undefs) {
        assert(!U.hasDefinition());
        if (!U.allReferencesAreWeak()) {
          // TODO: also need to show where the reference is made.
          llvm::errs() << "Error: Undefined symbol '"
                       << loadStdString(Ctxt->Db, U.name()) << "'\n";
        }
      }
      ErrorFlag = true;
    }

    llvm::Optional<llvm::Triple> Triple;
    if (!ErrorFlag) {
      Triple = Ctxt->triple();
      if (!Triple) {
        std::lock_guard<decltype(Ctxt->IOMut)> Lock{Ctxt->IOMut};
        llvm::errs() << "Error: The output triple could not be determined.\n";
        ErrorFlag = true;
      }
    }
    rld::llvmDebug(DebugType, Ctxt->IOMut, [&Ctxt] {
      llvm::dbgs() << "Output triple: " << Ctxt->triple()->normalize() << '\n';
    });

    if (ErrorFlag) {
      Layout.error();
    }
    LayoutThread.join();
    if (ErrorFlag) {
      return EXIT_FAILURE;
    }
  }

    std::tie(LO, GOTPLTs) = Layout.flattenSegments(
        Ctxt->baseAddress(),
        Layout.elfHeaderBlockSize<llvm::object::ELF64LE>());

    // Get the lists of local and global symbols from layout.
    SymOrder = Layout.symbolOrder();

  auto AllSymbols =
      std::make_unique<rld::GlobalSymbolsContainer>(GlobalSymbs->all());
  llvmDebug(DebugType, Ctxt->IOMut,
            [&] { debugDumpSymbols(*Ctxt, *AllSymbols); });

  // Now we set about emitting an ELF executable...
  rld::llvmDebug(DebugType, Ctxt->IOMut,
                 [] { llvm::dbgs() << "Beginning output\n"; });
  ExitOnErr(rld::elfOutput<llvm::object::ELF64LE>(
      OutputFileName, *Ctxt, *AllSymbols, SymOrder, Undefs, *WorkPool, LO.get(),
      *GOTPLTs));
  WorkPool->wait();

  // Avoid calling the destructors of some of our global objects. We can simply
  // let the O/S do that instead. Remove these calls if looking for memory
  // leaks, though!
  GOTPLTs.release();
  AllSymbols.release();
  GlobalSymbs.release();
  LO.release();
  FixupStorage.release();
  WorkPool.release();
  Ctxt.release();
  (*Db).release();
  return EXIT_SUCCESS;
}
