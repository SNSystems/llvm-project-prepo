//===- lib/Driver.cpp -----------------------------------------------------===//
//*  ____       _                 *
//* |  _ \ _ __(_)_   _____ _ __  *
//* | | | | '__| \ \ / / _ \ '__| *
//* | |_| | |  | |\ V /  __/ |    *
//* |____/|_|  |_| \_/ \___|_|    *
//*                               *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/Driver.h"

#include "rld/ErrorCode.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"

using namespace llvm;

namespace {

using CompilationExtent = pstore::extent<pstore::repo::compilation>;

class FileDescriptorCloser {
public:
  explicit constexpr FileDescriptorCloser(int const FD) : FD_{FD} {}
  FileDescriptorCloser(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser(FileDescriptorCloser &&) noexcept = delete;

  ~FileDescriptorCloser() noexcept {
    sys::Process::SafelyCloseFileDescriptor(FD_);
  }

  FileDescriptorCloser &operator=(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser &operator=(FileDescriptorCloser &&) noexcept = delete;

private:
  int const FD_;
};

} // end anonymous namespace

namespace pstore {

raw_ostream &operator<<(raw_ostream &OS, pstore::uint128 Digest) {
  return OS << Digest.to_hex_string();
}

} // end namespace pstore

namespace rld {

// (ctor)
// ~~~~~~
Driver::Driver(
    pstore::database *const Db,
    const std::shared_ptr<pstore::index::compilation_index> &CompilationIndex,
    const llvm::StringRef EntryPoint, const unsigned NumWorkers,
    const llvm::StringRef OutputFileName,
    const std::function<void(Context &, llvm::StringRef,
                             const std::error_code &)>
        ReportError)
    : Db_{Db}, CompilationIndex_{CompilationIndex},
      Context_{std::make_unique<rld::Context>(*Db, EntryPoint)},
      WorkPool_{std::make_unique<llvm::ThreadPool>(
          llvm::heavyweight_hardware_concurrency(NumWorkers))},
      GlobalSymbs_{std::make_unique<rld::GlobalsStorage>(NumWorkers)},
      FixupStorage_{std::make_unique<rld::FixupStorage>(NumWorkers)},
      OutputFileName_{OutputFileName}, ReportError_{ReportError} {}

// (dtor)
// ~~~~~~
Driver::~Driver() noexcept {
  AllSymbols_.release();
  GOTPLTs.release();
  LO.release();
  FixupStorage_.release();
  GlobalSymbs_.release();
  // UndefsContainer Undefs_;
  WorkPool_.release();
  Context_.release();
  // CompilationIndex_.release();
}

// resolve compilation
// ~~~~~~~~~~~~~~~~~~~
void Driver::resolveCompilation(Scanner *const Scan, StringRef InputFilePath,
                                const pstore::index::digest CompilationDigest,
                                GroupSet *const NextGroup,
                                const uint32_t Ordinal) {
  auto Pos = CompilationIndex_->find(*Db_, CompilationDigest);
  if (Pos == CompilationIndex_->end(*Db_)) {
    return sayError(InputFilePath,
                    make_error_code(ErrorCode::CompilationNotFound));
  }
  // Record the input-ordinal/name mapping in the context.
  Context_->setOrdinalName(Ordinal, InputFilePath);
  if (!Scan->run(InputFilePath, GlobalSymbs_->getThreadStorage(),
                 FixupStorage_->getThreadStorage(), NextGroup,
                 Pos->second, // compilation extent
                 Ordinal      // input ordinal
                 )) {
    ErrorFlag_.store(true, std::memory_order_relaxed);
  }
}

// open
// ~~~~
llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
Driver::open(const llvm::StringRef Path) {
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

// find file
// ~~~~~~~~~
// Find a file by concatenating given paths. If a resulting path
// starts with "=", the character is replaced with a --sysroot value.
Optional<std::string> Driver::findFile(StringRef path1, const Twine &path2) {
  SmallString<128> S;
  // if (path1.startswith("=")) {
  //   path::append(s, SysRoot, path1.substr(1), path2);
  // } else {
  sys::path::append(S, path1, path2);
  //}
  return sys::fs::exists(S) ? Optional<std::string>{std::string{S}}
                            : Optional<std::string>{None};
}

// get output triple
// ~~~~~~~~~~~~~~~~~
// The triple will be used to determine the output architecture and file format.
// ATM, these are both just hard-wired.
bool Driver::getOutputTriple() {
  llvm::Optional<llvm::Triple> Triple = Context_->triple();
  if (!Triple) {
    ReportError_(*Context_, "The output triple could not be determined.",
                 ErrorCode::UndeterminedOutputTriple);
    ErrorFlag_.store(true);
    return false;
  }

  llvmDebug(DebugType_, Context_->IOMut, [this] {
    llvm::dbgs() << "Output triple: " << Context_->triple()->normalize()
                 << '\n';
  });
  return true;
}

// debug group members
// ~~~~~~~~~~~~~~~~~~~
void Driver::debugGroupMembers(unsigned GroupNum,
                               CompilationGroup *const Group) {
  llvm::dbgs() << "Group " << GroupNum << '\n';
  for (const CompilationGroup::value_type &CR : *Group) {
    llvm::dbgs() << "    " << std::get<pstore::index::digest>(CR) << ' '
                 << *CR.second << '\n';
  }
  llvm::dbgs() << '\n';
}

// report undefined symbols
// ~~~~~~~~~~~~~~~~~~~~~~~~
bool Driver::reportUndefinedSymbols() {
  assert(Undefs_.strongUndefCountIsCorrect() &&
         "The strong undef count must match the entries in the undefs list");
  if (Undefs_.strongUndefCount() > 0U) {
    for (const Symbol &U : Undefs_) {
      assert(!U.isDefinition());
      if (!U.allReferencesAreWeak()) {
        const auto ContentsAndLock = U.contentsAndLock();
        const auto &Contents =
            std::get<const Symbol::Contents &>(ContentsAndLock);
        assert(holdsAlternative<Symbol::Reference>(Contents));
        std::string OutString;
        raw_string_ostream OS{OutString};
        OS << '\'' << loadStdString(Context_->Db, U.name())
           << "' referenced from \""
           << Context_->ordinalName(
                  get<Symbol::Reference>(Contents).InputOrdinal)
           << '"';
        ReportError_(*Context_, OutString, ErrorCode::UndefinedSymbol);
      }
    }
    ErrorFlag_.store(true);
    return false;
  }
  return true;
}

// run impl
// ~~~~~~~~
bool Driver::runImpl(CompilationGroup *const Group, GroupSet *const NextGroup) {
  LayoutBuilder Layout{*Context_, &Undefs_};

  {
    llvm::NamedRegionTimer LayoutTimer{
        "Layout", "Output file layout", rld::TimerGroupName,
        rld::TimerGroupDescription, Context_->TimersEnabled};
    std::thread LayoutThread{&rld::LayoutBuilder::run, &Layout};
    rld::Scanner Scan{*Context_, Layout, &Undefs_};

    auto GroupNum = 0U;
    do {
      llvmDebug(DebugType_, Context_->IOMut,
                [&] { Driver::debugGroupMembers(GroupNum, Group); });
      for (CompilationGroup::value_type const &Entry : *Group) {
        WorkPool_->async(
            [&](CompilationGroup::value_type const &C, const uint32_t Ordinal) {
              auto const &Digest = std::get<pstore::index::digest>(C);
              auto const &Origin = std::get<std::shared_ptr<std::string>>(C);
              this->resolveCompilation(&Scan, *Origin, Digest, NextGroup,
                                       Ordinal);
            },
            Entry, InputOrdinal_);
        ++InputOrdinal_;
      }
      WorkPool_->wait();
      NextGroup->transferTo(*Context_, Group);
      ++GroupNum;
    } while (!Group->empty() && Undefs_.strongUndefCount() > 0U);

    Layout.endGroup();
    if (ErrorFlag_.load()) {
      Layout.error();
    }
    LayoutThread.join();
    if (!ErrorFlag_.load()) {
      this->reportUndefinedSymbols();
    }
    if (!ErrorFlag_.load()) {
      this->getOutputTriple();
    }
  }

  if (ErrorFlag_.load()) {
    return false;
  }

  std::tie(LO, GOTPLTs) = Layout.flattenSegments(
      Context_->baseAddress(),
      Layout.elfHeaderBlockSize<llvm::object::ELF64LE>());

  // Get the lists of local and global symbols from layout.
  SymbolOrder SymOrder = Layout.symbolOrder();

  AllSymbols_ =
      std::make_unique<rld::GlobalSymbolsContainer>(GlobalSymbs_->all());
  llvmDebug(DebugType_, Context_->IOMut,
            [&] { debugDumpSymbols(*Context_, *AllSymbols_); });

  // Now we set about emitting an ELF executable...
  rld::llvmDebug(DebugType_, Context_->IOMut,
                 [] { llvm::dbgs() << "Beginning output\n"; });

  llvm::Error Err = elfOutput<llvm::object::ELF64LE>(
      OutputFileName_, *Context_, *AllSymbols_, SymOrder, Undefs_, *WorkPool_,
      LO.get(), *GOTPLTs);
  if (Err) {
    ErrorFlag_.store(true);
    handleAllErrors(std::move(Err), [&](const ErrorInfoBase &EI) {
      std::string Str;
      raw_string_ostream OS{Str};
      EI.log(OS);
      ReportError_(*Context_, "Output error: ", EI.convertToErrorCode());
    });
    return false;
  }

  WorkPool_->wait();

  return !ErrorFlag_.load();
}

} // end namespace rld
