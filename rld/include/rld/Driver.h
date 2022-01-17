//===- include/rld/Driver.h -------------------------------*- mode: C++ -*-===//
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
#ifndef RLD_DRIVER_H
#define RLD_DRIVER_H

#include "rld/Archive.h"
#include "rld/ELFOutput.h"
#include "rld/GroupSet.h"
#include "rld/LayoutBuilder.h"
#include "rld/Scanner.h"
#include "rld/Symbol.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/Timer.h"

namespace rld {

class FixupStorage;
class Layout;

class Driver {
public:
  using ErrorHandler =
      std::function<void(Context &, llvm::StringRef, const std::error_code &)>;

  Driver(
      pstore::database *const Db,
      const std::shared_ptr<pstore::index::compilation_index> &CompilationIndex,
      const llvm::StringRef EntryPoint, const unsigned NumWorkers,
      const llvm::StringRef OutputFileName, const ErrorHandler ReportError);

  Driver(const Driver &) = delete;
  Driver(Driver &&) noexcept = delete;

  ~Driver() noexcept;

  Driver &operator=(const Driver &) = delete;
  Driver &operator=(Driver &&) noexcept = delete;

  template <typename InputFileIterator, typename LibraryIterator,
            typename LibraryPathIterator>
  bool
  run(const std::pair<InputFileIterator, InputFileIterator> &InputFiles,
      const std::pair<LibraryIterator, LibraryIterator> &Libraries,
      const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths);

private:
  void resolveCompilation(Scanner *const Scan, llvm::StringRef InputFilePath,
                          const pstore::index::digest CompilationDigest,
                          GroupSet *const NextGroup, uint32_t Ordinal);

  template <typename GroupContainer, typename InputFileIterator,
            typename LibraryIterator, typename LibraryPathIterator>
  void scheduleGroup0(
      GroupContainer *const Group, GroupSet *const NextGroup,
      const std::pair<InputFileIterator, InputFileIterator> &InputFiles,
      const std::pair<LibraryIterator, LibraryIterator> &Libraries,
      const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths);

  template <typename GroupContainer>
  uint32_t addFile(llvm::StringRef InputFilePath, GroupContainer *const Group,
                   GroupSet *const NextGroup, uint32_t ArchiveCount);

  void doIterateArchiveMembers(std::string const &InputFilePath,
                               std::shared_ptr<llvm::MemoryBuffer> MB,
                               uint32_t ArchiveCount,
                               GroupSet *const NextGroup);

  template <typename LibraryPathIterator>
  void addLibrary(
      llvm::StringRef Library, unsigned ArchiveCount, GroupSet *const NextGroup,
      const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths);

  template <typename LibraryPathIterator>
  llvm::Optional<std::string> searchLibraryBaseName(
      llvm::StringRef Name,
      const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths,
      bool IsStatic);
  static llvm::Optional<std::string> findFile(llvm::StringRef path1,
                                              const llvm::Twine &path2);

  void sayError(llvm::StringRef FilePath, const std::error_code &Err) {
    ReportError_(*Context_, FilePath, Err);
    ErrorFlag_.store(true, std::memory_order_relaxed);
  }

  static llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  open(const llvm::StringRef Path);

  bool runImpl(CompilationGroup *const Group, GroupSet *const NextGroup);
  bool getOutputTriple();
  static void debugGroupMembers(unsigned GroupNum,
                                CompilationGroup *const Group);
  bool reportUndefinedSymbols();

  pstore::database *const Db_;
  std::shared_ptr<pstore::index::compilation_index> CompilationIndex_;
  std::unique_ptr<Context> Context_;
  std::unique_ptr<llvm::ThreadPool> WorkPool_;
  UndefsContainer Undefs_;
  std::unique_ptr<GlobalsStorage> GlobalSymbs_;
  std::unique_ptr<FixupStorage> FixupStorage_;
  std::unique_ptr<rld::Layout> LO;
  std::unique_ptr<rld::GOTPLTContainer> GOTPLTs;
  const llvm::StringRef OutputFileName_;
  ErrorHandler ReportError_;

  std::atomic<bool> ErrorFlag_{false};
  uint32_t InputOrdinal_ = 0;

  static constexpr auto DebugType_ = "rld-Driver";
};

template <typename InputFileIterator, typename LibraryIterator,
          typename LibraryPathIterator>
bool Driver::run(
    const std::pair<InputFileIterator, InputFileIterator> &InputFiles,
    const std::pair<LibraryIterator, LibraryIterator> &Libraries,
    const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths) {
  CompilationGroup Group;
  GroupSet NextGroup;
  this->scheduleGroup0(&Group, &NextGroup, InputFiles, Libraries, LibraryPaths);
  return this->runImpl(&Group, &NextGroup);
}

// schedule group 0
// ~~~~~~~~~~~~~~~~
template <typename GroupContainer, typename InputFileIterator,
          typename LibraryIterator, typename LibraryPathIterator>
void Driver::scheduleGroup0(
    GroupContainer *const Group, GroupSet *const NextGroup,
    const std::pair<InputFileIterator, InputFileIterator> &InputFiles,
    const std::pair<LibraryIterator, LibraryIterator> &Libraries,
    const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths) {
  auto ArchiveCount = uint32_t{1};
  std::for_each(
      InputFiles.first, InputFiles.second, [&](llvm::StringRef InputFilePath) {
        ArchiveCount =
            this->addFile(InputFilePath, Group, NextGroup, ArchiveCount);
      });
  std::for_each(
      Libraries.first, Libraries.second, [&](llvm::StringRef Library) {
        this->addLibrary(Library, ArchiveCount++, NextGroup, LibraryPaths);
      });
}

template <typename GroupContainer>
uint32_t Driver::addFile(llvm::StringRef InputFilePath,
                         GroupContainer *const Group, GroupSet *const NextGroup,
                         uint32_t ArchiveCount) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBuffer =
      open(InputFilePath);
  if (!FileBuffer) {
    this->sayError(InputFilePath, FileBuffer.getError());
    return ArchiveCount;
  }

  const llvm::ErrorOr<FileKind> Kind = getFileKind(**FileBuffer);
  if (!Kind) {
    this->sayError(InputFilePath, Kind.getError());
    return ArchiveCount;
  }
  switch (*Kind) {
  case FileKind::Ticket:
    if (const llvm::ErrorOr<pstore::index::digest> DigestOrError =
            llvm::mc::repo::getDigestFromTicket(**FileBuffer, &Context_->Db)) {
      // Add this compilation to the initial group to be resolved.
      Group->emplace_back(*DigestOrError,
                          std::make_shared<std::string>(InputFilePath));
    } else {
      this->sayError(InputFilePath, DigestOrError.getError());
      return ArchiveCount;
    }
    break;
  case FileKind::Archive:
    this->doIterateArchiveMembers(std::string{InputFilePath},
                                  std::move(*FileBuffer), ArchiveCount,
                                  NextGroup);
    ++ArchiveCount;
    break;
  case FileKind::Unknown:
    this->sayError(
        InputFilePath,
        make_error_code(llvm::object::object_error::invalid_file_type));
    return ArchiveCount;
  }
  return ArchiveCount;
}

template <typename LibraryPathIterator>
void Driver::addLibrary(
    llvm::StringRef Library, unsigned ArchiveCount, GroupSet *const NextGroup,
    const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths) {
  const llvm::Optional<std::string> LibPath =
      searchLibraryBaseName(Library, LibraryPaths, true /*IsStatic*/);
  if (!LibPath) {
    // FIXME: Couldn't find the library. Is there a better error code?
    return this->sayError(
        Library, make_error_code(std::errc::no_such_file_or_directory));
  }
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBuffer =
      open(*LibPath);
  if (!FileBuffer) {
    return this->sayError(*LibPath, FileBuffer.getError());
  }
  const llvm::ErrorOr<FileKind> Kind = getFileKind(**FileBuffer);
  if (!Kind) {
    return this->sayError(*LibPath, Kind.getError());
  }
  if (*Kind != FileKind::Archive) {
    // FIXME: Is there a better error?
    return this->sayError(
        *LibPath,
        make_error_code(llvm::object::object_error::invalid_file_type));
  }
  this->doIterateArchiveMembers(*LibPath, std::move(*FileBuffer), ArchiveCount,
                                NextGroup);
}

// This is for -l<basename>. We'll look for lib<basename>.so or lib<basename>.a
// from search paths.
template <typename LibraryPathIterator>
llvm::Optional<std::string> Driver::searchLibraryBaseName(
    llvm::StringRef Name,
    const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths,
    bool IsStatic) {
  for (auto It = LibraryPaths.first; It != LibraryPaths.second; ++It) {
    const auto &Dir = *It;
    if (!IsStatic) {
      if (const llvm::Optional<std::string> S =
              Driver::findFile(Dir, "lib" + Name + ".so")) {
        return S;
      }
    }
    if (const llvm::Optional<std::string> S =
            Driver::findFile(Dir, "lib" + Name + ".a")) {
      return S;
    }
  }
  return llvm::None;
}

} // end namespace rld

#endif // RLD_DRIVER_H
