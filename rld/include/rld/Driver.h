#ifndef RLD_DRIVER_H
#define RLD_DRIVER_H

#include "rld/Archive.h"
#include "rld/ElfOutput.h"
#include "rld/GroupSet.h"
#include "rld/LayoutBuilder.h"
#include "rld/scanner.h"
#include "rld/symbol.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ThreadPool.h"
#include "llvm/Support/Timer.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"

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
  llvm::Error
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

  llvm::Error runImpl(CompilationGroup *const Group, GroupSet *const NextGroup);
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
llvm::Error Driver::run(
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
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBuffer =
            open(InputFilePath);
        if (!FileBuffer) {
          return this->sayError(InputFilePath, FileBuffer.getError());
        }
        const llvm::ErrorOr<FileKind> Kind = getFileKind(**FileBuffer);
        if (!Kind) {
          return this->sayError(InputFilePath, FileBuffer.getError());
        }
        switch (*Kind) {
        case FileKind::Ticket:
          if (llvm::ErrorOr<pstore::index::digest> DigestOrError =
                  llvm::mc::repo::getDigestFromTicket(**FileBuffer,
                                                      &Context_->Db)) {
            // Add this compilation to the initial group to be resolved.
            Group->try_emplace(*DigestOrError,
                               std::make_shared<std::string>(InputFilePath));
            // FIXME: report an error if this was already in the group.
          } else {
            return this->sayError(InputFilePath, DigestOrError.getError());
          }
          break;
        case FileKind::Archive:
          // The arguments to async() are passed to std::bind() whose arguments
          // must be copyable. This means that we can't pass the FileBuffer
          // unique_ptr<> but instead must wrap the underlying memory in a
          // shared)_ptr<>.
          WorkPool_->async(
              iterateArchiveMembers, &ErrorFlag_, std::ref(*Context_),
              WorkPool_.get(), std::string{InputFilePath}, ArchiveCount,
              std::shared_ptr<llvm::MemoryBuffer>{FileBuffer->release()},
              NextGroup, CompilationIndex_);
          ++ArchiveCount;
          break;
        case FileKind::Unknown:
          return this->sayError(
              InputFilePath,
              make_error_code(llvm::object::object_error::invalid_file_type));
        }
      });

  std::for_each(
      Libraries.first, Libraries.second, [&](llvm::StringRef Library) {
        this->addLibrary(Library, ArchiveCount++, NextGroup, LibraryPaths);
      });
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
    return this->sayError(*LibPath, FileBuffer.getError());
  }
  if (*Kind != FileKind::Archive) {
    // FIXME: Is there a better error?
    return this->sayError(
        *LibPath,
        make_error_code(llvm::object::object_error::invalid_file_type));
  }

  WorkPool_->async(iterateArchiveMembers, &ErrorFlag_, std::ref(*Context_),
                   WorkPool_.get(), *LibPath, ArchiveCount,
                   std::shared_ptr<llvm::MemoryBuffer>{FileBuffer->release()},
                   NextGroup, std::ref(CompilationIndex_));
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
