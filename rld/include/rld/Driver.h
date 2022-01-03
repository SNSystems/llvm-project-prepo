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

  static constexpr auto DebugType = "rld-Driver";
};

template <typename InputFileIterator, typename LibraryIterator,
          typename LibraryPathIterator>
llvm::Error Driver::run(
    const std::pair<InputFileIterator, InputFileIterator> &InputFiles,
    const std::pair<LibraryIterator, LibraryIterator> &Libraries,
    const std::pair<LibraryPathIterator, LibraryPathIterator> &LibraryPaths) {
  llvm::SmallVector<CompilationRef *, 256> Group;
  GroupSet NextGroup;
  this->scheduleGroup0(&Group, &NextGroup, InputFiles, Libraries, LibraryPaths);

  LayoutBuilder Layout{*Context_, &Undefs_};

  {
    llvm::NamedRegionTimer LayoutTimer{
        "Layout", "Output file layout", rld::TimerGroupName,
        rld::TimerGroupDescription, Context_->TimersEnabled};
    std::thread LayoutThread{&rld::LayoutBuilder::run, &Layout};
    rld::Scanner Scan{*Context_, Layout, &Undefs_};

    auto GroupNum = 0U;
    do {
      // print ("Group ", ngroup++, " compilations: ",
      // ios_printer::range<decltype (group)::const_iterator>{std::cbegin
      // (group), std::cend (group)});
      for (CompilationRef *const Compilation : Group) {
        WorkPool_->async(
            [&](CompilationRef *const C, const uint32_t Ordinal) {
              this->resolveCompilation(&Scan, *C->Origin, C->Digest, &NextGroup,
                                       Ordinal);
            },
            Compilation, InputOrdinal_);
        ++InputOrdinal_;
      }
      WorkPool_->wait();
      NextGroup.transferTo(&Group);
      ++GroupNum;
    } while (!Group.empty() && Undefs_.strongUndefCount() > 0U);

    Layout.endGroup();
    assert(Undefs_.strongUndefCountIsCorrect() &&
           "The strong undef count mus*t match the entries in the undefs list");
    if (Undefs_.strongUndefCount() > 0U) {
      std::lock_guard<decltype(Context_->IOMut)> Lock{Context_->IOMut};
      for (auto const &U : Undefs_) {
        assert(!U.hasDefinition());
        if (!U.allReferencesAreWeak()) {
          // TODO: also need to show where the reference is made.
          llvm::errs() << "Error: Undefined symbol '"
                       << loadStdString(Context_->Db, U.name()) << "'\n";
        }
      }
      ErrorFlag_.store(true);
    }

    llvm::Optional<llvm::Triple> Triple;
    if (!ErrorFlag_.load()) {
      Triple = Context_->triple();
      if (!Triple) {
        std::lock_guard<decltype(Context_->IOMut)> Lock{Context_->IOMut};
        llvm::errs() << "Error: The output triple could not be determined.\n";
        ErrorFlag_.store(true);
      }
    }
    llvmDebug(DebugType, Context_->IOMut, [this] {
      llvm::dbgs() << "Output triple: " << Context_->triple()->normalize()
                   << '\n';
    });

    if (ErrorFlag_.load()) {
      Layout.error();
    }
    LayoutThread.join();
  }

  if (ErrorFlag_.load()) {
    // FIXME: clearly this is wrong. It's a hack whilst I unify the error
    // reporting code.
    return llvm::Error::success();
  }

  std::tie(LO, GOTPLTs) = Layout.flattenSegments(
      Context_->baseAddress(),
      Layout.elfHeaderBlockSize<llvm::object::ELF64LE>());

  // Get the lists of local and global symbols from layout.
  SymbolOrder SymOrder = Layout.symbolOrder();

  auto AllSymbols =
      std::make_unique<rld::GlobalSymbolsContainer>(GlobalSymbs_->all());
  llvmDebug(DebugType, Context_->IOMut,
            [&] { debugDumpSymbols(*Context_, *AllSymbols); });

  // Now we set about emitting an ELF executable...
  rld::llvmDebug(DebugType, Context_->IOMut,
                 [] { llvm::dbgs() << "Beginning output\n"; });

  llvm::Error Err = elfOutput<llvm::object::ELF64LE>(
      OutputFileName_, *Context_, *AllSymbols, SymOrder, Undefs_, *WorkPool_,
      LO.get(), *GOTPLTs);
  WorkPool_->wait();
  return Err;
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
  auto InputFileCount = 0U;
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
            // Create a CompilationRef to represent this ticket's contents and
            // add it to the initial group of files to be resolved.
            Group->push_back(Context_->createCompilationRef(
                *DigestOrError, InputFilePath,
                std::make_pair(0U, InputFileCount)));
            ++InputFileCount;
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
