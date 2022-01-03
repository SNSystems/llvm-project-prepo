#include "rld/Driver.h"

#include "rld/ErrorCode.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace llvm;

namespace {

using CompilationExtent = pstore::extent<pstore::repo::compilation>;

class FileDescriptorCloser {
public:
  explicit constexpr FileDescriptorCloser(int const FD) : FD_{FD} {}
  FileDescriptorCloser(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser(FileDescriptorCloser &&) noexcept = delete;

  ~FileDescriptorCloser() noexcept { ::close(FD_); }

  FileDescriptorCloser &operator=(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser &operator=(FileDescriptorCloser &&) noexcept = delete;

private:
  int const FD_;
};

} // end anonymous namespace

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
  FixupStorage_.release();
  GlobalSymbs_.release();
  WorkPool_.release();
  Context_.release();
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

} // end namespace rld
