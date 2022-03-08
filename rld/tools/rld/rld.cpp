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

#include "rld/Driver.h"
#include "rld/ErrorCode.h"

#include "llvm/Support/CommandLine.h"

using namespace llvm;

static bool reportError(const rld::Context &Context, const Twine &InputFilePath,
                        const std::error_code Error) {
  const std::lock_guard<std::mutex> _{Context.IOMut};
  errs() << InputFilePath << ": Error: " << Error.message() << '\n';
  return true;
}

using namespace rld;

namespace {

//FIXME: hack hack. These options are simply ignored ATM.
cl::opt<bool> GcSections{
    "gc-sections",
    cl::desc{"Enable garbage collection of unused sections (ignored)"}};
cl::opt<std::string> RPath{
    "rpath", cl::desc{"Add a DT_RUNPATH to the output (ignored)"}};
cl::opt<char> OptLevel{"O", cl::Prefix,
                       cl::desc{"Optimization level (ignored)"}};
cl::opt<bool> ExportDymamic{"export-dynamic",
                            cl::desc{"Export all dynamic symbols (ignored)"}};
// end hack hack.


cl::list<std::string> InputFiles{cl::Positional, cl::desc{"<input-file>..."}};

cl::opt<std::string> RepoPath{"repo", cl::Optional,
                              cl::desc{"Program repository file"},
                              cl::value_desc{"FILE"}, cl::init("./clang.db")};

cl::opt<std::string> EntryPoint{"entry-point", cl::desc{"Set start address"},
                                cl::value_desc{"ADDRESS"}, cl::init("_start")};
cl::alias EntryPoint2{"E", cl::Prefix, cl::desc{"Alias for --entry-point"},
                      cl::aliasopt{EntryPoint}};

cl::opt<std::string> OutputFileName{"output", cl::desc{"Set output file name"},
                                    cl::value_desc{"FILE"},
                                    cl::init("./a.out")};
cl::alias OutputFileName2{"o", cl::Prefix, cl::desc{"Alias for --output"},
                          cl::aliasopt{OutputFileName}};

cl::list<std::string> Libraries{"library",
                                cl::desc{"Search for library LIBNAME"},
                                cl::value_desc{"LIBNAME"}};
cl::alias Libraries2{"l", cl::Prefix, cl::desc{"Alias for --library"},
                     cl::aliasopt{Libraries}};

cl::list<std::string> LibraryPaths{
    "library-path", cl::desc{"Add DIRECTORY to library search path"},
    cl::value_desc{"DIRECTORY"}};
cl::alias LibraryPaths2{"L", cl::Prefix, cl::desc{"Alias for --library-path"},
                        cl::aliasopt{LibraryPaths}};

// Hidden options
// ==============
cl::opt<bool> TimersEnabled{
    "enable-timers", cl::Hidden,
    cl::desc{"Time each job, printing elapsed time for each on exit"}};
cl::opt<unsigned> NumWorkers{
    "workers", cl::Hidden, cl::desc{"Number of worker threads"},
    cl::init(heavyweight_hardware_concurrency().compute_thread_count())};

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

static ErrorOr<std::unique_ptr<pstore::database>> openRepository() {
  std::string const FilePath = getRepoPath();
  if (!sys::fs::exists(FilePath)) {
    return rld::ErrorCode::DatabaseNotFound;
  }
  return std::make_unique<pstore::database>(
      FilePath, pstore::database::access_mode::writable);
}

int main(int Argc, char *Argv[]) {
  cl::ParseCommandLineOptions(Argc, Argv, "Program Repository Linker");

  ExitOnError ExitOnErr;
  ExitOnErr.setBanner(std::string{Argv[0]} + ": error: ");

  ErrorOr<std::unique_ptr<pstore::database>> Db = openRepository();
  if (!Db) {
    errs() << "Error: " << Db.getError().message() << '\n';
    return EXIT_FAILURE;
  }

  auto CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(**Db);
  if (!CompilationIndex) {
    errs()
        << "Error: "
        << make_error_code(rld::ErrorCode::CompilationIndexNotFound).message()
        << '\n';
    return EXIT_FAILURE;
  }

  // We call release() on the database pointer here safe in the knowledge that
  // it is, for the moment, read-only.
  rld::Driver D(Db->release(), CompilationIndex, EntryPoint, NumWorkers,
                OutputFileName, reportError);
  return D.run(std::make_pair(InputFiles.begin(), InputFiles.end()),
               std::make_pair(Libraries.begin(), Libraries.end()),
               std::make_pair(LibraryPaths.begin(), LibraryPaths.end()))
             ? EXIT_SUCCESS
             : EXIT_FAILURE;
}
