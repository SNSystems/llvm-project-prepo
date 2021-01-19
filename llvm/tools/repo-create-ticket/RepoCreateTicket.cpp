//===- RepoCreateTicket.cpp - Create a ticket file from a repository. -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ToolOutputFile.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"

using namespace llvm;

namespace {

cl::opt<std::string> CompilationDigest(cl::Positional, cl::Optional,
                                       cl::desc("<compilation-digest>"));
cl::opt<std::string>
    OutputPath("output", cl::Optional,
               cl::desc("Path of the ticket file to be created"),
               cl::init("./a.out"));
cl::alias OutputPath2("o", cl::aliasopt(OutputPath));
cl::opt<std::string> RepoPath("repo", cl::Optional,
                              cl::desc("Path of the program repository"),
                              cl::init("./clang.db"));

class BadDigestError : public ErrorInfo<BadDigestError> {
public:
  BadDigestError(const StringRef Digest) : Digest_{Digest} {}
  void log(raw_ostream &OS) const override {
    OS << "compilation digest \"" << Digest_ << "\" was not valid";
  }
  std::error_code convertToErrorCode() const override {
    return inconvertibleErrorCode();
  }

  static char ID;

private:
  std::string Digest_;
};

char BadDigestError::ID;

} // end anonymous namespace

static std::string getRepoPath(const cl::opt<std::string> &RepoPath) {
  if (RepoPath.getNumOccurrences() == 0) {
    // TODO: remove this environment variable once the matching behavior is
    // removed from the compiler.
    if (auto File = getenv("REPOFILE")) {
      return {File};
    }
  }
  return RepoPath;
}

static Expected<pstore::index::digest> digestFromCommandLine(StringRef Str) {
  std::unique_ptr<MemoryBuffer> Buffer;
  if (Str == "-") {
    ErrorOr<std::unique_ptr<MemoryBuffer>> BufferOrErr =
        MemoryBuffer::getSTDIN();
    if (!BufferOrErr) {
      return errorCodeToError(BufferOrErr.getError());
    }
    Buffer = std::move(*BufferOrErr);
    Str = Buffer->getBuffer();
  }

  const auto Digest = pstore::uint128::from_hex_string(Str.trim().str());
  if (!Digest) {
    return make_error<BadDigestError>(Str);
  }
  return *Digest;
}

int main(int argc, char *argv[]) {
  ExitOnError ExitOnErr("error: ", EXIT_FAILURE);
  cl::ParseCommandLineOptions(
      argc, argv,
      "Create a ticket file from an existing program repository.\n");

  pstore::database Db{getRepoPath(RepoPath),
                      pstore::database::access_mode::read_only};
  auto const Digest = ExitOnErr(digestFromCommandLine(CompilationDigest));
  const auto Index =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  assert(Index != nullptr);
  if (!Index->contains(Db, Digest)) {
    errs() << "error: compilation " << Digest.to_hex_string()
           << " was not found.\n";
    return EXIT_FAILURE;
  }

  // Write the ticket file to disk.

  std::error_code EC;
  ToolOutputFile OutputFile{OutputPath, EC, sys::fs::OF_None};
  if (EC) {
    errs() << "error: unable to open output file \"" << OutputPath
           << "\": " << EC.message() << '\n';
    return EXIT_FAILURE;
  }
  // Don't remove output file if we exit with an error.
  OutputFile.keep();

  raw_fd_ostream &OS = OutputFile.os();
  mc::repo::writeTicketFile(
      support::endian::Writer{OS, support::endianness::native}, Db, Digest);
  ExitOnErr(errorCodeToError(OS.error()));
  return EXIT_SUCCESS;
}
