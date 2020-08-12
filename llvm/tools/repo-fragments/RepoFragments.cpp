//===- repo-fragments - Dump the names and digests using a ticket file. --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"

#include "pstore/dump/mcrepo_value.hpp"

using namespace llvm;

namespace {

cl::opt<std::string> RepoPath("repo", cl::Optional,
                              cl::desc("Path of the program repository"),
                              cl::init("./clang.db"));
cl::opt<std::string> TicketPath(cl::Positional,
                                cl::desc("<Path of the ticket file>"),
                                cl::Required);
cl::list<std::string>
    Names("names", cl::ZeroOrMore, cl::CommaSeparated,
          cl::desc("<Compilation member(s) to be displayed>"));
cl::opt<bool> DigestOnly("digest-only", cl::init(false),
                         cl::desc("Display the digest only"));
cl::opt<bool>
    UseComma("comma", cl::init(false),
             cl::desc("Output fields are comma (rather than CR) separated"));

} // anonymous namespace

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &Digest) {
  return OS << Digest.to_hex_string();
}

static std::string getRepoPath() {
  if (RepoPath.getNumOccurrences() == 0) {
    // TODO: remove this envrionment variable once the matching behavior is
    // removed from the compiler.
    if (auto File = getenv("REPOFILE")) {
      return {File};
    }
  }
  return RepoPath;
}

int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv,
                              "A utility to dump a compilation's names and "
                              "digests from a program repository.\n");

  ErrorOr<pstore::index::digest> DigestOrError =
      llvm::repo::getTicketIdFromFile(TicketPath);
  if (!DigestOrError) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrError.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  pstore::database Db(getRepoPath(), pstore::database::access_mode::read_only);
  const auto CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  if (!CompilationIndex) {
    errs() << "Error: compilation index was not found. \n";
    return EXIT_FAILURE;
  }

  pstore::index::digest const &Digest = DigestOrError.get();
  auto CompilationPos = CompilationIndex->find(Db, Digest);
  if (CompilationPos == CompilationIndex->end(Db)) {
    errs() << "Error: compilation " << Digest << " was not found.\n";
    return EXIT_FAILURE;
  }

  auto Compilation =
      pstore::repo::compilation::load(Db, CompilationPos->second);

  auto *Sep = "";
  for (auto const &CM : *Compilation) {
    outs() << Sep;
    Sep = (UseComma ? "," : "\n");
    assert(CM.name != pstore::typed_address<pstore::indirect_string>::null());
    const auto MemberName =
        pstore::indirect_string::read(Db, CM.name).to_string();
    if (Names.empty() ||
        (std::find(Names.begin(), Names.end(), MemberName) != Names.end())) {
      if (!DigestOnly)
        outs() << MemberName << ": ";
      outs() << CM.digest;
    }
  }
  outs() << "\n";

  return EXIT_SUCCESS;
}
