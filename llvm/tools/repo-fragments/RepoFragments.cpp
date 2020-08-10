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

static cl::OptionCategory ToolOptions("Tool Options");

static cl::opt<std::string> RepoPath("repo", cl::Optional,
                                     cl::desc("Path of the program repository"),
                                     cl::init("./clang.db"),
                                     cl::cat(ToolOptions));
static cl::opt<std::string> TicketPath(cl::Positional,
                                       cl::desc("<Path of the ticket file>"),
                                       cl::Required, cl::cat(ToolOptions));
static cl::list<std::string>
    Names("names", cl::ZeroOrMore, cl::CommaSeparated,
          cl::desc("<Compilation member(s) to be displayed>"),
          cl::cat(ToolOptions));
static cl::opt<bool> DigestOnly("digest-only", cl::init(false),
                                cl::desc("Display the digest only"),
                                cl::cat(ToolOptions));
static cl::opt<bool>
    UseComma("comma", cl::init(false),
             cl::desc("Output fields are comma (rather than CR) separated"),
             cl::cat(ToolOptions));
static cl::opt<bool> Verbose("verbose", cl::init(false),
                             cl::desc("Produce verbose output"),
                             cl::cat(ToolOptions));

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
  cl::HideUnrelatedOptions({&ToolOptions});
  cl::ParseCommandLineOptions(argc, argv,
                              "A repo utility for dumping the names and "
                              "digests using a ticket file.\n");

  ErrorOr<pstore::index::digest> DigestOrError =
      llvm::repo::getTicketIdFromFile(TicketPath);
  if (!DigestOrError) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrError.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  pstore::index::digest const &Digest = DigestOrError.get();
  if (Verbose) {
    outs() << "[ticket file ('" << TicketPath << ")' : \n" << Digest << "]\n\n";
  }

  pstore::database Db(getRepoPath(), pstore::database::access_mode::read_only);
  std::shared_ptr<pstore::index::compilation_index const> const
      CompilationIndex =
          pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  if (!CompilationIndex) {
    errs() << "Error: compilation index was not found. \n";
    return EXIT_FAILURE;
  }

  auto CompilationPos = CompilationIndex->find(Db, Digest);
  if (CompilationPos == CompilationIndex->end(Db)) {
    std::string DigestStr;
    raw_string_ostream SS{DigestStr};
    SS << Digest;
    errs() << "Error: compilation " + SS.str() + " was not found.\n";
    return EXIT_FAILURE;
  }

  auto Ticket = pstore::repo::compilation::load(Db, CompilationPos->second);
  if (Verbose) {
    outs() << "[Compilation]:\n";
    std::ostringstream out;
    pstore::dump::value_ptr Output = pstore::dump::make_value(Db, Ticket);
    Output->write(out);
    outs() << out.str() << "\n\n";
  }

  bool IsFirst = true;
  for (auto const &CM : *Ticket) {
    if (!IsFirst)
      outs() << (UseComma ? ',' : '\n');
    IsFirst = false;
    assert(CM.name != pstore::typed_address<pstore::indirect_string>::null());
    const auto Name = pstore::indirect_string::read(Db, CM.name).to_string();
    if (Names.empty() ||
        (std::find(Names.begin(), Names.end(), Name) != Names.end())) {
      if (!DigestOnly)
        outs() << pstore::indirect_string::read(Db, CM.name).to_string()
               << ": ";
      outs() << CM.digest;
    }
  }
  outs() << "\n";

  return EXIT_SUCCESS;
}
