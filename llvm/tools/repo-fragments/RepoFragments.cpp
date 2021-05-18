//===- RepoFragments.cpp - Dump the names and digests using a ticket file.-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Optional.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/mcrepo/compilation.hpp"

#include <algorithm>
#include <limits>
#include <unordered_map>

using namespace llvm;

raw_ostream &operator<<(raw_ostream &OS, const pstore::index::digest &Digest) {
  return OS << Digest.to_hex_string();
}

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

// An object which can be used to produce the correct output for a particular
// name and digest pair from a compilation. The name will be omitted from output
// if the single-argument ctor is used.
struct NameAndDigest {
  explicit NameAndDigest(const pstore::index::digest &Digest)
      : Digest{Digest} {}
  NameAndDigest(const StringRef &Name, const pstore::index::digest &Digest)
      : Name{Name}, Digest{Digest} {}

  Optional<StringRef> Name;
  pstore::index::digest Digest;
};

raw_ostream &operator<<(raw_ostream &OS, const NameAndDigest &ND) {
  if (ND.Name) {
    OS << *ND.Name << ": ";
  }
  OS << ND.Digest;
  return OS;
}

// Returns an object that can be written to an ostream to produce the requested
// output.
static NameAndDigest makeNameAndDigest(bool DigestOnly, const StringRef &Name,
                                       const pstore::index::digest &Digest) {
  if (DigestOnly) {
    return NameAndDigest{Digest};
  }
  return NameAndDigest{Name, Digest};
}

/// Dumps selected entries from a compilation.
///
/// \tparam NameIterator  An input iterator whose value-type is compatible with
///   StringRef.
/// \param Db  The owning database instance.
/// \param Compilation  The compilation whose members are to be dumped.
/// \param First  The start of the range of objects to be dumped.
/// \param Last  The end of the range of objects to be dumped.
/// \param Dump  A function which is called to dump an individual name. It must
///   have a signature compatible with std::function<void(const
///   pstore::repo::definition &)>.
/// \returns False if one of the names in the [First, Last) range was not found
///   in the compilation. True otherwise.
template <typename NameIterator, typename DumpFunction>
static bool dumpSelected(const pstore::database &Db,
                         const pstore::repo::compilation &Compilation,
                         NameIterator First, NameIterator Last,
                         DumpFunction Dump) {
  using pstore::indirect_string;
  using pstore::repo::definition;

  bool Okay = true;

  // The collection of names defined by this compilation.
  std::unordered_map<indirect_string, const definition *> Names{
      Compilation.size()};
  std::transform(Compilation.begin(), Compilation.end(),
                 std::inserter(Names, std::begin(Names)),
                 [&Db](const definition &D) {
                   return std::make_pair(indirect_string::read(Db, D.name), &D);
                 });

  std::for_each(First, Last, [&](const StringRef &N) {
    auto View = pstore::make_sstring_view(N.data(), N.size());
    // If this name associated with a definition in this compilation?
    const auto Pos = Names.find(indirect_string{Db, &View});
    if (Pos != Names.end()) {
      // Yes, dump it.
      Dump(*Pos->second);
    } else {
      // The name was not found. Signal failure.
      Okay = false;
    }
  });

  return Okay;
}

namespace {

cl::opt<std::string> TicketPath(cl::Positional,
                                cl::desc("<ticket file>"),
                                cl::Required);
cl::list<std::string> Names(cl::Positional, cl::ZeroOrMore,
                            cl::desc("[<name>...]"));
cl::opt<std::string> RepoPath("repo",
                              cl::desc("Path of the program repository"),
                              cl::init("./clang.db"));
cl::opt<bool> DigestOnly("digest-only", cl::init(false),
                         cl::desc("Display the digest only"));
cl::alias DigestOnly2("d", cl::aliasopt(DigestOnly));
cl::opt<bool>
    UseComma("comma", cl::init(false),
             cl::desc("Output fields are comma (rather than CR) separated"));
cl::alias UseComma2("c", cl::aliasopt(UseComma));

} // end anonymous namespace

int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv,
                              "A utility to dump a compilation's names and "
                              "digests from a program repository.\n");

  pstore::database Db{getRepoPath(RepoPath),
                      pstore::database::access_mode::read_only};

  const ErrorOr<pstore::index::digest> DigestOrError =
      mc::repo::getDigestFromTicket(TicketPath, &Db);
  if (!DigestOrError) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrError.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  const auto CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  assert(CompilationIndex != nullptr &&
         "get_index<> should not return nullptr!");
  const auto CompilationPos = CompilationIndex->find(Db, *DigestOrError);
  if (CompilationPos == CompilationIndex->end(Db)) {
    errs() << "Error: compilation " << *DigestOrError << " was not found.\n";
    return EXIT_FAILURE;
  }

  const auto Compilation =
      pstore::repo::compilation::load(Db, CompilationPos->second);

  const auto *Sep = "";
  const auto *End = "";
  const auto Dump = [&](const pstore::repo::definition &Definition) {
    pstore::shared_sstring_view Owner;
    const auto View = pstore::indirect_string::read(Db, Definition.name)
                          .as_db_string_view(&Owner);
    outs() << Sep
           << makeNameAndDigest(DigestOnly,
                                StringRef{View.data(), View.length()},
                                Definition.digest);
    End = "\n";
    Sep = UseComma ? "," : "\n";
  };

  int ExitCode = EXIT_SUCCESS;
  if (Names.empty()) {
    // If no names are listed we just just dump the entire compilation.
    std::for_each(Compilation->begin(), Compilation->end(), Dump);
  } else {
    // Dump just the names requested by the user. If one or more are not
    // present, the tool returns failure.
    if (!dumpSelected(Db, *Compilation, Names.begin(), Names.end(), Dump)) {
      ExitCode = EXIT_FAILURE;
    }
  }

  outs() << End;
  return ExitCode;
}
