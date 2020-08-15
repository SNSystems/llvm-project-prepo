//===- repo-fragments - Dump the names and digests using a ticket file. --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Optional.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/mcrepo/compilation.hpp"

#include <algorithm>
#include <limits>

using namespace llvm;

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &Digest) {
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

raw_ostream &operator<<(raw_ostream &OS, NameAndDigest const &ND) {
  if (ND.Name) {
    OS << *ND.Name << ": ";
  }
  OS << ND.Digest;
  return OS;
}

// Returns an object that can be written to an ostream to produce the requested
// output.
static NameAndDigest makeNameAndDigest(bool DigestOnly, const StringRef &Name,
                                       pstore::index::digest const &Digest) {
  if (DigestOnly) {
    return NameAndDigest{Digest};
  }
  return NameAndDigest{Name, Digest};
};

// Clamps the size_t input value to the maximum unsigned value.
static constexpr unsigned clamp_to_unsigned(size_t S) {
  constexpr size_t largest = std::numeric_limits<unsigned>::max();
  return static_cast<uint16_t>(S > largest ? largest : S);
}

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

} // end anonymous namespace

int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv,
                              "A utility to dump a compilation's names and "
                              "digests from a program repository.\n");

  const ErrorOr<pstore::index::digest> DigestOrError =
      llvm::repo::getTicketIdFromFile(TicketPath);
  if (!DigestOrError) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrError.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  pstore::database Db{getRepoPath(RepoPath),
                      pstore::database::access_mode::read_only};
  const auto CompilationIndex =
      pstore::index::get_index<pstore::trailer::indices::compilation>(Db);
  if (!CompilationIndex) {
    errs() << "Error: compilation index was not found.\n";
    return EXIT_FAILURE;
  }

  const auto CompilationPos = CompilationIndex->find(Db, *DigestOrError);
  if (CompilationPos == CompilationIndex->end(Db)) {
    errs() << "Error: compilation " << *DigestOrError << " was not found.\n";
    return EXIT_FAILURE;
  }

  // Copy CommandLineNames to a set. This both eliminates any duplicates and
  // gives us a fast find() function which can quickly determine whether a name
  // was requested by the user. (Unfortunately, DenseSet<> doesn't support
  // std::inserter(), so this is a hand-coded loop rather than a call to
  // std::copy().)
  DenseSet<StringRef> CommandLineNames{clamp_to_unsigned(Names.size())};
  for (const std::string &N : Names) {
    CommandLineNames.insert(N);
  }

  // Is 'Name' present in the CommandLineNames set? If CommandLineNames is
  // empty, then we consider this to be a set containing all names and always
  // return true.
  const auto IsRequestedName = [&CommandLineNames](const StringRef &Name) {
    return CommandLineNames.empty() ||
           CommandLineNames.find(Name) != std::end(CommandLineNames);
  };

  const auto *Sep = "";
  for (auto const &CM :
       *pstore::repo::compilation::load(Db, CompilationPos->second)) {
    // Convert the pstore indirect-string to a StringRef. We don't allocate any
    // unnecessary memory in these steps.
    pstore::shared_sstring_view Owner;
    const auto MemberNameView =
        pstore::indirect_string::read(Db, CM.name).as_db_string_view(&Owner);
    StringRef MemberNameRef{MemberNameView.data(), MemberNameView.length()};

    // Dump this digest (and perhaps name) if it was requested by the user.
    if (IsRequestedName(MemberNameRef)) {
      outs() << Sep << makeNameAndDigest(DigestOnly, MemberNameRef, CM.digest);
    }

    Sep = UseComma ? "," : "\n";
  }
  outs() << '\n';
  return EXIT_SUCCESS;
}
