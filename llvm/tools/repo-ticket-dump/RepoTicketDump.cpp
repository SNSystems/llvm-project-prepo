//===- RepoTicketDump.cpp - Dump the contents of a prepo ticket file. -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "pstore/core/index_types.hpp"

#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

cl::opt<bool> DatabaseId{
    "database-id",
    cl::desc("Dump the ID of the database associated with the ticket rather "
             "than the compilation digest"),
    cl::init(false)};
cl::alias DatabaseId2{"I", cl::aliasopt(DatabaseId)};

cl::opt<std::string> TicketPath{cl::Positional, cl::desc("<ticket path>"),
                                cl::Optional, cl::init("./a.out")};

} // end anonymous namespace

raw_ostream &operator<<(raw_ostream &OS, pstore::index::digest const &Digest) {
  return OS << Digest.to_hex_string();
}

int main(int argc, char *argv[]) {
  cl::ParseCommandLineOptions(argc, argv);

  if (DatabaseId) {
    const ErrorOr<pstore::uuid> IDOrErr =
        mc::repo::getOwnerIDFromTicket(TicketPath);
    if (!IDOrErr) {
      errs() << "Error: '" << TicketPath << "' ("
             << IDOrErr.getError().message() << ")\n";
      return EXIT_FAILURE;
    }

    outs() << pstore::uuid{IDOrErr.get()}.str() << '\n';
    return EXIT_SUCCESS;
  }

  ErrorOr<pstore::index::digest> DigestOrErr =
      mc::repo::getDigestFromTicket(TicketPath, nullptr);
  if (!DigestOrErr) {
    errs() << "Error: '" << TicketPath << "' ("
           << DigestOrErr.getError().message() << ")\n";
    return EXIT_FAILURE;
  }

  outs() << DigestOrErr.get() << '\n';
  return EXIT_SUCCESS;
}
