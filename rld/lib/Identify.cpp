//*  ___    _            _   _  __        *
//* |_ _|__| | ___ _ __ | |_(_)/ _|_   _  *
//*  | |/ _` |/ _ \ '_ \| __| | |_| | | | *
//*  | | (_| |  __/ | | | |_| |  _| |_| | *
//* |___\__,_|\___|_| |_|\__|_|_|  \__, | *
//*                                |___/  *
//===- lib/Identify.cpp ---------------------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
//===----------------------------------------------------------------------===//
#include "rld/Identify.h"
#include "rld/context.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"
#include "pstore/support/uint128.hpp"

#include "llvm/BinaryFormat/Magic.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/Archive.h"
#include "llvm/Support/MemoryBuffer.h"

#include <iterator>
#include <unordered_map>

#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#else
#include <io.h>
#endif

using namespace llvm;

namespace {
auto DebugType = "rld-Identify";

class FileDescriptorCloser {
public:
  explicit constexpr FileDescriptorCloser(int const FD) : FD_{FD} {}

  // No move or copy.
  FileDescriptorCloser(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser(FileDescriptorCloser &&) = delete;
  FileDescriptorCloser &operator=(FileDescriptorCloser const &) = delete;
  FileDescriptorCloser &operator=(FileDescriptorCloser &&) = delete;

  ~FileDescriptorCloser() { ::close(FD_); }

private:
  int const FD_;
};

} // end anonymous namespace

template <typename Function> bool rld::Identifier::error(Function F) {
  const std::lock_guard<std::mutex> Lock{IOMut_};
  auto &OS = llvm::errs();
  OS << "Error: ";
  F(OS);
  return false;
}

// add
// ~~~
std::pair<bool, size_t> rld::Identifier::add(const StringRef Path,
                                             const size_t InputOrdinal) {
  auto FD = 0;
  if (const std::error_code Erc = sys::fs::openFileForRead(Path, FD /*out!*/)) {
    return {this->error([&](raw_ostream &OS) {
              OS << "Could not open \"" << Path << "\" (" << Erc.message()
                 << ")\n";
            }),
            size_t{0}};
  }
  FileDescriptorCloser const FDCloser{FD};
  sys::fs::file_status Status;
  if (const std::error_code Erc = sys::fs::status(FD, Status /*out!*/)) {
    return {this->error([&](raw_ostream &OS) {
              OS << "Could not stat \"" << Path << "\" (" << Erc.message()
                 << ")\n";
            }),
            size_t{0}};
  }

  ErrorOr<std::unique_ptr<MemoryBuffer>> ContentsOrError =
      MemoryBuffer::getOpenFile(sys::fs::convertFDToNativeFile(FD), Path,
                                Status.getSize());
  if (!ContentsOrError) {
    return {this->error([&](raw_ostream &OS) {
              OS << "Could not load contents of \"" << Path << "\"\n";
            }),
            size_t{0}};
  }

  Ordinals Ord{InputOrdinal, 0};
  return this->addFile(Path, **ContentsOrError, Ord, false /*archive member?*/);
}

// getArchiveSymbols
// ~~~~~~~~~~~~~~~~~
auto rld::Identifier::getArchiveSymbols() -> ArchiveSymbolMap && {
  const std::lock_guard<decltype(ArchiveSymbolsMut_)> Lock{ArchiveSymbolsMut_};
  return std::move(ArchiveSymbols_);
}

// getCompilationVector
// ~~~~~~~~~~~~~~~~~~~~
auto rld::Identifier::getCompilationVector() -> CompilationVector && {
  const std::lock_guard<decltype(CompilationsMut_)> Lock{CompilationsMut_};
  return std::move(Compilations_);
}

// addFile
// ~~~~~~~
std::pair<bool, size_t> rld::Identifier::addFile(const Twine &Path,
                                                 MemoryBufferRef Contents,
                                                 Ordinals &Ord,
                                                 bool InArchive) {
  ErrorOr<FileKind> KindOrError = this->getFileKind(Contents);
  if (!KindOrError) {
    return {this->error([&](raw_ostream &OS) {
              OS << "File \"" << Path << "\". "
                 << KindOrError.getError().message() << '\n';
            }),
            size_t{0}};
  }

  std::pair<bool, std::size_t> Result{true, 0};
  switch (*KindOrError) {
  case FileKind::Ticket:
    Result = std::make_pair(this->addTicketFile(Path, Contents, Ord, InArchive),
                            std::size_t{1});
    break;
  case FileKind::Archive:
    Result = this->addArchiveContents(Path, Contents, Ord);
    break;
  case FileKind::Unknown:
    Result.first = this->error([&Path](raw_ostream &OS) {
      OS << "Unknown file type: \"" << Path << "\"\n";
    });
    break;
  }

  return Result;
}

// addTicketFile
// ~~~~~~~~~~~~~
bool rld::Identifier::addTicketFile(const Twine &Name, MemoryBufferRef Memory,
                                    const Ordinals &Ord, bool InArchive) {
  ErrorOr<pstore::index::digest> CompilationDigestOrError =
      mc::repo::getDigestFromTicket(Memory, &Db_);
  if (!CompilationDigestOrError) {
    return this->error([&](raw_ostream &OS) {
      OS << "File \"" << Name << "\". "
         << CompilationDigestOrError.getError().message() << '\n';
    });
  }

  // TODO: check the database UUID.
  // if we're in an archive, add the symbol definitions.
  auto Pos = Index_->find(Db_, *CompilationDigestOrError);
  if (Pos == Index_->end(Db_)) {
    return this->error([&](raw_ostream &OS) {
      OS << "The compilation referenced by ticket file \"" << Name << "\" ("
         << CompilationDigestOrError->to_hex_string() << ") was not found\n";
    });
  }

  if (!InArchive) {
    llvmDebug(DebugType, IOMut_, [&]() {
      dbgs() << "Adding ticket file: \"" << Name << "\" (" << std::get<0>(Ord)
             << ',' << std::get<1>(Ord) << ")\n";
    });

    const std::lock_guard<std::mutex> Lock{CompilationsMut_};
    Compilations_.emplace_back(Name.str(), Pos->second, std::get<0>(Ord));
    return true;
  }

  // Archive member.
  llvmDebug(DebugType, IOMut_, [&]() {
    dbgs() << "Adding archive ticket: \"" << Name << "\" (" << std::get<0>(Ord)
           << ',' << std::get<1>(Ord) << ")\n";
  });

  std::shared_ptr<const pstore::repo::compilation> Compilation =
      pstore::repo::compilation::load(Db_, Pos->second);
  const std::lock_guard<decltype(ArchiveSymbolsMut_)> Lock{ArchiveSymbolsMut_};
  for (const auto &Symbol : *Compilation) {
    ArchiveSymbols_.try_emplace(Symbol.name, Symbol, Ord);
  }

  return true;
}

// addArchiveContents
// ~~~~~~~~~~~~~~~~~~
std::pair<bool, size_t>
rld::Identifier::addArchiveContents(const Twine &ArchivePath,
                                    MemoryBufferRef Contents, Ordinals &Ord) {
  bool Okay = true;
  auto Err = Error::success();
  auto WriteError = [&ArchivePath, &Err](raw_ostream &OS) {
    OS << "Library \"" << ArchivePath << "\". " << Err << '\n';
  };
  auto ArchiveMembers = size_t{0};

  object::Archive Arch(Contents, Err);
  if (Err) {
    return {this->error(WriteError), ArchiveMembers};
  }
  iterator_range<object::Archive::child_iterator> ChildRange =
      Arch.children(Err /*out!*/);
  if (Err) {
    return {this->error(WriteError), ArchiveMembers};
  }

  for (const auto &Child : ChildRange) {
    Expected<MemoryBufferRef> ChildMemory = Child.getMemoryBufferRef();
    Expected<StringRef> ChildName = Child.getName();
    if (!ChildMemory || !ChildName) {
      Okay = this->error(WriteError);
      break;
    }

    bool Success;
    size_t Members;
    std::tie(Success, Members) =
        this->addFile(ArchivePath + "(" + Twine(*ChildName) + ")", *ChildMemory,
                      Ord, true /*archive member?*/);
    if (!Success) {
      Okay = false;
    }
    ++std::get<1>(Ord);
    ArchiveMembers += Members;
  }

  return {Okay, ArchiveMembers};
}

// getFileKind
// ~~~~~~~~~~~
auto rld::Identifier::getFileKind(MemoryBufferRef Memory) -> ErrorOr<FileKind> {
  // TODO: teach identify_magic about repo ticket files.
  if (Memory.getBufferSize() == mc::repo::TicketFileSize) {
    // It might be a ticket file.
    const ErrorOr<pstore::index::digest> DigestOrError =
        mc::repo::getDigestFromTicket(Memory, nullptr);
    if (DigestOrError) {
      return FileKind::Ticket;
    }
    if (DigestOrError.getError() !=
        make_error_code(mc::repo::TicketError::CorruptedTicket)) {
      return DigestOrError.getError();
    }
  }

  if (identify_magic(Memory.getBuffer()) == file_magic::archive) {
    return FileKind::Archive;
  }
  return FileKind::Unknown;
}
