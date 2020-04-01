//===- MCRepoTicketFile.cpp - Repo Ticket File ------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCRepoTicketFile.h"

#include "pstore/core/index_types.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"

#include <array>

using namespace llvm;
using namespace repo;

namespace {
static constexpr auto MagicSize = size_t{8};
static const std::array<char, MagicSize> BERepoMagic{
    {'R', 'e', 'p', 'o', 'T', 'c', 'k', 't'}};
static const std::array<char, MagicSize> LERepoMagic{
    {'t', 'k', 'c', 'T', 'o', 'p', 'e', 'R'}};

class TicketErrorCategoryType final : public std::error_category {
  const char *name() const noexcept override { return "llvm.repo.ticket"; }

  std::string message(int IE) const override {
    switch (static_cast<llvm::repo::TicketError>(IE)) {
    case llvm::repo::TicketError::CorruptedTicket:
      return "Corrupted Ticket File";
    }
    llvm_unreachable("Unknown error type!");
  }
};
} // anonymous namespace

const uint64_t llvm::repo::TicketFileSize = MagicSize + sizeof(pstore::index::digest);

const std::error_category &llvm::repo::ticketErrorCategory() {
  static TicketErrorCategoryType Category;
  return Category;
}

void llvm::repo::writeTicketFile(support::endian::Writer &W,
                                 pstore::index::digest const &Digest) {
  std::array<char, MagicSize> const *Signature;
  std::uint64_t Out[2];

  if (W.Endian == support::little) {
    Signature = &LERepoMagic;
    support::endian::write64le(&Out[0], Digest.low());
    support::endian::write64le(&Out[1], Digest.high());
  } else {
    Signature = &BERepoMagic;
    support::endian::write64be(&Out[0], Digest.high());
    support::endian::write64be(&Out[1], Digest.low());
  }

  W.OS.write(Signature->data(), Signature->size());
  W.OS.write(reinterpret_cast<const char *>(Out), sizeof(Out));
}

ErrorOr<pstore::index::digest>
llvm::repo::getTicketId(llvm::MemoryBufferRef const & Buffer) {
  StringRef const Contents = Buffer.getBuffer();
  if (Contents.size () != TicketFileSize) {
    return std::error_code(TicketError::CorruptedTicket);
  }

  std::array<char, MagicSize> Signature;
  std::copy_n(std::begin(Contents), MagicSize, std::begin(Signature));

  if (Signature == LERepoMagic) {
    auto const Low = Contents.data() + MagicSize;
    auto const High = Low + sizeof(std::uint64_t);
    return {pstore::index::digest{support::endian::read64le(High),
                                  support::endian::read64le(Low)}};
  } else if (Signature == BERepoMagic) {
    auto const High = Contents.data() + MagicSize;
    auto const Low = High + sizeof(std::uint64_t);
    return {pstore::index::digest{support::endian::read64be(High),
                                  support::endian::read64be(Low)}};
  }
  return std::error_code(TicketError::CorruptedTicket);
}

ErrorOr<pstore::index::digest>
llvm::repo::getTicketIdFromFile(StringRef TicketPath) {
  int TicketFD = 0;
  if (std::error_code Err = sys::fs::openFileForRead(TicketPath, TicketFD)) {
    return Err;
  }

  sys::fs::file_status Status;
  if (std::error_code Err = sys::fs::status(TicketFD, Status)) {
    return Err;
  }
  uint64_t const FileSize = Status.getSize();
  if (FileSize != TicketFileSize) {
    return std::error_code(TicketError::CorruptedTicket);
  }

  ErrorOr<std::unique_ptr<MemoryBuffer>> MemberBufferOrErr =
      MemoryBuffer::getOpenFile(sys::fs::convertFDToNativeFile(TicketFD),
                                TicketPath, FileSize, false);
  if (!MemberBufferOrErr) {
    return MemberBufferOrErr.getError();
  }

  if (auto Err = sys::Process::SafelyCloseFileDescriptor(TicketFD)) {
    return Err;
  }

  return getTicketId (**MemberBufferOrErr);
}
