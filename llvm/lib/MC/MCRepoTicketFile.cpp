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
#include "pstore/core/database.hpp"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CRC.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"

#include <array>
#include <type_traits>

using namespace llvm;

namespace {

static constexpr auto MagicSize = size_t{8};
static const std::array<char, MagicSize> BERepoMagic{
    {'R', 'e', 'p', 'o', 'T', 'c', 'k', 't'}};
static const std::array<char, MagicSize> LERepoMagic{
    {'t', 'k', 'c', 'T', 'o', 'p', 'e', 'R'}};

class TicketErrorCategory final : public std::error_category {
  const char *name() const noexcept override { return "llvm.repo.ticket"; }

  std::string message(int IE) const override {
    switch (static_cast<mc::repo::TicketError>(IE)) {
    case mc::repo::TicketError::CorruptedTicket:
      return "Corrupted Ticket File";
    case mc::repo::TicketError::DatabaseIDMismatch:
      return "Database ID mismatch";
    }
    llvm_unreachable("Unknown error type!");
  }
};

struct TicketFile {
  std::array<char, MagicSize> Magic;
  uint32_t CRC;
  uint16_t Version;
  uint16_t Unused;
  pstore::uuid OwnerID;
  pstore::index::digest Digest;
};

constexpr uint16_t TicketFileVersion = 1;

// Verify that the compiler used the expected structure layout so that the
// on-disk representation will be consistent between different machines.
static_assert(sizeof(TicketFile) == 48, "Expected TicketFile to be 48 bytes");
static_assert(std::is_standard_layout<TicketFile>::value,
              "TicketFile must be StandardLayout");
static_assert(offsetof(TicketFile, Magic) == 0,
              "Expected Magic to be at offset 0");
static_assert(offsetof(TicketFile, CRC) == 8, "Expected CRC to be at offset 8");
static_assert(offsetof(TicketFile, Version) == 12,
              "Expected Version to be at offset 12");
static_assert(offsetof(TicketFile, Unused) == 14,
              "Expected Unused to be at offset 14");
static_assert(offsetof(TicketFile, OwnerID) == 16,
              "Expected OwnerID to be at offset 16");
static_assert(offsetof(TicketFile, Digest) == 32,
              "Expected Digest to be at offset 32");

class ScopedFD {
public:
  ScopedFD() = default;
  explicit ScopedFD(int Descriptor) : FD_{Descriptor} {}
  ScopedFD(const ScopedFD &) = delete;
  ScopedFD(ScopedFD &&Other) {
    if (Other.FD_) {
      FD_ = *Other.FD_;
      Other.FD_.reset();
    }
  }
  ~ScopedFD() {
    if (FD_) {
      sys::Process::SafelyCloseFileDescriptor(*FD_);
    }
  }

  explicit operator bool() const { return FD_.hasValue(); }
  bool hasValue() const { return FD_.hasValue(); }
  int getValue() const { return FD_.getValue(); }

  ScopedFD &operator=(const ScopedFD &) = delete;
  ScopedFD &operator=(ScopedFD &&Other) {
    if (Other.FD_) {
      FD_ = *Other.FD_;
      Other.FD_.reset();
    } else {
      FD_.reset();
    }
    return *this;
  }

private:
  Optional<int> FD_;
};

} // end anonymous namespace

namespace pstore {

inline uint128 getSwappedBytes(uint128 const &V) {
  return {sys::getSwappedBytes(V.low()), sys::getSwappedBytes(V.high())};
}

} // end namespace pstore

const uint64_t llvm::mc::repo::TicketFileSize = sizeof(TicketFile);

const std::error_category &llvm::mc::repo::ticketErrorCategory() {
  static TicketErrorCategory Category;
  return Category;
}

template <typename Writer, typename T>
static size_t write(Writer &&W, const T *Data, size_t Elements) {
  const auto Size = Elements * sizeof(T);
  W.OS.write(reinterpret_cast<const char *>(Data), Size);
  return Size;
}

template <typename Writer>
static size_t
writeTicketFileImpl(Writer &&W, const std::array<char, MagicSize> &Signature,
                    const std::array<uint64_t, 2> &SwappedID,
                    const std::array<uint64_t, 2> &SwappedDigest) {

  auto BytesWritten = write(W, Signature.data(), Signature.size());
  BytesWritten += write(W, SwappedID.data(), SwappedID.size());
  BytesWritten += write(W, SwappedDigest.data(), SwappedDigest.size());
  return BytesWritten;
}

static uint32_t ticketCRC(const TicketFile &T) {
  const auto *Base =
      reinterpret_cast<const uint8_t *>(&T) + offsetof(TicketFile, Version);
  const size_t Size = sizeof(TicketFile) - offsetof(TicketFile, Version);
  return crc32(ArrayRef<uint8_t>{Base, Size});
}

template <typename Writer>
static size_t writeTicketFile(Writer &&W, const pstore::database &Db,
                              const pstore::index::digest &Digest) {
  const support::endianness Endian = W.Endian == support::native
                                         ? support::endian::system_endianness()
                                         : W.Endian;
  assert(Endian == support::little || Endian == support::big);

  TicketFile Ticket;
  Ticket.Magic = (W.Endian == support::little) ? LERepoMagic : BERepoMagic;
  Ticket.CRC = 0;
  Ticket.Version = support::endian::byte_swap(TicketFileVersion, Endian);
  Ticket.Unused = 0;
  Ticket.OwnerID = Db.get_header().id(); // No need to swap. OwnerID is a UUID
                                         // whose bytes are in network order.
  Ticket.Digest = support::endian::byte_swap(Digest, Endian);

  Ticket.CRC = support::endian::byte_swap(ticketCRC(Ticket), Endian);
  W.OS.write(reinterpret_cast<const char *>(&Ticket), sizeof(Ticket));
  return sizeof(Ticket);
}

void llvm::mc::repo::writeTicketFile(support::endian::Writer &&W,
                                     const pstore::database &Db,
                                     const pstore::index::digest &Digest) {
  const auto BytesWritten = ::writeTicketFile(W, Db, Digest);
  (void)BytesWritten;
  assert(BytesWritten == TicketFileSize &&
         "TicketFileSize did not match bytes written!");
}

void llvm::mc::repo::writeTicketFile(support::endian::Writer &W,
                                     const pstore::database &Db,
                                     const pstore::index::digest &Digest) {
  const auto BytesWritten = ::writeTicketFile(W, Db, Digest);
  (void)BytesWritten;
  assert(BytesWritten == TicketFileSize &&
         "TicketFileSize did not match bytes written!");
}

Error llvm::mc::repo::writeTicketFile(const llvm::StringRef &Path,
                                      const pstore::database &Db,
                                      const pstore::index::digest &Digest) {
  std::error_code EC;
  llvm::raw_fd_ostream OutFile{Path, EC};
  if (EC) {
    return errorCodeToError(EC);
  }
  writeTicketFile(
      llvm::support::endian::Writer{OutFile, llvm::support::endianness::native},
      Db, Digest);
  return errorCodeToError(OutFile.error());
}

static ErrorOr<TicketFile> getTicket(const llvm::MemoryBufferRef &Buffer,
                                     pstore::database const *const Owner) {
  const StringRef Contents = Buffer.getBuffer();
  assert(mc::repo::TicketFileSize == sizeof(TicketFile) &&
         "TicketFileSize must be sizeof(TicketFile)");
  if (Contents.size() != sizeof(TicketFile)) {
    return mc::repo::TicketError::CorruptedTicket;
  }

  TicketFile Ticket = *reinterpret_cast<const TicketFile *>(Contents.data());

  support::endianness Endian;
  if (Ticket.Magic == LERepoMagic) {
    Endian = support::little;
  } else if (Ticket.Magic == BERepoMagic) {
    Endian = support::big;
  } else {
    return mc::repo::TicketError::CorruptedTicket;
  }

  const uint32_t ExpectedCRC = ticketCRC(Ticket);

  Ticket.CRC = support::endian::byte_swap(Ticket.CRC, Endian);
  Ticket.Version = support::endian::byte_swap(Ticket.Version, Endian);
  Ticket.Digest = support::endian::byte_swap(Ticket.Digest, Endian);

  if (Ticket.Version != TicketFileVersion || Ticket.CRC != ExpectedCRC) {
    return mc::repo::TicketError::CorruptedTicket;
  }

  if (Owner != nullptr) {
    if (Ticket.OwnerID != Owner->get_header().id()) {
      return mc::repo::TicketError::DatabaseIDMismatch;
    }
  }
  return Ticket;
}

static ErrorOr<ScopedFD> openFile(StringRef Path) {
  int FD = 0;
  if (const std::error_code Err = sys::fs::openFileForRead(Path, FD)) {
    return Err;
  }
  return ScopedFD{FD};
}

static ErrorOr<std::unique_ptr<MemoryBuffer>>
openTicketFile(StringRef TicketPath) {
  const auto FDOrErr = openFile(TicketPath);
  if (!FDOrErr) {
    return FDOrErr.getError();
  }
  const int FD = FDOrErr->getValue();
  sys::fs::file_status Status;
  if (const std::error_code Err = sys::fs::status(FD, Status)) {
    return Err;
  }
  const uint64_t FileSize = Status.getSize();
  if (FileSize != mc::repo::TicketFileSize) {
    return mc::repo::TicketError::CorruptedTicket;
  }

  return MemoryBuffer::getOpenFile(sys::fs::convertFDToNativeFile(FD),
                                   TicketPath, FileSize, false);
}

ErrorOr<pstore::index::digest>
llvm::mc::repo::getDigestFromTicket(const llvm::MemoryBufferRef &Buffer,
                                    const pstore::database *const Owner) {
  const ErrorOr<TicketFile> TicketOrErr = getTicket(Buffer, Owner);
  if (!TicketOrErr) {
    return TicketOrErr.getError();
  }
  return TicketOrErr->Digest;
}

ErrorOr<pstore::index::digest>
llvm::mc::repo::getDigestFromTicket(StringRef TicketPath,
                                    pstore::database const *const Owner) {
  const auto MemoryBufferOrErr = openTicketFile(TicketPath);
  if (!MemoryBufferOrErr) {
    return MemoryBufferOrErr.getError();
  }
  return getDigestFromTicket(**MemoryBufferOrErr, Owner);
}

ErrorOr<pstore::uuid>
llvm::mc::repo::getOwnerIDFromTicket(const llvm::MemoryBufferRef &Buffer) {
  const ErrorOr<TicketFile> TicketOrErr = getTicket(Buffer, nullptr);
  if (!TicketOrErr) {
    return TicketOrErr.getError();
  }
  return TicketOrErr->OwnerID;
}

ErrorOr<pstore::uuid>
llvm::mc::repo::getOwnerIDFromTicket(StringRef TicketPath) {
  const auto MemoryBufferOrErr = openTicketFile(TicketPath);
  if (!MemoryBufferOrErr) {
    return MemoryBufferOrErr.getError();
  }
  return getOwnerIDFromTicket(**MemoryBufferOrErr);
}
