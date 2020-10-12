//===- MCRepoTicketFile.h - Repo Ticket File --------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCREPOTICKETFILE_H
#define LLVM_MC_MCREPOTICKETFILE_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorOr.h"

#include "pstore/core/uuid.hpp"
#include "pstore/support/uint128.hpp"

#include <system_error>

namespace pstore {
class database;
namespace index {
using digest = uint128;
} // end namespace index
} // end namespace pstore

namespace llvm {
class raw_ostream;
class MemoryBufferRef;

namespace mc {
namespace repo {

extern const uint64_t TicketFileSize;

const std::error_category &ticketErrorCategory();
enum class TicketError { CorruptedTicket = 1, DatabaseIDMismatch };
inline std::error_code make_error_code(TicketError E) {
  return {static_cast<int>(E), ticketErrorCategory()};
}

/// Write a ticket file to an endian Writer instance.
///
/// \param W The Writer to which to ticket file should be written.
/// \param Db  The database which owns the digest contained within the ticket
///   file.
/// \param Digest  The digest of a compilation contained with the database given
///   by \p Db.
void writeTicketFile(support::endian::Writer &&W, const pstore::database &Db,
                     const pstore::index::digest &Digest);

/// Write a ticket file to an endian Writer instance.
///
/// \param W The Writer to which to ticket file should be written.
/// \param Db  The database which owns the digest contained within the ticket
///   file.
/// \param Digest  The digest of a compilation contained with the database given
///   by \p Db.
void writeTicketFile(support::endian::Writer &W, const pstore::database &Db,
                     const pstore::index::digest &Digest);

/// Write a ticket file to supplied path.
///
/// \param Path The path of the ticket file to be written.
/// \param Db  The database which owns the digest contained within the ticket
///   file.
/// \param Digest  The digest of a compilation contained with the database given
///   by \p Db.
Error writeTicketFile(const llvm::StringRef &Path, const pstore::database &Db,
                      const pstore::index::digest &Digest);

/// Given an in-memory ticket, returns the digest that it contains. An optional
/// database instance can be passed. If supplied, the function checks that the
/// ticket reference this database and will yield an error if not.
///
/// \param Buffer  A memory buffer containing a ticket.
/// \param Owner  If non-null, the function verifies that the ticket is owned by
///   this database. If this check fails, an error is returned. If null, this
///   check is bypassed.
/// \returns  The index digest containing within the ticket or an error.
ErrorOr<pstore::index::digest>
getDigestFromTicket(const llvm::MemoryBufferRef &Buffer,
                    pstore::database const *Owner);

/// Returns the index digest contained within the ticket file at the given path.
/// An optional database instance can be passed. If supplied, the function
/// checks that the ticket reference this database and will yield an error if
/// not.
///
/// \param TicketPath  The path of a ticket file.
/// \param Owner  If non-null, the function verifies that the ticket is owned by
///   this database. If this check fails, an error is returned. If null, this
///   check is bypassed.
/// \returns  The index digest containing within the ticket or an error.
ErrorOr<pstore::index::digest>
getDigestFromTicket(StringRef TicketPath, pstore::database const *Owner);

/// Given an in-memory ticket, returns the ID of the database by which it is
/// "owned".
///
/// \param Buffer  A memory buffer containing a ticket.
/// \returns  The ID of the owning database or an error.
ErrorOr<pstore::uuid> getOwnerIDFromTicket(const llvm::MemoryBufferRef &Buffer);

/// Returns the ID of the database which owns the ticket at the given path.
///
/// \param TicketPath  The path of a ticket file.
/// \returns  The ID of the owning database or an error.
ErrorOr<pstore::uuid> getOwnerIDFromTicket(StringRef TicketPath);

} // end namespace repo
} // end namespace mc
} // end namespace llvm

namespace std {

template <>
struct is_error_code_enum<llvm::mc::repo::TicketError> : std::true_type {};

} // end namespace std

#endif // LLVM_MC_MCREPOTICKETFILE_H
