#ifndef RLD_IDENTIFY_H
#define RLD_IDENTIFY_H

#include "rld/types.h"

#include "pstore/core/index_types.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorOr.h"

#include <mutex>
#include <utility>

namespace llvm {
class MemoryBuffer;
class MemoryBufferRef;
} // end namespace llvm
namespace pstore {
class database;
} // end namespace pstore

namespace rld {

using StringAddress = pstore::typed_address<pstore::indirect_string>;

/// A pair containing the archive and member ordinal values.
using Ordinals = std::pair<size_t, size_t>;

struct ArchiveSymbolInfo {
  ArchiveSymbolInfo(pstore::repo::compilation_member const &CM,
                    Ordinals const &O)
      : CompilationMember{CM}, Ordinal{O} {}

  pstore::repo::compilation_member CompilationMember;
  Ordinals Ordinal;
};

  using ArchiveSymbolMap =
      llvm::DenseMap<rld::StringAddress, ArchiveSymbolInfo>;

class Identifier {
public:
  using CompilationVector = std::vector<
      std::tuple<std::string, pstore::extent<pstore::repo::compilation>, size_t>>;

  /// \param Db  The program database.
  /// \param Index  The database's compilation index.
  /// \param IOMut  A mutex which guards access to the error and debug streams.
  Identifier(const pstore::database &Db, const CompilationIndexPtr &Index,
             std::mutex &IOMut)
      : Db_{Db}, Index_{Index}, IOMut_{IOMut} {}

  /// Adds the file contents at a specific path for processing. The
  /// input-ordinal value is used to ensure that we always produce consistent
  /// output regardless of how threads are scheduled.
  ///
  /// \param Path  The path of the file to be added to the link.
  /// \param InputOrdinal  The input-ordinal of the file to be added to the
  /// link.
  /// \returns A bool indicating success or failure, and a size_t giving
  /// the number of archive members in the file.
  std::pair<bool, size_t> add(llvm::StringRef Path, size_t InputOrdinal);

  /// Retrieves the collection of archive symbols. Call this function once all
  /// of the input files have been processed by add().
  ArchiveSymbolMap &&getArchiveSymbols();

  CompilationVector &&getCompilationVector();

private:
  enum class FileKind { Ticket, Archive, Unknown };
  static llvm::ErrorOr<FileKind> getFileKind(llvm::MemoryBufferRef Memory);

  std::pair<bool, size_t> addFile(const llvm::Twine &Path,
                                  llvm::MemoryBufferRef Contents, Ordinals &Ord,
                                  bool InArchive);
  std::pair<bool, size_t> addArchiveContents(const llvm::Twine &ArchivePath,
                                             llvm::MemoryBufferRef Contents,
                                             Ordinals &Ord);
  bool addTicketFile(const llvm::Twine &Name, llvm::MemoryBufferRef Memory,
                     const Ordinals &Ord, bool InArchive);

  template <typename Function> bool error(Function F);

  const pstore::database &Db_;       ///< The program database.
  const CompilationIndexPtr &Index_; ///< The database's compilation index.
  std::mutex &IOMut_; ///< Guards access to the error and debug streams.

  /// Guards access to the ArchiveSymbols member.
  std::mutex ArchiveSymbolsMut_;
  ArchiveSymbolMap ArchiveSymbols_;

  std::mutex CompilationsMut_;
  CompilationVector Compilations_;
};

} // end namespace rld

#endif // RLD_IDENTIFY_H
