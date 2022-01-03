#ifndef RLD_ARCHIVE_H
#define RLD_ARCHIVE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorOr.h"

#include "pstore/core/hamt_map.hpp"
#include "pstore/core/index_types.hpp"

#include <atomic>
#include <memory>

namespace llvm {

class MemoryBuffer;
class MemoryBufferRef;
class ThreadPool;

} // end namespace llvm

namespace rld {

class Context;
class GroupSet;

enum class FileKind { Ticket, Archive, Unknown };

llvm::ErrorOr<FileKind> getFileKind(const llvm::MemoryBufferRef &Memory);

using CompilationIndex = std::shared_ptr<pstore::index::compilation_index>;
void iterateArchiveMembers(std::atomic<bool> *const ErrorFlag, Context &Context,
                           llvm::ThreadPool *WorkPool,
                           const std::string ArchivePath, unsigned ArchiveIndex,
                           const std::shared_ptr<llvm::MemoryBuffer> FileBuffer,
                           GroupSet *const NextGroup,
                           CompilationIndex const &CompilationIndex);
} // end namespace rld

#endif // RLD_ARCHIVE_H
