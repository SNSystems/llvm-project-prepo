#include "rld/Archive.h"

#include "rld/ErrorCode.h"
#include "rld/Shadow.h"
#include "rld/context.h"
#include "rld/symbol.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/BinaryFormat/Magic.h"
#include "llvm/MC/MCRepoTicketFile.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/ThreadPool.h"

#include <tuple>
#include <unordered_set>

using namespace llvm;
using namespace rld;

namespace {
auto *const DebugType = "rld-Archive";
}

class GroupSet {
public:
  void insert(pstore::index::digest const compilation) {
    std::lock_guard<std::mutex> _{mutex};
    m.insert(compilation);
  }

  bool clear() {
    std::lock_guard<std::mutex> _{mutex};
    bool more = !m.empty();
    m.clear();
    return more;
  }

  template <typename Function> void iterate(Function function) {
    std::lock_guard<std::mutex> _{mutex};
    for (auto const &s : m) {
      function(s);
    }
  }

private:
  std::unordered_set<pstore::index::digest> m;
  std::mutex mutex;
};

static void reportError(std::atomic<bool> *const ErrorFlag,
                        const Context &Context, const Twine &InputFilePath,
                        const Error &Err) {
  const std::lock_guard<decltype(Context.IOMut)> _{Context.IOMut};
  errs() << InputFilePath << ": Error: " << Err << '\n';
  ErrorFlag->store(true, std::memory_order_relaxed);
}

namespace rld {

struct ArchDef {
  ArchDef(const pstore::index::digest CompilationDigest_,
          const std::shared_ptr<std::string> &MemberPath_,
          const std::pair<unsigned, unsigned> &Position_)
      : CompilationDigest{CompilationDigest_},
        MemberPath{MemberPath_}, Position{Position_} {}

  pstore::index::digest CompilationDigest;
  // TODO: perhaps a pointer into a global char vector instead? Would avoid
  // reference counting.
  std::shared_ptr<std::string> MemberPath;
  std::pair<unsigned, unsigned> Position;
};

} // end namespace rld

static ArchDef *createArchDef(const ArchDef &LibraryMember) {
  // FIXME: FIXME: FIXME:!!! These variables should not be global!
  static std::mutex ArchDefsMutex;
  static pstore::chunked_sequence<ArchDef> ArchDefs;

  std::lock_guard<decltype(ArchDefsMutex)> _{ArchDefsMutex};
  ArchDefs.push_back(LibraryMember);
  return &ArchDefs.back();
}

template <typename T>
inline std::atomic<void *> *shadowPointer(Context &Context,
                                          pstore::typed_address<T> Addr) {
  assert(Addr.absolute() % alignof(std::atomic<Symbol *>) == 0);
  return reinterpret_cast<std::atomic<void *> *>(Context.shadow() +
                                                 Addr.absolute());
}

void createArchDefsForLibraryMember(std::atomic<bool> *const ErrorFlag,
                                    Context &Context,
                                    const CompilationIndex &CompilationIndex,
                                    const ArchDef &LibraryMember,
                                    GroupSet *const NextGroup) {
  llvmDebug(DebugType, Context.IOMut, [&]() {
    dbgs() << "Definitions in \"" << *LibraryMember.MemberPath
           << "\" (position " << LibraryMember.Position.first << ','
           << LibraryMember.Position.second << ")\n";
  });

  auto Pos =
      CompilationIndex->find(Context.Db, LibraryMember.CompilationDigest);
  if (Pos == CompilationIndex->end(Context.Db)) {
    return reportError(
        ErrorFlag, Context, *LibraryMember.MemberPath,
        errorCodeToError(make_error_code(ErrorCode::CompilationNotFound)));
  }

  // Look through the definitions in this compilation.
  for (const auto &Definition : *Context.Db.getro(Pos->second)) {
    if (isLocalLinkage(Definition.linkage())) {
      // Definitions with local linkage don't constitute names that can resolve
      // undefs. We can skip them.
      continue;
    }
    llvmDebug(DebugType, Context.IOMut, [&]() {
      dbgs() << "ArchDef: '" << loadStdString(Context.Db, Definition.name)
             << "'\n";
    });

    const auto create = [&] {
      llvmDebug(DebugType, Context.IOMut, [&]() {
        dbgs() << "  Create archdef: '"
               << loadStdString(Context.Db, Definition.name) << "'\n";
      });
      return createArchDef(LibraryMember);
    };
    const auto createFromArchDef = [&](ArchDef *const AD) {
      // There's an existing archdef for this symbol. Check their positions and
      // keep the lower.
      if (LibraryMember.Position < AD->Position) {
        return create();
      }
      llvmDebug(DebugType, Context.IOMut, [&]() {
        dbgs() << "  Rejected: '" << loadStdString(Context.Db, Definition.name)
               << "' in favor of (" << AD->Position.first << ','
               << AD->Position.second << ')';
      });
      return AD;
    };
    const auto update = [&](Symbol *const Sym) {
      auto D = Sym->definition();
      // Do we have a definition for this symbol?
      if (!std::get<Symbol::OptionalBodies &>(D)) {
        // No. Create an ArchDef to represent this archive definition. Add this
        // compilation to the next group to be resolved.
        ArchDef *const AD = create();
        NextGroup->insert(AD->CompilationDigest);
      }
      return Sym;
    };
    shadow::set(shadowPointer(Context, Definition.name), create,
                createFromArchDef, update);
  }
}

namespace rld {

// get file kind
// ~~~~~~~~~~~~~
ErrorOr<FileKind> getFileKind(const MemoryBufferRef &Memory) {
  // FIXME: teach identify_magic about repo ticket files.
  if (Memory.getBufferSize() == mc::repo::TicketFileSize) {
    // It might be a ticket file.
    const ErrorOr<pstore::index::digest> DigestOrError =
        mc::repo::getDigestFromTicket(Memory, nullptr);
    if (DigestOrError) {
      return {FileKind::Ticket};
    }
    if (DigestOrError.getError() !=
        make_error_code(mc::repo::TicketError::NotATicket)) {
      return DigestOrError.getError();
    }
  }

  if (identify_magic(Memory.getBuffer()) == file_magic::archive) {
    return FileKind::Archive;
  }
  return FileKind::Unknown;
}

// FIXME: FIXME: FIXME:!!! These variables should not be global!
static GroupSet NextGroup;

// iterate archive members
// ~~~~~~~~~~~~~~~~~~~~~~~
void iterateArchiveMembers(std::atomic<bool> *const ErrorFlag, Context &Context,
                           ThreadPool *const WorkPool,
                           const StringRef ArchivePath,
                           const uint32_t ArchiveIndex,
                           const std::shared_ptr<MemoryBuffer> FileBuffer,
                           CompilationIndex const &CompilationIndex) {
  llvmDebug(DebugType, Context.IOMut, [&ArchivePath] {
    dbgs() << "Archive \"" << ArchivePath << "\"\n";
  });

  auto Err = Error::success();
  object::Archive Arch{*FileBuffer, Err /*out!*/};
  if (Err) {
    return reportError(ErrorFlag, Context, ArchivePath, Err);
  }
  const iterator_range<object::Archive::child_iterator> ChildRange =
      Arch.children(Err /*out!*/);
  if (Err) {
    return reportError(ErrorFlag, Context, ArchivePath, Err);
  }
  auto ChildCount = 0U;
  for (const auto &Child : ChildRange) {
    Expected<StringRef> ChildName = Child.getName();
    if (!ChildName) {
      return reportError(ErrorFlag, Context, ArchivePath,
                         ChildName.takeError());
    }
    SmallString<256> TempBuffer;
    auto MemberPath = std::make_shared<std::string>(
        (ArchivePath + "(" + *ChildName + ")").toStringRef(TempBuffer));
    llvmDebug(DebugType, Context.IOMut, [&MemberPath] {
      dbgs() << "Archive member: \"" << *MemberPath << "\"\n";
    });

    Expected<MemoryBufferRef> ChildMemory = Child.getMemoryBufferRef();
    if (!ChildMemory) {
      return reportError(ErrorFlag, Context, *MemberPath,
                         ChildMemory.takeError());
    }
    const ErrorOr<FileKind> Kind = getFileKind(*ChildMemory);
    if (!Kind) {
      return reportError(ErrorFlag, Context, *MemberPath,
                         errorCodeToError(Kind.getError()));
    }
    switch (*Kind) {
    case FileKind::Ticket:
      if (const ErrorOr<pstore::index::digest> CompilationDigestOrError =
              mc::repo::getDigestFromTicket(*ChildMemory, &Context.Db)) {
        // schedule a job for this compilation.
        WorkPool->async(createArchDefsForLibraryMember, ErrorFlag,
                        std::ref(Context), CompilationIndex,
                        ArchDef{*CompilationDigestOrError, MemberPath,
                                std::make_pair(ArchiveIndex, ChildCount)},
                        &NextGroup);
        ++ChildCount;
      } else {
        reportError(ErrorFlag, Context, *MemberPath,
                    errorCodeToError(CompilationDigestOrError.getError()));
      }
      break;
    case FileKind::Archive:
    case FileKind::Unknown:
      // Just ignore.
      break;
    }
  }
  if (Err) {
    reportError(ErrorFlag, Context, ArchivePath, Err);
  }
}

} // end namespace rld
