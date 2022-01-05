//===- lib/Archive.cpp ----------------------------------------------------===//
//*     _             _     _            *
//*    / \   _ __ ___| |__ (_)_   _____  *
//*   / _ \ | '__/ __| '_ \| \ \ / / _ \ *
//*  / ___ \| | | (__| | | | |\ V /  __/ *
//* /_/   \_\_|  \___|_| |_|_| \_/ \___| *
//*                                      *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/Archive.h"

#include "rld/ErrorCode.h"
#include "rld/GroupSet.h"
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

static void reportError(std::atomic<bool> *const ErrorFlag,
                        const Context &Context, const Twine &InputFilePath,
                        const Error &Err) {
  const std::lock_guard<decltype(Context.IOMut)> _{Context.IOMut};
  errs() << InputFilePath << ": Error: " << Err << '\n';
  ErrorFlag->store(true, std::memory_order_relaxed);
}

// create CompilationRef for library member
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static void createCompilationRefForLibraryMember(
    std::atomic<bool> *const ErrorFlag, Context &Context,
    const CompilationIndex &CompilationIndex,
    const CompilationRef LibraryMember, GroupSet *const NextGroup) {
  llvmDebug(DebugType, Context.IOMut, [&]() {
    dbgs() << "Definitions in \"" << *LibraryMember.Origin << "\" (position "
           << LibraryMember.Position.first << ','
           << LibraryMember.Position.second << ")\n";
  });

  auto Pos = CompilationIndex->find(Context.Db, LibraryMember.Digest);
  if (Pos == CompilationIndex->end(Context.Db)) {
    return reportError(
        ErrorFlag, Context, *LibraryMember.Origin,
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
      dbgs() << "CompilationRef: '"
             << loadStdString(Context.Db, Definition.name) << "'\n";
    });

    const auto Create = [&] {
      llvmDebug(DebugType, Context.IOMut, [&]() {
        dbgs() << "  Create CompilationRef: '"
               << loadStdString(Context.Db, Definition.name) << "'\n";
      });
      return shadow::TaggedPointer{Context.createCompilationRef(LibraryMember)};
    };
    const auto CreateFromCompilationRef = [&](shadow::AtomicTaggedPointer *,
                                              CompilationRef *const CR) {
      // There's an existing CompilationRef for this symbol. Check their
      // positions and keep the lower.
      if (LibraryMember.Position < CR->Position) {
        return shadow::TaggedPointer{Create()};
      }
      llvmDebug(DebugType, Context.IOMut, [&] {
        dbgs() << "  Rejected: '" << loadStdString(Context.Db, Definition.name)
               << "' in favor of (" << CR->Position.first << ','
               << CR->Position.second << ')';
      });
      return shadow::TaggedPointer{CR};
    };
    const auto Update = [&](shadow::AtomicTaggedPointer *const P,
                            Symbol *const Sym) {
      auto D = Sym->definition();
      // Do we have a definition for this symbol?
      if (std::get<Symbol::OptionalBodies &>(D)) {
        return shadow::TaggedPointer{Sym};
      }
      // No. A definition in an archive has matched with an undefined symbol
      // so turn the undef into an CompilationRef to represent this archive
      // definition. Add this compilation to the next group to be resolved.
      NextGroup->insert(P);

      // FIXME: a version of createCompilationRef that takes a symbol.
      CompilationRef *const CR = Context.createCompilationRef(LibraryMember);
      CR->Sym = Sym;
      return shadow::TaggedPointer{CR};
    };
    shadow::set(Context.shadowPointer(Definition.name), Create,
                CreateFromCompilationRef, Update);
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

// iterate archive members
// ~~~~~~~~~~~~~~~~~~~~~~~
void iterateArchiveMembers(std::atomic<bool> *const ErrorFlag, Context &Context,
                           ThreadPool *const WorkPool,
                           const std::string ArchivePath,
                           const unsigned ArchiveIndex,
                           const std::shared_ptr<MemoryBuffer> FileBuffer,
                           GroupSet *const NextGroup,
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
        WorkPool->async(
            createCompilationRefForLibraryMember, ErrorFlag, std::ref(Context),
            CompilationIndex,
            CompilationRef{*CompilationDigestOrError, MemberPath,
                           std::make_pair(ArchiveIndex, ChildCount)},
            NextGroup);
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
