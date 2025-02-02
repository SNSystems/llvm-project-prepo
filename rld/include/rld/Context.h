//===- include/rld/Context.h ------------------------------*- mode: C++ -*-===//
//*   ____            _            _    *
//*  / ___|___  _ __ | |_ _____  _| |_  *
//* | |   / _ \| '_ \| __/ _ \ \/ / __| *
//* | |__| (_) | | | | ||  __/>  <| |_  *
//*  \____\___/|_| |_|\__\___/_/\_\\__| *
//*                                     *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_CONTEXT_H
#define RLD_CONTEXT_H

#include "rld/Shadow.h"
#include "rld/Types.h"

#include "pstore/adt/chunked_sequence.hpp"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/Format.h"

#include <functional>
#include <memory>
#include <mutex>
#include <unordered_map>

namespace rld {

using UintptrAddress = pstore::typed_address<std::uintptr_t>;

inline pstore::raw_sstring_view
loadString(const pstore::database &Db,
           const pstore::typed_address<pstore::indirect_string> Addr,
           const NotNull<pstore::shared_sstring_view *> Owner) {
  return pstore::get_sstring_view(Db, Addr, Owner);
}
inline pstore::raw_sstring_view
loadString(const pstore::database &Db, const pstore::address Addr,
           const NotNull<pstore::shared_sstring_view *> Owner) {
  return pstore::get_sstring_view(Db, Addr, Owner);
}

std::string
loadStdString(const pstore::database &Db,
              const pstore::typed_address<pstore::indirect_string> Addr);
std::string loadStdString(const pstore::database &Db,
                          const pstore::address Addr);

llvm::StringRef stringViewAsRef(pstore::raw_sstring_view S);

size_t stringLength(const pstore::database &Db,
                    const pstore::typed_address<pstore::indirect_string> Addr);
size_t stringLength(const pstore::database &Db, const pstore::address Addr);

class Symbol;

#ifndef NDEBUG
inline bool llvmDebugEnabled(char const *DebugFype) {
  return llvm::DebugFlag && llvm::isCurrentDebugType(DebugFype);
}
#else
inline constexpr bool llvmDebugEnabled(char const *DebugFype) { return false; }
#endif // NDEBUG

#ifndef NDEBUG
// This is exactly the same as the LLVM_DEBUG macro except that it's not a macro
// and hence a lot safer!
template <typename Function>
inline void llvmDebug(char const *DebugFype, Function F) {
  if (llvmDebugEnabled(DebugFype)) {
    F();
  }
}
template <typename Function>
inline void llvmDebug(char const *DebugType, std::mutex &IOMut, Function F) {
  if (llvmDebugEnabled(DebugType)) {
    const std::lock_guard<std::mutex> _{IOMut};
    F();
  }
}
#else
template <typename Function> inline void llvmDebug(char const *, Function) {}
template <typename Function>
inline void llvmDebug(char const *, std::mutex &, Function) {}
#endif

// Use this group name for NamedRegionTimer.
extern const char *TimerGroupName;
extern const char *TimerGroupDescription;

template <typename T> inline llvm::FormattedNumber format_hex(T N) {
  return llvm::format_hex_no_prefix(N, sizeof(N), true);
}

} // end namespace rld

namespace rld {

struct CompilationRef {
  CompilationRef(const pstore::index::digest Digest_,
                 const std::shared_ptr<std::string> &Origin_,
                 const std::pair<unsigned, unsigned> &Position_)
      : Digest{Digest_}, Origin{Origin_}, Position{Position_} {}

  /// The compilation's digest.
  const pstore::index::digest Digest;
  // TODO: perhaps a pointer into a global char vector instead? Would avoid
  // reference counting.
  /// A string describing the source of the compilation. Will be either the path
  /// of an individual ticket file or the path of a library and its member name
  /// in parentheses.
  const std::shared_ptr<std::string> Origin;
  /// The (x,y) coordinate of this compilation in the space used to determine
  /// which library member is selected in the event of there being more than one
  /// definition of the name with which this CompilationRef is associated.
  const std::pair<unsigned, unsigned> Position;

  Symbol *Sym = nullptr;
};

class Context {
public:
  Context(pstore::database &D, llvm::StringRef EntryPoint);

  std::uint8_t *shadow() noexcept { return ShadowDb_.get(); }
  const std::uint8_t *shadow() const noexcept { return ShadowDb_.get(); }

  const llvm::StringRef entryPoint() const { return EntryPoint_; }

  auto mergeTriple(pstore::repo::compilation const &Compilation)
      -> llvm::ErrorOr<llvm::Triple>;
  auto triple() const -> llvm::Optional<llvm::Triple>;

  pstore::database &Db;
  mutable std::mutex IOMut;
  // True if use of named timers is enabled.
  bool TimersEnabled = false;

  std::atomic<uint64_t> ELFStringTableSize{1U};
  std::atomic<unsigned> PLTEntries{0U};
  /// The number of GOT entries to create in the output.
  std::atomic<unsigned> GOTEntries{0U};

  pstore::repo::compilation const &recordCompilation(
      pstore::extent<pstore::repo::compilation> const &CompilationExtent);

  uint64_t baseAddress() const { return BaseAddress_; }

  void setOrdinalName(uint32_t InputOrdinal, llvm::StringRef Name) {
    std::lock_guard<std::mutex> const _{OrdinalNamesMut_};
    OrdinalNames_[InputOrdinal] = std::string{Name};
  }

  const std::string ordinalName(uint32_t InputOrdinal) const {
    std::lock_guard<std::mutex> const _{OrdinalNamesMut_};
    const auto Pos = OrdinalNames_.find(InputOrdinal);
    return Pos != OrdinalNames_.end() ? Pos->second : std::string{""};
  }

  CompilationRef *createCompilationRef(const CompilationRef &CR) {
    std::lock_guard<decltype(CompilationRefsMutex_)> _{CompilationRefsMutex_};
    return &CompilationRefs_.push_back(CR);
  }
  CompilationRef *
  createCompilationRef(const pstore::index::digest &Digest,
                       const llvm::StringRef &Origin,
                       const std::pair<unsigned, unsigned> &Position) {
    std::lock_guard<decltype(CompilationRefsMutex_)> _{CompilationRefsMutex_};
    return &CompilationRefs_.emplace_back(
        Digest, std::make_shared<std::string>(Origin), Position);
  }

  template <typename T>
  inline shadow::AtomicTaggedPointer *
  shadowPointer(const pstore::typed_address<T> Addr) {
    assert(Addr.absolute() % alignof(shadow::AtomicTaggedPointer) == 0);
    return reinterpret_cast<shadow::AtomicTaggedPointer *>(this->shadow() +
                                                           Addr.absolute());
  }
  template <typename T>
  inline const shadow::AtomicTaggedPointer *
  shadowPointer(const pstore::typed_address<T> Addr) const {
    assert(Addr.absolute() % alignof(shadow::AtomicTaggedPointer) == 0);
    return reinterpret_cast<const shadow::AtomicTaggedPointer *>(
        this->shadow() + Addr.absolute());
  }

private:
  class MmapDeleter {
  public:
    explicit MmapDeleter(std::size_t Len) : Len_{Len} {}
    void operator()(std::uint8_t *P) const;

  private:
    std::size_t const Len_;
  };
  using ShadowPtr = std::unique_ptr<std::uint8_t, MmapDeleter>;
  ShadowPtr createShadowMemory(std::size_t Size);

  const llvm::StringRef EntryPoint_;
  ShadowPtr ShadowDb_;

  mutable std::mutex TripleMut_;
  llvm::Optional<llvm::Triple> Triple_;

  std::mutex CompilationsMut_;
  pstore::chunked_sequence<std::shared_ptr<pstore::repo::compilation const>>
      Compilations_;

  mutable std::mutex OrdinalNamesMut_;
  llvm::DenseMap<uint32_t, std::string> OrdinalNames_;

  uint64_t BaseAddress_ = 0x0000000000400000;

  std::mutex CompilationRefsMutex_;
  pstore::chunked_sequence<CompilationRef> CompilationRefs_;
};

} // namespace rld
#endif // RLD_CONTEXT_H
