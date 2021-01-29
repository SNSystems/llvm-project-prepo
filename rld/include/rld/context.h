//*                  _            _    *
//*   ___ ___  _ __ | |_ _____  _| |_  *
//*  / __/ _ \| '_ \| __/ _ \ \/ / __| *
//* | (_| (_) | | | | ||  __/>  <| |_  *
//*  \___\___/|_| |_|\__\___/_/\_\\__| *
//*                                    *
//===- include/rld/context.h ----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_CONTEXT_H
#define RLD_CONTEXT_H

#include "rld/types.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/Format.h"

#include "pstore/adt/chunked_vector.hpp"

#include <functional>
#include <memory>
#include <mutex>
#include <unordered_map>

namespace rld {

using UintptrAddress = pstore::typed_address<std::uintptr_t>;

pstore::raw_sstring_view
loadString(pstore::database const &Db,
           pstore::typed_address<pstore::indirect_string> Addr,
           NotNull<pstore::shared_sstring_view *> Owner);

std::string loadStdString(pstore::database const &Db,
                          pstore::typed_address<pstore::indirect_string> Addr);

llvm::StringRef stringViewAsRef(pstore::raw_sstring_view S);

class Symbol;

#ifndef NDEBUG
inline bool llvmDebugEnabled (char const *DebugFype) {
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
    const std::lock_guard <std::mutex> Lock (IOMut);
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

class SymbolTable;

class Context {
public:
  explicit Context(pstore::database &D);

  std::uint8_t *shadow() noexcept { return ShadowDb_.get(); }
  const std::uint8_t *shadow() const noexcept { return ShadowDb_.get(); }

  auto mergeTriple(pstore::repo::compilation const &Compilation)
      -> llvm::ErrorOr<llvm::Triple>;
  auto triple() const -> llvm::Optional<llvm::Triple>;

  pstore::database &Db;
  mutable std::mutex IOMut;

  pstore::repo::compilation const &recordCompilation(
      pstore::extent<pstore::repo::compilation> const &CompilationExtent);

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

  ShadowPtr ShadowDb_;

  mutable std::mutex TripleMut_;
  llvm::Optional<llvm::Triple> Triple_;

  std::mutex CompilationsMut_;
  std::list<std::shared_ptr<pstore::repo::compilation const>> Compilations_;
};

} // namespace rld
#endif // RLD_CONTEXT_H
