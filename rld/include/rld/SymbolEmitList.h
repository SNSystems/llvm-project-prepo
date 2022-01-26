//===- include/rld/SymbolEmitList.h -----------------------*- mode: C++ -*-===//
//*  ____                  _           _   _____           _ _     _     _     _    *
//* / ___| _   _ _ __ ___ | |__   ___ | | | ____|_ __ ___ (_) |_  | |   (_)___| |_  *
//* \___ \| | | | '_ ` _ \| '_ \ / _ \| | |  _| | '_ ` _ \| | __| | |   | / __| __| *
//*  ___) | |_| | | | | | | |_) | (_) | | | |___| | | | | | | |_  | |___| \__ \ |_  *
//* |____/ \__, |_| |_| |_|_.__/ \___/|_| |_____|_| |_| |_|_|\__| |_____|_|___/\__| *
//*        |___/                                                                    *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_SYMBOL_EMIT_LIST_H
#define RLD_SYMBOL_EMIT_LIST_H

#include "rld/Symbol.h"

#include <cassert>

namespace rld {

/// Forms an intrusive linked list of symbols that will later be used when it
/// comes to writing the symbols to the output.
///
/// \tparam T  The symbol type.
/// \tparam P  The number of members in a partition.
template <typename T, size_t P> class EmitList {
public:
  static constexpr auto PartitionSize = P;
  using PartitionContainer = pstore::chunked_sequence<T *>;

  EmitList() = default;
  EmitList(EmitList const &) = delete;
  EmitList(EmitList &&Other);

  ~EmitList() = default;

  EmitList &operator=(EmitList const &) = delete;
  EmitList &operator=(EmitList &&Other);

  /// Append the symbol \p Sym to the emit list.
  /// \param Sym The symbol to be added to the emit list.
  /// \returns True is the symbol was added, false if it was already in the list.
  bool append(T *Sym);
  // Call to notify that the last symbol has been added.
  void last();

  T *head() { return Head_; }
  const T *head() const { return Head_; }
  size_t size() const { return Size_; }

  typename PartitionContainer::const_iterator partitionsBegin() const {
    return Partitions_.begin();
  }
  typename PartitionContainer::const_iterator partitionsEnd() const {
    return Partitions_.end();
  }

private:
  T *Head_ = nullptr;
  T *Last_ = nullptr;
  size_t Size_ = 0U;

  PartitionContainer Partitions_;
};

// (ctor)
// ~~~~~~
template <typename T, size_t P>
EmitList<T, P>::EmitList(EmitList &&Other)
    : Head_{Other.Head_}, Last_{Other.Last_}, Size_{Other.Size_},
      Partitions_{std::move(Other.Partitions_)} {}

// operator=
// ~~~~~~~~~
template <typename T, size_t P>
EmitList<T, P> &EmitList<T, P>::operator=(EmitList &&Other) {
  if (&Other != this) {
    Head_ = Other.Head_;
    Last_ = Other.Last_;
    Size_ = Other.Size_;
    Partitions_ = std::move(Other.Partitions_);
  }
  return *this;
}

// append
// ~~~~~~
template <typename T, size_t P> bool EmitList<T, P>::append(T *const Sym) {
  assert(Sym != nullptr);
  assert((Head_ == nullptr) == (Last_ == nullptr));

  if (Last_ == Sym || Sym->NextEmit != nullptr) {
    return false; // Already in the list.
  }

  if (Size_ % PartitionSize == 0U) {
    Partitions_.emplace_back(Sym);
  }
  ++Size_;

  if (Head_ == nullptr) {
    Head_ = Last_ = Sym;
  } else if (Last_->setNextEmit(Sym)) {
    Last_ = Sym;
  }
  return true;
}

// last
// ~~~~
template <typename T, size_t P> void EmitList<T, P>::last() {
  Partitions_.emplace_back(nullptr);
}

using SymbolEmitList = EmitList<Symbol, 512>;

class SymbolOrder {
public:
  SymbolOrder(SymbolEmitList &&Local, SymbolEmitList &&Global)
      : Local{std::move(Local)}, Global{std::move(Global)} {}
  SymbolOrder(SymbolOrder const &) = delete;
  SymbolOrder(SymbolOrder &&) = default;

  SymbolOrder &operator=(SymbolOrder const &) = delete;
  SymbolOrder &operator=(SymbolOrder &&) = default;

  const SymbolEmitList &locals() const { return Local; }
  const SymbolEmitList &globals() const { return Global; }

  /// Returns the number of local symbols.
  size_t localSize() const { return Local.size(); }

#if 1
  template <typename Function>
  void syncWalk(const UndefsContainer &Undefs, Function F) const {
    auto W = [&F](const Symbol *S) {
      for (; S != nullptr; S = S->NextEmit) {
        assert(S != S->NextEmit);
        F(*S);
      }
    };
    W(Local.head());
    W(Global.head());
    std::for_each(std::begin(Undefs), std::end(Undefs), F);
  }
#endif

private:
  SymbolEmitList Local;
  SymbolEmitList Global;
};

} // namespace rld

#endif // RLD_SYMBOL_EMIT_LIST_H
