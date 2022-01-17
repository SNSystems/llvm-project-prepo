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

#define RLD_PARTITION_EMIT_LIST 1

namespace rld {

/// Forms an intrusive linked list of symbols that will later be used when it
/// comes to writing the symbols to the output.
template <typename T> class EmitList {
public:
  void append(T *Sym);
  void last();

  T *head() { return Head_; }
  const T *head() const { return Head_; }
  size_t size() const { return Size_; }

private:
  T *Head_ = nullptr;
  T *Last_ = nullptr;
  size_t Size_ = 0U;

#if RLD_PARTITION_EMIT_LIST
  static constexpr auto PartitionSize_ = 128U;
  pstore::chunked_sequence<T *> Partitions_;
#endif // RLD_PARTITION_EMIT_LIST
};

template <typename T> void EmitList<T>::append(T *const Sym) {
  assert(Sym != nullptr);
  assert((Head_ == nullptr) == (Last_ == nullptr));
#if RLD_PARTITION_EMIT_LIST
  if (Partitions_.size() % PartitionSize_ == 0U) {
    Partitions_.emplace_back(Sym);
  }
#endif // RLD_PARTITION_EMIT_LIST
  ++Size_;
  if (Head_ == nullptr) {
    Head_ = Last_ = Sym;
  } else {
    if (Last_ == Sym || Sym->NextEmit != nullptr) {
      return; // Already in the list.
    }
    if (Last_->setNextEmit(Sym)) {
      Last_ = Sym;
    }
  }
}

template <typename T> void EmitList<T>::last() {
#if RLD_PARTITION_EMIT_LIST
  Partitions_.emplace_back(nullptr);
#endif // RLD_PARTITION_EMIT_LIST
}

using SymbolEmitList = EmitList<Symbol>;

struct SymbolOrder {
  const Symbol *Local = nullptr;
  size_t LocalsSize = 0U;
  const Symbol *Global = nullptr;

  SymbolOrder() = default;
  SymbolOrder(const SymbolEmitList &Local, const SymbolEmitList &Global)
      : Local{Local.head()}, LocalsSize{Local.size()}, Global{Global.head()} {}
  SymbolOrder(SymbolOrder const &) = default;
  SymbolOrder(SymbolOrder &&) = default;

  SymbolOrder &operator=(SymbolOrder const &) = default;
  SymbolOrder &operator=(SymbolOrder &&) = default;

  template <typename Function>
  void walk(const UndefsContainer &Undefs, Function F) const {
    auto W = [&F](const Symbol *S) {
      for (; S != nullptr; S = S->NextEmit) {
        assert(S != S->NextEmit);
        F(*S);
      }
    };
    W(Local);
    W(Global);
    std::for_each(std::begin(Undefs), std::end(Undefs), F);
  }
};

} // namespace rld

#endif // RLD_SYMBOL_EMIT_LIST_H
