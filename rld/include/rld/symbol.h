//*                      _           _  *
//*  ___ _   _ _ __ ___ | |__   ___ | | *
//* / __| | | | '_ ` _ \| '_ \ / _ \| | *
//* \__ \ |_| | | | | | | |_) | (_) | | *
//* |___/\__, |_| |_| |_|_.__/ \___/|_| *
//*      |___/                          *
//===- include/rld/symbol.h -----------------------------------------------===//
// Copyright (c) 2017-2018 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
//===----------------------------------------------------------------------===//
#ifndef RLD_SYMBOL_H
#define RLD_SYMBOL_H

#include <atomic>
#include <memory>
#include <mutex>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/simple_ilist.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include "pstore/core/address.hpp"
#include "pstore/core/indirect_string.hpp"
#include "pstore/mcrepo/compilation.hpp"
#include "rld/context.h"

#include <atomic>
#include <thread>

namespace rld {

class SpinLock {
public:
  static constexpr uint64_t SpinsBeforeYield = 1000;

  void lock() {
    auto Count = uint64_t{0};
    while (Flag_.test_and_set(std::memory_order_acquire)) {
      if (++Count > SpinsBeforeYield) {
        std::this_thread::yield();
      }
    }
  }
  void unlock() { Flag_.clear(std::memory_order_release); }

private:
  std::atomic_flag Flag_ = ATOMIC_FLAG_INIT;
};

class Symbol;

class UndefsContainer {
public:
  using container = llvm::simple_ilist<Symbol>;

  bool empty() const {
    std::unique_lock<std::mutex> const Lock{mut_};
    return list_.empty();
  }
  size_t size() const {
    std::unique_lock<std::mutex> const Lock{mut_};
    return list_.size();
  }
  void remove(Symbol *Sym);
  void insert(Symbol *Sym);

  container::const_iterator begin() const { return list_.begin(); }
  container::const_iterator end() const { return list_.end(); }

private:
  mutable std::mutex mut_;
  container list_;
};

class Symbol : public llvm::ilist_node<Symbol> {
public:
  class Body {
  public:
    Body(pstore::repo::linkage Linkage,
         std::shared_ptr<const pstore::repo::fragment> const &Fragment,
         pstore::typed_address<pstore::repo::fragment> FAddr,
         uint32_t InputOrdinal)
        : InputOrdinal_{InputOrdinal}, Linkage_{Linkage}, Fragment_{Fragment},
          FAddr_{FAddr} {}

    pstore::repo::linkage linkage() const {
      return static_cast<pstore::repo::linkage>(Linkage_);
    }
    uint32_t inputOrdinal() const { return InputOrdinal_; }
    const std::shared_ptr<const pstore::repo::fragment> &fragment() const {
      return Fragment_;
    }
    pstore::typed_address<pstore::repo::fragment> fragmentAddress() const {
      return FAddr_;
    }

  private:
    /// The index number of the object file which defined this symbol. Used to
    /// guarantee that a linkonce instance from the first object file listed on
    /// the command line is retained regardless of the order in which the files
    /// are processed.
    uint32_t InputOrdinal_;
    /// The symbol's linkage.
    pstore::repo::linkage Linkage_;
    /// The fragment which provides the definition of this symbol.
    std::shared_ptr<const pstore::repo::fragment> Fragment_;
    /// The fragment's address. Used to find its shadow memory offset.
    pstore::typed_address<pstore::repo::fragment> FAddr_;
  };

  explicit Symbol(StringAddress N) {
    setName(N);
    assert(name() == N);
  }
  Symbol(StringAddress N, Body &&Definition)
      : Definition_{llvm::SmallVector<Body, 1>{std::move(Definition)}} {
    setName(N);
    assert(name() == N);
  }

  using Mutex = SpinLock;
  using BodyContainer = llvm::SmallVector<Body, 1>;
  using OptionalBodies = llvm::Optional<BodyContainer>;

  enum { DefinitionIndex, LockIndex };

  auto definition() const
      -> std::tuple<OptionalBodies const &, std::unique_lock<Mutex>> {
    std::unique_lock<Mutex> Lock{Mut_};
    return {std::cref(Definition_), std::move(Lock)};
  }

  auto definition() -> std::tuple<OptionalBodies &, std::unique_lock<Mutex>> {
    std::unique_lock<Mutex> Lock{Mut_};
    return {std::ref(Definition_), std::move(Lock)};
  }

  bool hasDefinition() const {
    std::lock_guard<Mutex> Lock{Mut_};
    assert((!Definition_.hasValue() || Definition_->size() > 0U) &&
           "A defined symbol must have a least 1 body");
    return Definition_.hasValue();
  }

  StringAddress name() const {
    const std::lock_guard<decltype(Mut_)> Lock{Mut_};
    static_assert(sizeof(Name_) == 5U,
                  "Expected the Name_ field to be 5 bytes");
    return StringAddress::make(Name_[0] |
                               (static_cast<uint64_t>(Name_[1]) << 8) |
                               (static_cast<uint64_t>(Name_[2]) << 16) |
                               (static_cast<uint64_t>(Name_[3]) << 24) |
                               (static_cast<uint64_t>(Name_[4]) << 32));
  }

  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this on success or nullptr if there was an existing non-append
  ///   definition of the symbol.
  Symbol *updateAppendSymbol(pstore::database const &Db,
                             pstore::repo::compilation_member const &CM,
                             NotNull<UndefsContainer *> const Undefs,
                             uint32_t InputOrdinal);

  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this on success or nullptr if there was an existing non-append
  ///   definition of the symbol.
  Symbol *updateCommonSymbol(pstore::database const &Db,
                             pstore::repo::compilation_member const &CM,
                             NotNull<UndefsContainer *> const Undefs,
                             uint32_t InputOrdinal);

  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this on success or nullptr if there was an existing incompatible
  ///   definition of the symbol.
  Symbol *updateExternalSymbol(pstore::database const &Db,
                               pstore::repo::compilation_member const &CM,
                               NotNull<UndefsContainer *> const Undefs,
                               uint32_t InputOrdinal);

  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this on success or nullptr if there was an existing incompatible
  /// definition of the symbol.
  Symbol *updateLinkOnceSymbol(pstore::database const &Db,
                               pstore::repo::compilation_member const &CM,
                               NotNull<UndefsContainer *> const Undefs,
                               uint32_t InputOrdinal);

  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this on success or nullptr if there was an existing incompatible
  /// definition of the symbol.
  Symbol *updateWeakSymbol(pstore::database const &Db,
                           pstore::repo::compilation_member const &CM,
                           NotNull<UndefsContainer *> const Undefs,
                           uint32_t InputOrdinal);

private:
  /// If we're an earlier input file than the one which was responsible
  /// for this definition, then ours beats it out; if we're later than
  /// ours is discarded.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this.
  Symbol *replaceIfLowerOrdinal(std::lock_guard<Mutex> const &Lock,
                                pstore::database const &Db,
                                pstore::repo::compilation_member const &CM,
                                uint32_t InputOrdinal);

  /// Associates a body with the symbol which must not already have one.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The owning database.
  /// \param CM  The new definition's compilation member.
  /// \param Undefs  The collection of undefined symbols.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  ///   Used to impose an order on the symbol definitions that is not related to
  ///   the order in which the files are scanned.
  /// \returns this.
  Symbol *defineImpl(std::lock_guard<Mutex> const &Lock,
                     pstore::database const &Db,
                     pstore::repo::compilation_member const &CM,
                     NotNull<UndefsContainer *> const Undefs,
                     uint32_t InputOrdinal);

  /// Associates a body with the symbol which must already have one.
  ///
  /// \param Lock A lock on the symbol's mutex.
  /// \param Db  The containing database.
  /// \param CM  The new definition's compilation member.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  /// Used to impose an order on the symbol definitions that is not related to
  /// the order in which the files are scanned.
  /// \returns this.
  Symbol *replaceImpl(std::lock_guard<Mutex> const &Lock,
                      pstore::database const &Db,
                      pstore::repo::compilation_member const &CM,
                      uint32_t InputOrdinal);

  /// Associates a body with the symbol which must already have one.
  ///
  /// \param Lock  A lock on the symbol's mutex.
  /// \param Db  The containing database.
  /// \param CM  The new definition's compilation member.
  /// \param InputOrdinal  The command-line index of the defining ticket file.
  /// Used to impose an order on the symbol definitions that is not related to
  /// the order in which the files are scanned.
  /// \param Fragment  The symbol's fragment. This is the data that is
  /// associated with the symbol.
  /// \returns this.
  Symbol *replaceImpl(std::lock_guard<Mutex> const &Lock,
                      pstore::database const &Db,
                      pstore::repo::compilation_member const &CM,
                      uint32_t const InputOrdinal, FragmentPtr const &Fragment);

  void setName(StringAddress N) {
    std::uint64_t const NAbs = N.absolute();
    static_assert(sizeof(Name_) == 5U,
                  "Expected the Name_ field to be 5 bytes");
    Name_[0] = static_cast<uint8_t>(NAbs & 0xFF);
    Name_[1] = static_cast<uint8_t>((NAbs >> 8) & 0xFF);
    Name_[2] = static_cast<uint8_t>((NAbs >> 16) & 0xFF);
    Name_[3] = static_cast<uint8_t>((NAbs >> 24) & 0xFF);
    Name_[4] = static_cast<uint8_t>((NAbs >> 32) & 0xFF);
  }

  mutable Mutex Mut_; // TODO: is this really better than std::mutex?
  /// The next field is a StringAddress, but optimized so that we don't need
  /// 8-bytes and 8-byte alignment.
  uint8_t Name_[(StringAddress::total_bits + 7) / 8]; // StringAddress Name_;

  /// We allow an array of definitions so that append symbols can associate
  /// multiple definitions with a single name.
  llvm::Optional<BodyContainer> Definition_;
};

inline void UndefsContainer::remove(Symbol *Sym) {
  std::lock_guard<std::mutex> const Lock{mut_};
  list_.remove(*Sym); // remove this symbol from the undef list.
}

inline void UndefsContainer::insert(Symbol *Sym) {
  std::lock_guard<std::mutex> const Lock{mut_};
  list_.push_back(*Sym); // add this symbol to the undef list.
}

// shadow pointer
// ~~~~~~~~~~~~~~
template <typename T>
inline std::atomic<Symbol *> *shadowPointer(Context &Ctx,
                                            pstore::typed_address<T> Addr) {
  assert(Addr.absolute() % alignof(std::atomic<Symbol *>) == 0);
  return reinterpret_cast<std::atomic<Symbol *> *>(Ctx.shadow() +
                                                   Addr.absolute());
}

// set symbol shadow
// ~~~~~~~~~~~~~~~~~
/// Shadow pointers are initially null, are set to "busy" whilst the pointee is
/// being created, then finally point to a symbol instance. This order is
/// strictly followed.
///
/// \tparam CreateOp A function with the signature `symbol *()`.
/// \tparam UpdateOp A function with the signature `symbol*(symbol *)`
/// \param Sptr A pointer to the symbol's "shadow" memory.
/// \param Create A function which is called if this is the first time this
///   symbol has been encountered. It should return a pointer to the newly
///   created symbol instance.
/// \param Update A function which is called if this is an occurrence of a
///   previously encountered symbol. It should return the symbol to be used
///   (normally the function's input value).
/// \return A symbol instance or null if a "duplicate symbol" error message
///   should be issued.
template <typename CreateOp, typename UpdateOp>
Symbol *setSymbolShadow(std::atomic<Symbol *> *Sptr, CreateOp Create,
                        UpdateOp Update) {
  static auto *const Busy =
      reinterpret_cast<Symbol *>(std::numeric_limits<std::uintptr_t>::max());
  // Is it defined (or in the process of being defined) by another module?
  Symbol *Symbol = nullptr;
  if (Sptr->compare_exchange_strong(Symbol, Busy, std::memory_order_acq_rel,
                                    std::memory_order_relaxed)) {
    assert(Sptr->load(std::memory_order_acquire) == Busy);
    // This is the first time we've encountered this symbol name.
    Symbol = Create();
    assert(Symbol != Busy);
    Sptr->store(Symbol, std::memory_order_release);
    return Symbol;
  }

  // We've previously seen either a reference or a definition of this symbol.
  //
  // Spin until *ns != busy. This ensures that no other thread is in in the
  // process of creating the symbol instance.
  auto Count = uint64_t{0};
  while ((Symbol = Sptr->load(std::memory_order_acquire)) == Busy) {
    if (++Count > SpinLock::SpinsBeforeYield) {
      std::this_thread::yield();
    }
  }
  assert(Symbol != nullptr);
  return Update(Symbol);
}

using LocalSymbolsContainer = llvm::DenseMap<StringAddress, Symbol *>;
/// The Global symbols are allocated in chunks of 4MiB.
using GlobalSymbolsContainer =
    pstore::chunked_vector<Symbol, (4 * 1024 * 1024) / sizeof(Symbol)>;

void debugDumpSymbols(Context const &Ctx,
                      GlobalSymbolsContainer const &Globals);

//*   ___ _     _          _    ___ _                          *
//*  / __| |___| |__  __ _| |__/ __| |_ ___ _ _ __ _ __ _ ___  *
//* | (_ | / _ \ '_ \/ _` | (_-<__ \  _/ _ \ '_/ _` / _` / -_) *
//*  \___|_\___/_.__/\__,_|_/__/___/\__\___/_| \__,_\__, \___| *
//*                                                 |___/      *
class GlobalsStorage {
public:
  explicit GlobalsStorage(unsigned NumWorkerThreads)
      : SymbolMemory_{NumWorkerThreads} {}

  /// Returns the global symbol memory block associated with the current thread,
  /// creating it if necessary.
  NotNull<GlobalSymbolsContainer *> getThreadSymbols();
  /// Splices the collection of global symbol memory blocks into a single
  /// container and returns it. The contents of this object are released in the
  /// process.
  GlobalSymbolsContainer all();

private:
  llvm::DenseMap<char *, GlobalSymbolsContainer> SymbolMemory_;
  std::mutex Mut_;
};

//*  ___            _         _   ___             _              *
//* / __|_  _ _ __ | |__  ___| | | _ \___ ___ ___| |_ _____ _ _  *
//* \__ \ || | '  \| '_ \/ _ \ | |   / -_|_-</ _ \ \ V / -_) '_| *
//* |___/\_, |_|_|_|_.__/\___/_| |_|_\___/__/\___/_|\_/\___|_|   *
//*      |__/                                                    *
class SymbolResolver {
public:
  explicit SymbolResolver(Context &Ctx) : Context_{Ctx} {}

  /// \tparam Function A function with the signature void(StringAddress).
  /// \param Undefs  The global collection of undefined symbols.
  /// \param Globals  A container which will hold new defined and undefined
  ///   symbols.
  /// \param Compilation  The database compilation record containing
  ///   the symbols to be defined and (via each fragment's xfixup vector)
  ///   referenced.
  /// \param InputOrdinal  The "index" of this compilation: used to
  ///   ensure consistent output regardless of the order of processing.
  /// \param ErrorFn  Called in the event of an error.
  template <typename Function>
  llvm::Optional<LocalSymbolsContainer>
  defineSymbols(NotNull<GlobalSymbolsContainer *> const Globals,
                NotNull<UndefsContainer *> const Undefs,
                pstore::repo::compilation const &Compilation,
                uint32_t InputOrdinal, Function ErrorFn);

  static Symbol *addUndefined(NotNull<GlobalSymbolsContainer *> const Globals,
                              NotNull<UndefsContainer *> const Undefs,
                              StringAddress const Name);

private:
  Symbol *defineSymbol(NotNull<GlobalSymbolsContainer *> const Globals,
                       NotNull<UndefsContainer *> const Undefs,
                       pstore::repo::compilation_member const &CM,
                       uint32_t InputCount);

  Symbol *add(NotNull<GlobalSymbolsContainer *> const Globals,
              pstore::repo::compilation_member const &CM,
              uint32_t InputOrdinal);

  Context &Context_;
};

template <typename Function>
llvm::Optional<LocalSymbolsContainer>
SymbolResolver::defineSymbols(NotNull<GlobalSymbolsContainer *> const Globals,
                              NotNull<UndefsContainer *> const Undefs,
                              pstore::repo::compilation const &Compilation,
                              uint32_t InputOrdinal, Function ErrorFn) {
  bool Error = false;

  LocalSymbolsContainer Locals{Compilation.size()};

  // Define the symbols in this module.
  for (pstore::repo::compilation_member const &CM : Compilation) {
    if (Symbol *const Symbol =
            this->defineSymbol(Globals, Undefs, CM, InputOrdinal)) {

      Locals.insert({CM.name, Symbol});
    } else {
      Error = true;
      ErrorFn(CM.name); // Allow the error to be reported to the user.
    }
  }

  if (Error) {
    return {llvm::None};
  }
  return {std::move(Locals)};
}

Symbol *referenceSymbol(Context &Ctx, StringAddress Name,
                        LocalSymbolsContainer const &Locals,
                        NotNull<GlobalSymbolsContainer *> const Globals,
                        NotNull<UndefsContainer *> const Undefs);

} // end namespace rld
#endif // RLD_SYMBOL_H
