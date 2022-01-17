//===- lib/context.cpp ----------------------------------------------------===//
//*                  _            _    *
//*   ___ ___  _ __ | |_ _____  _| |_  *
//*  / __/ _ \| '_ \| __/ _ \ \/ / __| *
//* | (_| (_) | | | | ||  __/>  <| |_  *
//*  \___\___/|_| |_|\__\___/_/\_\\__| *
//*                                    *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/Context.h"
#include "rld/Symbol.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/Memory.h"
#include "llvm/Support/raw_ostream.h"

// Use this group name for NamedRegionTimer.
const char *rld::TimerGroupName = "rld";
const char *rld::TimerGroupDescription = "rld prepo linker";

size_t
rld::stringLength(const pstore::database &Db,
                  const pstore::typed_address<pstore::indirect_string> Addr) {
  pstore::shared_sstring_view Owner;
  return pstore::get_sstring_view(Db, Addr, &Owner).length();
}

size_t rld::stringLength(const pstore::database &Db,
                         const pstore::address Addr) {
  pstore::shared_sstring_view Owner;
  return pstore::get_sstring_view(Db, Addr, &Owner).length();
}

std::string
rld::loadStdString(const pstore::database &Db,
                   const pstore::typed_address<pstore::indirect_string> Addr) {
  pstore::shared_sstring_view Owner;
  return std::string{loadString(Db, Addr, &Owner)};
}

std::string rld::loadStdString(const pstore::database &Db,
                               const pstore::address Addr) {
  pstore::shared_sstring_view Owner;
  return std::string{loadString(Db, Addr, &Owner)};
}

llvm::StringRef rld::stringViewAsRef(pstore::raw_sstring_view S) {
  return {S.data(), S.size()};
}

//*   ___         _           _    *
//*  / __|___ _ _| |_ _____ _| |_  *
//* | (__/ _ \ ' \  _/ -_) \ /  _| *
//*  \___\___/_||_\__\___/_\_\\__| *
//*                                *
// ctor
// ~~~~
rld::Context::Context(pstore::database &D, llvm::StringRef EntryPoint)
    : Db{D}, EntryPoint_{EntryPoint},
      ShadowDb_{createShadowMemory(static_cast<std::size_t>(Db.size()))} {}

void rld::Context::MmapDeleter::operator()(std::uint8_t *P) const {
  llvm::sys::MemoryBlock Block(static_cast<void *>(P), Len_);
  llvm::sys::Memory::releaseMappedMemory(Block);
}

// record compilation
// ~~~~~~~~~~~~~~~~~~
pstore::repo::compilation const &rld::Context::recordCompilation(
    pstore::extent<pstore::repo::compilation> const &CompilationExtent) {
  std::lock_guard<std::mutex> Lock{CompilationsMut_};
  Compilations_.emplace_back(
      pstore::repo::compilation::load(this->Db, CompilationExtent));
  return *Compilations_.back();
}

// create shadow memory
// ~~~~~~~~~~~~~~~~~~~~
auto rld::Context::createShadowMemory(std::size_t Size) -> ShadowPtr {
  using llvm::sys::Memory;

  std::error_code EC;
  llvm::sys::MemoryBlock const MB = Memory::allocateMappedMemory(
      Size, nullptr /*near*/, Memory::MF_READ | Memory::MF_WRITE, EC);
  return ShadowPtr{reinterpret_cast<std::uint8_t *>(EC ? nullptr : MB.base()),
                   MmapDeleter(Size)};
}

// merge triple
// ~~~~~~~~~~~~
llvm::ErrorOr<llvm::Triple>
rld::Context::mergeTriple(pstore::repo::compilation const &Compilation) {
  // TODO: store the triple directly to avoid the need for string parsing here?
  pstore::shared_sstring_view Owner;
  pstore::raw_sstring_view const TripleString =
      pstore::get_sstring_view(this->Db, Compilation.triple(), &Owner);
  llvm::Triple const CompilationTriple{
      llvm::StringRef{TripleString.data(), TripleString.length()}};

  std::lock_guard<std::mutex> const _{TripleMut_};
  if (!Triple_) {
    Triple_ = CompilationTriple;
  } else {
    if (!Triple_->isCompatibleWith(CompilationTriple)) {
      // FIXME: a proper error code.
      return make_error_code(std::errc::not_enough_memory);
    }
    Triple_ = llvm::Triple{Triple_->merge(CompilationTriple)};
  }
  return *Triple_;
}

// triple
// ~~~~~~
llvm::Optional<llvm::Triple> rld::Context::triple() const {
  std::lock_guard<std::mutex> const _{TripleMut_};
  return Triple_;
}
