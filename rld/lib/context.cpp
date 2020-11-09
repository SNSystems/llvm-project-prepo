//*                  _            _    *
//*   ___ ___  _ __ | |_ _____  _| |_  *
//*  / __/ _ \| '_ \| __/ _ \ \/ / __| *
//* | (_| (_) | | | | ||  __/>  <| |_  *
//*  \___\___/|_| |_|\__\___/_/\_\\__| *
//*                                    *
//===- lib/context.cpp ----------------------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
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
#include "rld/context.h"
#include "rld/symbol.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/Memory.h"
#include "llvm/Support/raw_ostream.h"

// Use this group name for NamedRegionTimer.
const char *rld::TimerGroupName = "rld";
const char *rld::TimerGroupDescription = "rld prepo linker";

pstore::raw_sstring_view
rld::loadString(pstore::database const &Db,
                pstore::typed_address<pstore::indirect_string> Addr,
                NotNull<pstore::shared_sstring_view *> Owner) {
  using namespace pstore::serialize;
  return read<pstore::indirect_string>(
             archive::database_reader{Db, Addr.to_address()})
      .as_string_view(Owner);
}

std::string
rld::loadStdString(pstore::database const &Db,
                   pstore::typed_address<pstore::indirect_string> Addr) {
  pstore::shared_sstring_view Owner;
  return std::string(loadString(Db, Addr, &Owner));
}

llvm::StringRef rld::stringViewAsRef(pstore::raw_sstring_view S) {
  return {S.data(), S.size()};
}


//*   ___         _           _    *
//*  / __|___ _ _| |_ _____ _| |_  *
//* | (__/ _ \ ' \  _/ -_) \ /  _| *
//*  \___\___/_||_\__\___/_\_\\__| *
//*                                *
rld::Context::Context(pstore::database &D)
    : Db{D}, ShadowDb_{
                 createShadowMemory(static_cast<std::size_t>(Db.size()))} {}

void rld::Context::MmapDeleter::operator()(std::uint8_t *P) const {
  llvm::sys::MemoryBlock Block(static_cast<void *>(P), Len_);
  llvm::sys::Memory::releaseMappedMemory(Block);
}

// createShadowMemory
// ~~~~~~~~~~~~~~~~~~
auto rld::Context::createShadowMemory(std::size_t Size) -> ShadowPtr {
  using llvm::sys::Memory;

  std::error_code EC;
  llvm::sys::MemoryBlock const MB = Memory::allocateMappedMemory(
      Size, nullptr /*near*/, Memory::MF_READ | Memory::MF_WRITE, EC);
  return ShadowPtr{reinterpret_cast<std::uint8_t *>(EC ? nullptr : MB.base()),
                   MmapDeleter(Size)};
}

// mergeTriple
// ~~~~~~~~~~~
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
