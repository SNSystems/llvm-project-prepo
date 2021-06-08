//===- include/rld/XfxScanner.h ---------------------------*- mode: C++ -*-===//
//* __  __ __        ____                                   *
//* \ \/ // _|_  __ / ___|  ___ __ _ _ __  _ __   ___ _ __  *
//*  \  /| |_\ \/ / \___ \ / __/ _` | '_ \| '_ \ / _ \ '__| *
//*  /  \|  _|>  <   ___) | (_| (_| | | | | | | |  __/ |    *
//* /_/\_\_| /_/\_\ |____/ \___\__,_|_| |_|_| |_|\___|_|    *
//*                                                         *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_XFX_SCANNER_H
#define RLD_XFX_SCANNER_H

#include "rld/LayoutBuilder.h"
#include "rld/symbol.h"

#include <cassert>

namespace rld {

/// \param Storage  A chunked-vector which will be used to manage the storage.
/// \param Required  The number of contiguous elements required.
/// \result A pointer to a contiguous block of storage which is sufficient for
/// \p Required members.
template <typename ChunkedVector,
          typename ValueType = typename ChunkedVector::value_type>
ValueType *reserveContiguous(ChunkedVector *const Storage,
                             size_t const Required) {
  assert(Storage != nullptr);
  assert(Required <= ChunkedVector::elements_per_chunk);
  if (Required == 0U) {
    return nullptr;
  }
  size_t const Capacity = Storage->capacity();
  size_t Size = Storage->size();
  if (Capacity - Size < Required) {
    // A resize to burn through the remaining members of the container's final
    // chunk.
    Storage->resize(Capacity);
    Size = Capacity;
  }
  // Add a nullptr. This is the first element of the returned array and allows
  // us to use back() to get the starting address.
  Storage->emplace_back(nullptr);
  ++Size;
  assert(Storage->size() == Size &&
         "Size didn't track the container size correctly");
  ValueType *const Result = &Storage->back();
  Storage->resize(Size + Required - 1U);
  assert(static_cast<size_t>(&Storage->back() + 1 - Result) == Required &&
         "Storage isn't contiguous");
  return Result;
}

/// \param ResolvedFixups  A container for the results of resolving external
/// fixups.

LocalPLTsContainer
resolveXfixups(Context &Context, const LocalSymbolsContainer &Locals,
               const NotNull<rld::GlobalSymbolsContainer *> Globals,
               const NotNull<UndefsContainer *> Undefs,
               const NotNull<pstore::chunked_vector<Symbol *> *> ResolvedFixups,
               uint32_t InputOrdinal);

} // end namespace rld

#endif // RLD_XFX_SCANNER_H
