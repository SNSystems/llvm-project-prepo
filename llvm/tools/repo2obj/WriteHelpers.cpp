//===- WriteHelpers.cpp ---------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "WriteHelpers.h"

std::size_t write8(llvm::raw_ostream &OS, std::uint8_t V) {
  assert(OS.tell() % alignof(decltype(V)) == 0);
  OS.write(reinterpret_cast<char const *>(&V), sizeof(V));
  return sizeof(V);
}
