//===- lib/ELF.cpp --------------------------------------------------------===//
//*  _____ _     _____  *
//* | ____| |   |  ___| *
//* |  _| | |   | |_    *
//* | |___| |___|  _|   *
//* |_____|_____|_|     *
//*                     *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ELF.h"

llvm::ErrorOr<unsigned> rld::elf::details::machineFromTriple(
    llvm::Optional<llvm::Triple> const &Triple) {
  if (!Triple) {
    return std::errc::not_enough_memory; // FIXME: no triple was found
  }
  switch (Triple->getArch()) {
  case llvm::Triple::x86:
    return llvm::ELF::EM_386;
  case llvm::Triple::x86_64:
    return llvm::ELF::EM_X86_64;
  default:
    return std::errc::not_enough_memory; // FIXME: unknown machine type in
                                         // triple
  }
}

// FIXME: copied from repo2obj WriteHelpers.h
template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty const &T) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), sizeof(T));
  return sizeof(T);
}
