//===- include/rld/ELF.h ----------------------------------*- mode: C++ -*-===//
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
#ifndef RLD_ELF_H
#define RLD_ELF_H

#include "llvm/ADT/Optional.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/raw_ostream.h"

#include "rld/LayoutBuilder.h"
#include "rld/SectionArray.h"

#include <memory>
#include <type_traits>

namespace llvm {
class raw_pwrite_stream;
class Triple;
} // namespace llvm

namespace rld {
namespace elf {

namespace details {

llvm::ErrorOr<unsigned>
machineFromTriple(llvm::Optional<llvm::Triple> const &Triple);

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty const &T) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), sizeof(T));
  return sizeof(T);
}

template <typename Ty, typename = typename std::enable_if<
                           std::is_standard_layout<Ty>::value>::type>
std::size_t writeRaw(llvm::raw_ostream &OS, Ty *T, std::size_t Size) {
  assert(OS.tell() % alignof(Ty) == 0);
  OS.write(reinterpret_cast<char const *>(&T), Size);
  return Size;
}

} // end namespace details

template <class ELFT>
auto initELFHeader(const unsigned Machine) -> typename ELFT::Ehdr {
  typename ELFT::Ehdr Header;
  Header.e_ident[llvm::ELF::EI_MAG0] = 0x7f;
  Header.e_ident[llvm::ELF::EI_MAG1] = 'E';
  Header.e_ident[llvm::ELF::EI_MAG2] = 'L';
  Header.e_ident[llvm::ELF::EI_MAG3] = 'F';
  Header.e_ident[llvm::ELF::EI_CLASS] =
      ELFT::Is64Bits ? llvm::ELF::ELFCLASS64 : llvm::ELF::ELFCLASS32;
  Header.e_ident[llvm::ELF::EI_DATA] =
      (ELFT::TargetEndianness == llvm::support::little)
          ? llvm::ELF::ELFDATA2LSB
          : llvm::ELF::ELFDATA2MSB;
  Header.e_ident[llvm::ELF::EI_VERSION] = llvm::ELF::EV_CURRENT;
  Header.e_ident[llvm::ELF::EI_OSABI] = llvm::ELF::ELFOSABI_NONE;
  Header.e_ident[llvm::ELF::EI_ABIVERSION] = 0;
  Header.e_type = llvm::ELF::ET_REL;
  Header.e_machine = Machine;
  Header.e_version = llvm::ELF::EV_CURRENT;
  Header.e_entry = 0;
  Header.e_phoff = 0; // patched up later
  Header.e_shoff = 0; // patched up later
  Header.e_flags = 0;
  Header.e_ehsize = sizeof(typename ELFT::Ehdr);
  Header.e_phentsize = sizeof(typename ELFT::Phdr);
  Header.e_phnum = 0; // patched up later
  Header.e_shentsize = sizeof(typename ELFT::Shdr);
  Header.e_shnum = 0;    // patched up later.
  Header.e_shstrndx = 0; // patched up later.
  return Header;
}

template <typename ELFT>
constexpr unsigned char elfVisibility(pstore::repo::visibility SV) {
  switch (SV) {
  case pstore::repo::visibility::default_vis:
    return llvm::ELF::STV_DEFAULT;
  case pstore::repo::visibility::hidden_vis:
    return llvm::ELF::STV_HIDDEN;
  case pstore::repo::visibility::protected_vis:
    return llvm::ELF::STV_PROTECTED;
  }
  llvm_unreachable("Unsupported visibility type");
}

} // end namespace elf
} // end namespace rld

#endif // RLD_ELF_H
