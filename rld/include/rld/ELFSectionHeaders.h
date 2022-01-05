//===- include/rld/ELFSectionHeaders.h --------------------*- mode: C++ -*-===//
//*  _____ _     _____ ____            _   _              *
//* | ____| |   |  ___/ ___|  ___  ___| |_(_) ___  _ __   *
//* |  _| | |   | |_  \___ \ / _ \/ __| __| |/ _ \| '_ \  *
//* | |___| |___|  _|  ___) |  __/ (__| |_| | (_) | | | | *
//* |_____|_____|_|   |____/ \___|\___|\__|_|\___/|_| |_| *
//*                                                       *
//*  _   _                _                *
//* | | | | ___  __ _  __| | ___ _ __ ___  *
//* | |_| |/ _ \/ _` |/ _` |/ _ \ '__/ __| *
//* |  _  |  __/ (_| | (_| |  __/ |  \__ \ *
//* |_| |_|\___|\__,_|\__,_|\___|_|  |___/ *
//*                                        *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ELFSECTIONHEADERS_H
#define RLD_ELFSECTIONHEADERS_H

#include "rld/SectionArray.h"

#include "llvm/Object/ELF.h"

namespace rld {

class Layout;

namespace elf {

template <typename ELFT>
typename llvm::object::ELFFile<ELFT>::Elf_Shdr *emitSectionHeaders(
    typename llvm::object::ELFFile<ELFT>::Elf_Shdr *Shdr, const Layout &Lout,
    const SectionArray<llvm::Optional<int64_t>> &SectionFileOffsets,
    const EnumIndexedArray<SectionKind, SectionKind::last, uint64_t>
        &NameOffsets,
    size_t LocalsSize, uint64_t TargetDataOffset);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFSECTIONHEADERS_H
