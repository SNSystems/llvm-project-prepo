//===- include/rld/ELFSectionNames.h ----------------------*- mode: C++ -*-===//
//*  _____ _     _____ ____            _   _              *
//* | ____| |   |  ___/ ___|  ___  ___| |_(_) ___  _ __   *
//* |  _| | |   | |_  \___ \ / _ \/ __| __| |/ _ \| '_ \  *
//* | |___| |___|  _|  ___) |  __/ (__| |_| | (_) | | | | *
//* |_____|_____|_|   |____/ \___|\___|\__|_|\___/|_| |_| *
//*                                                       *
//*  _   _                            *
//* | \ | | __ _ _ __ ___   ___  ___  *
//* |  \| |/ _` | '_ ` _ \ / _ \/ __| *
//* | |\  | (_| | | | | | |  __/\__ \ *
//* |_| \_|\__,_|_| |_| |_|\___||___/ *
//*                                   *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_ELFSECTIONNAMES_H
#define RLD_ELFSECTIONNAMES_H

#include "rld/LayoutBuilder.h"

namespace rld {
namespace elf {

SectionIndexedArray<uint64_t> buildSectionNameStringTable(Layout *const Lout);

uint8_t *sectionNameTableWriter(uint8_t *Data, const Layout &Lout);

} // end namespace elf
} // end namespace rld

#endif // RLD_ELFSECTIONNAMES_H
