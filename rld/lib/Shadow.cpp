//===- lib/Shadow.cpp -----------------------------------------------------===//
//*  ____  _               _                *
//* / ___|| |__   __ _  __| | _____      __ *
//* \___ \| '_ \ / _` |/ _` |/ _ \ \ /\ / / *
//*  ___) | | | | (_| | (_| | (_) \ V  V /  *
//* |____/|_| |_|\__,_|\__,_|\___/ \_/\_/   *
//*                                         *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/Shadow.h"

#include "rld/symbol.h"

static_assert(
    alignof(rld::Symbol) > 1U,
    "The LSB of pointers is used to distinguish between CompilationRef* "
    "and Symbol*");
static_assert(
    alignof(rld::CompilationRef) > 1U,
    "The LSB of pointers is used to distinguish between CompilationRef* "
    "and Symbol*");

const rld::shadow::TaggedPointer rld::shadow::TaggedPointer::Busy{
    reinterpret_cast<void *>(std::numeric_limits<uintptr_t>::max())};
