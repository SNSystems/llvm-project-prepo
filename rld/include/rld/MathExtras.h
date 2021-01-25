//*  __  __       _   _     _____      _                  *
//* |  \/  | __ _| |_| |__ | ____|_  _| |_ _ __ __ _ ___  *
//* | |\/| |/ _` | __| '_ \|  _| \ \/ / __| '__/ _` / __| *
//* | |  | | (_| | |_| | | | |___ >  <| |_| | | (_| \__ \ *
//* |_|  |_|\__,_|\__|_| |_|_____/_/\_\\__|_|  \__,_|___/ *
//*                                                       *
//===- include/rld/MathExtras.h -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_MATH_EXTRAS_H
#define RLD_MATH_EXTRAS_H

#include "llvm/Support/MathExtras.h"
#include <cassert>

namespace rld {

inline uint64_t alignTo(uint64_t Value, std::size_t Align) noexcept {
  assert(llvm::countPopulation(Align) == 1U &&
         "Alignment must be a power of 2");
  return (Value + Align - 1U) & ~(Align - 1U);
  ;
}

} // end namespace rld

#endif // RLD_MATH_EXTRAS_H
