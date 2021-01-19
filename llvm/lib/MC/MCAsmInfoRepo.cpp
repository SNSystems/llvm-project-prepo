//===- MCAsmInfoRepo.cpp - Repo asm properties ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines target asm properties related what form asm statements
// should take in general on ELF-based targets
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCAsmInfoRepo.h"
using namespace llvm;

void MCAsmInfoRepo::anchor() {}

MCAsmInfoRepo::MCAsmInfoRepo() {
  // Repo uses the MM_None mangling mode type.
  HasIdentDirective = false;
  WeakRefDirective = "\t.weak\t";
  PrivateGlobalPrefix = "";
  PrivateLabelPrefix = ".L";
  HasDotTypeDotSizeDirective = false;

  // Set up DWARF directives
  SupportsDebugInformation = true;
  NeedsDwarfSectionOffsetDirective = true;
}
