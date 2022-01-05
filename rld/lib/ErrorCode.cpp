//===- lib/ErrorCode.cpp --------------------------------------------------===//
//*  _____                        ____          _       *
//* | ____|_ __ _ __ ___  _ __   / ___|___   __| | ___  *
//* |  _| | '__| '__/ _ \| '__| | |   / _ \ / _` |/ _ \ *
//* | |___| |  | | | (_) | |    | |__| (_) | (_| |  __/ *
//* |_____|_|  |_|  \___/|_|     \____\___/ \__,_|\___| *
//*                                                     *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "rld/ErrorCode.h"

char const *rld::ErrorCategory::name() const noexcept { return "rld category"; }

std::string rld::ErrorCategory::message(int error) const {
  auto *result = "unknown error";
  switch (error) {
  case ErrorCode::none:
    result = "no error";
    break;
  case ErrorCode::DatabaseNotFound:
    result = "The database was not found";
    break;
  case ErrorCode::CompilationIndexNotFound:
    result = "Cannot load the compilation index";
    break;
  case ErrorCode::FragmentIndexNotFound:
    result = "Cannot load the fragment index";
    break;
  case ErrorCode::NamesIndexNotFound:
    result = "Cannot load the names index";
    break;
  case ErrorCode::CompilationNotFound:
    result = "The compilation was not found";
    break;
  case ErrorCode::UndefinedSymbol:
    result = "Undefined symbol";
    break;
  case ErrorCode::UndeterminedOutputTriple:
    result = "The output triple could not be determined";
    break;
  }
  return result;
}

std::error_category const &rld::ErrorCategory::get_error_category() {
  static ErrorCategory const cat;
  return cat;
}
