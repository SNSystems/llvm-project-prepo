//*  _____                      ____          _       *
//* | ____|_ __ _ __ ___  _ __ / ___|___   __| | ___  *
//* |  _| | '__| '__/ _ \| '__| |   / _ \ / _` |/ _ \ *
//* | |___| |  | | | (_) | |  | |__| (_) | (_| |  __/ *
//* |_____|_|  |_|  \___/|_|   \____\___/ \__,_|\___| *
//*                                                   *
//===- lib/ErrorCode.cpp --------------------------------------------------===//
// Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
// All rights reserved.
//
// Developed by:
//   Toolchain Team
//   SN Systems, Ltd.
//   www.snsystems.com
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal with the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// - Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimers.
//
// - Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimers in the
//   documentation and/or other materials provided with the distribution.
//
// - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
//   Inc. nor the names of its contributors may be used to endorse or
//   promote products derived from this Software without specific prior
//   written permission.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
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
  }
  return result;
}

std::error_category const &rld::ErrorCategory::get_error_category() {
  static ErrorCategory const cat;
  return cat;
}
