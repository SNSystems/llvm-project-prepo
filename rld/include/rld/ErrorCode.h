//*  _____                      ____          _       *
//* | ____|_ __ _ __ ___  _ __ / ___|___   __| | ___  *
//* |  _| | '__| '__/ _ \| '__| |   / _ \ / _` |/ _ \ *
//* | |___| |  | | | (_) | |  | |__| (_) | (_| |  __/ *
//* |_____|_|  |_|  \___/|_|   \____\___/ \__,_|\___| *
//*                                                   *
//===- include/rld/ErrorCode.h --------------------------------------------===//
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
#ifndef RLD_ERROR_CODE_H
#define RLD_ERROR_CODE_H

#include <string>
#include <system_error>

namespace rld {

enum ErrorCode : int {
  none,
  DatabaseNotFound,
  CompilationIndexNotFound,
  FragmentIndexNotFound,
  NamesIndexNotFound,
};

class ErrorCategory : public std::error_category {
public:
  char const *name() const noexcept override;
  std::string message(int error) const override;

  static std::error_category const &get_error_category();

private:
  ErrorCategory() = default;
};

inline std::error_code make_error_code(ErrorCode const e) noexcept {
  return {static_cast<int>(e), ErrorCategory::get_error_category()};
}

} // end namespace rld

namespace std {

template <> struct is_error_code_enum<rld::ErrorCode> : std::true_type {};

} // end namespace std

#endif // RLD_ERROR_CODE_H
