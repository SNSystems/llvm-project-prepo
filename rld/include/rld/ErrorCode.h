//===- include/rld/ErrorCode.h ----------------------------*- mode: C++ -*-===//
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
