//===- unit_tests/rld/ErrorFn.h ---------------------------*- mode: C++ -*-===//
//*  _____                       _____       *
//* | ____|_ __ _ __ ___  _ __  |  ___| __   *
//* |  _| | '__| '__/ _ \| '__| | |_ | '_ \  *
//* | |___| |  | | | (_) | |    |  _|| | | | *
//* |_____|_|  |_|  \___/|_|    |_|  |_| |_| *
//*                                          *
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_UNIT_TESTS_ERROR_FN_H
#define RLD_UNIT_TESTS_ERROR_FN_H

#include "gmock/gmock.h"

namespace rld {
class Symbol;
}

class ErrorFnInterface {
public:
  virtual ~ErrorFnInterface() = default;
  virtual void invoke(rld::Symbol const *) const = 0;
  void operator()(rld::Symbol const *Sym) const { return invoke(Sym); }
};

class ErrorFn final : public ErrorFnInterface {
public:
  MOCK_CONST_METHOD1(invoke, void(rld::Symbol const *));
};

#endif // RLD_UNIT_TESTS_ERROR_FN_H
