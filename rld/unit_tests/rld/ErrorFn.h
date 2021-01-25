//*  _____                     _____       *
//* | ____|_ __ _ __ ___  _ __|  ___| __   *
//* |  _| | '__| '__/ _ \| '__| |_ | '_ \  *
//* | |___| |  | | | (_) | |  |  _|| | | | *
//* |_____|_|  |_|  \___/|_|  |_|  |_| |_| *
//*                                        *
//===- unit_tests/rld/ErrorFn.h -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef RLD_UNIT_TESTS_ERROR_FN_H
#define RLD_UNIT_TESTS_ERROR_FN_H

#include "pstore/core/address.hpp"
#include "gmock/gmock.h"

class ErrorFnInterface {
public:
  using StringAddress = pstore::typed_address<pstore::indirect_string>;

  virtual ~ErrorFnInterface() = default;
  virtual void invoke(StringAddress) const = 0;
  void operator()(StringAddress Addr) const { return invoke(Addr); }
};

class ErrorFn final : public ErrorFnInterface {
public:
  MOCK_CONST_METHOD1(invoke, void(StringAddress));
};

#endif // RLD_UNIT_TESTS_ERROR_FN_H
