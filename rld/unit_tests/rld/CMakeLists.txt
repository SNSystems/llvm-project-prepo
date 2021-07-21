#===- unit_tests/rld/CMakeLists.txt ---------------------------------------===//
#*   ____ __  __       _        _     _     _        *
#*  / ___|  \/  | __ _| | _____| |   (_)___| |_ ___  *
#* | |   | |\/| |/ _` | |/ / _ \ |   | / __| __/ __| *
#* | |___| |  | | (_| |   <  __/ |___| \__ \ |_\__ \ *
#*  \____|_|  |_|\__,_|_|\_\___|_____|_|___/\__|___/ *
#*                                                   *
#===----------------------------------------------------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===----------------------------------------------------------------------===//
add_rld_unittest(rldTests
  CompilationBuilder.cpp
  EmptyStore.cpp
  StringAdder.cpp
  TestCSAlloc.cpp
  TestMathExtras.cpp
  TestSymbolResolver.cpp
  TestXfxResolver.cpp
)
target_link_libraries (rldTests PRIVATE LLVMrld)
