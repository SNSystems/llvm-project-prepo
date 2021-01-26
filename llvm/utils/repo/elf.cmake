#===- elf.cmake ----------------------------------------------------------===
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM
# Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===----------------------------------------------------------------------===

set (triple "x86_64-pc-linux-gnu")

SET (CMAKE_C_COMPILER   "clang"  CACHE FILEPATH "Compiler")
SET (CMAKE_CXX_COMPILER "clang++"  CACHE FILEPATH "Compiler")

SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -fno-exceptions -fno-rtti" CACHE STRING "Default C Flags Debug")
SET (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -fno-exceptions -fno-rtti" CACHE STRING "Default CXX Flags Debug")
SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fno-exceptions -fno-rtti" CACHE STRING "Default C Flags Release")
SET (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -fno-exceptions -fno-rtti" CACHE STRING "Default CXX Flags Release")
set (CMAKE_C_COMPILER_TARGET ${triple})
set (CMAKE_CXX_COMPILER_TARGET ${triple})
