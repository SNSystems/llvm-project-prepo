#===- repo.cmake ---------------------------------------------------------===
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM
# Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===----------------------------------------------------------------------===

SET (CMAKE_SYSTEM_VERSION 1)
SET (CMAKE_SYSTEM_PROCESSOR "x86_64")

set (triple "x86_64-pc-linux-gnu-repo")

SET (CMAKE_C_COMPILER   "clang"  CACHE FILEPATH "Compiler")
SET (CMAKE_CXX_COMPILER "clang++"  CACHE FILEPATH "Compiler")

# The user must specify the utils_dir by giving
# '-Dutils_dir:STRING=/path/to/llvm/utils/repo' on the command line
if (utils_dir)
    # Environment variables are always preserved.
    set(ENV{_utils_dir} "${utils_dir}")
else ()
    set(utils_dir "$ENV{_utils_dir}")
endif ()

SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -fno-exceptions -fno-rtti" CACHE STRING "Default C Flags Debug")
SET (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -fno-exceptions -fno-rtti" CACHE STRING "Default CXX Flags Debug")
SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -fno-exceptions -fno-rtti" CACHE STRING "Default C Flags Release")
SET (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -fno-exceptions -fno-rtti" CACHE STRING "Default CXX Flags Release")
set (CMAKE_C_COMPILER_TARGET ${triple})
set (CMAKE_CXX_COMPILER_TARGET ${triple})

set (CMAKE_LINKER ${utils_dir}/link.py)
set (CMAKE_C_LINKER ${CMAKE_LINKER})
set (CMAKE_CXX_LINKER ${CMAKE_LINKER})

set (CMAKE_C_LINK_EXECUTABLE "${utils_dir}/link.py <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
set (CMAKE_CXX_LINK_EXECUTABLE "${utils_dir}/link.py <FLAGS> <CMAKE_CXX_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")


set (CMAKE_C_CREATE_SHARED_LIBRARY   "${utils_dir}/link.py <FLAGS> <CMAKE_SHARED_LIBRARY_C_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")
set (CMAKE_CXX_CREATE_SHARED_LIBRARY "${utils_dir}/link.py <FLAGS> <CMAKE_SHARED_LIBRARY_CXX_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")

set (CMAKE_AR "${utils_dir}/archive.py" CACHE FILEPATH "Archiver")

set (CMAKE_FIND_LIBRARY_PREFIXES "")
set (CMAKE_FIND_LIBRARY_SUFFIXES ".a")

# eof: repo.cmake
