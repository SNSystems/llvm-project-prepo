#*   ____ __  __       _        _     _     _        *
#*  / ___|  \/  | __ _| | _____| |   (_)___| |_ ___  *
#* | |   | |\/| |/ _` | |/ / _ \ |   | / __| __/ __| *
#* | |___| |  | | (_| |   <  __/ |___| \__ \ |_\__ \ *
#*  \____|_|  |_|\__,_|_|\_\___|_____|_|___/\__|___/ *
#*                                                   *
#===- tools/gen/CMakeLists.txt --------------------------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===----------------------------------------------------------------------===//

set (XXHASH_BUILD_ENABLE_INLINE_API OFF) #optional
set (XXHASH_BUILD_XXHSUM OFF) #optional
add_subdirectory (xxHash/cmake_unofficial EXCLUDE_FROM_ALL)

set(LLVM_LINK_COMPONENTS
  MC
  Support
)

add_llvm_tool (rld-gen
  gen.cpp
  fibonacci.hpp
  hash.h
  FragmentCreator.cpp
)
target_link_libraries (rld-gen PRIVATE pstore-core pstore-mcrepo)
target_link_libraries (rld-gen PRIVATE xxHash::xxhash)

set_target_properties (rld-gen PROPERTIES
    CXX_STANDARD 14
    CXX_STANDARD_REQUIRED Yes
)
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options (rld-gen PUBLIC
        -Wno-c++14-extensions
        -Wno-nullability-extension
    )
endif ()
