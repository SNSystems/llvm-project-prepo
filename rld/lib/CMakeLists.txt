#*   ____ __  __       _        _     _     _        *
#*  / ___|  \/  | __ _| | _____| |   (_)___| |_ ___  *
#* | |   | |\/| |/ _` | |/ / _ \ |   | / __| __/ __| *
#* | |___| |  | | (_| |   <  __/ |___| \__ \ |_\__ \ *
#*  \____|_|  |_|\__,_|_|\_\___|_____|_|___/\__|___/ *
#*                                                   *
#===- lib/CMakeLists.txt --------------------------------------------------===//
# Copyright (c) 2017-2020 by Sony Interactive Entertainment, Inc.
# All rights reserved.
#
# Developed by:
#   Toolchain Team
#   SN Systems, Ltd.
#   www.snsystems.com
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the
# "Software"), to deal with the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# - Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimers.
#
# - Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimers in the
#   documentation and/or other materials provided with the distribution.
#
# - Neither the names of SN Systems Ltd., Sony Interactive Entertainment,
#   Inc. nor the names of its contributors may be used to endorse or
#   promote products derived from this Software without specific prior
#   written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
# ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
#===----------------------------------------------------------------------===//

set (RLD_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../include)
configure_file (config.h.in "${CMAKE_CURRENT_BINARY_DIR}/include/rld/config.h" ESCAPE_QUOTES)


set (LLVM_LINK_COMPONENTS
  Object
  Support
)
add_llvm_library (LLVMrld
    config.h.in
    "${CMAKE_CURRENT_BINARY_DIR}/include/rld/config.h"

    elf.cpp
    context.cpp
    ErrorCode.cpp
    Identify.cpp
    IdentifyPass.cpp
    LayoutBuilder.cpp
    scanner.cpp
    symbol.cpp
    XfxScanner.cpp

    ADDITIONAL_HEADER_DIRS
    "${RLD_INCLUDE_DIR}/rld"
)
target_link_libraries (LLVMrld PUBLIC pstore-core pstore-mcrepo)
target_include_directories (LLVMrld PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>"
    "$<INSTALL_INTERFACE:include>"
)
target_include_directories (LLVMrld PUBLIC  
    "$<BUILD_INTERFACE:${RLD_INCLUDE_DIR}>"
    "$<INSTALL_INTERFACE:include>"
)
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options (LLVMrld PUBLIC
        -Wno-c++14-extensions
        -Wno-nullability-extension
    )
endif ()
