#===- lib/CMakeLists.txt --------------------------------------------------===//
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

set (RLD_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../include)
configure_file (config.h.in "${CMAKE_CURRENT_BINARY_DIR}/include/rld/config.h" ESCAPE_QUOTES)


set (LLVM_LINK_COMPONENTS
  Demangle
  Object
  Support
)
add_llvm_library (LLVMrld
    config.h.in
    "${CMAKE_CURRENT_BINARY_DIR}/include/rld/config.h"

    Archive.cpp
    CSAlloc.cpp
    Driver.cpp
    ELFProgramHeaders.cpp
    ELFSectionHeaders.cpp
    ELFSectionNames.cpp
    ELFStringTable.cpp
    ELFSymbolTable.cpp
    ElfOutput.cpp
    ErrorCode.cpp
    GroupSet.cpp
    LayoutBuilder.cpp
    SectionKind.cpp
    Shadow.cpp
    SpecialNames.cpp
    XfxScanner.cpp
    context.cpp
    copy.cpp
    elf.cpp
    scanner.cpp
    symbol.cpp

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
if (WIN32)
  target_compile_definitions (LLVMrld PUBLIC _ENABLE_EXTENDED_ALIGNED_STORAGE)
endif (WIN32)
