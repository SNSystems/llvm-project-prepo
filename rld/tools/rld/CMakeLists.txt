#*   ____ __  __       _        _     _     _        *
#*  / ___|  \/  | __ _| | _____| |   (_)___| |_ ___  *
#* | |   | |\/| |/ _` | |/ / _ \ |   | / __| __/ __| *
#* | |___| |  | | (_| |   <  __/ |___| \__ \ |_\__ \ *
#*  \____|_|  |_|\__,_|_|\_\___|_____|_|___/\__|___/ *
#*                                                   *
#===- tools/rld/CMakeLists.txt --------------------------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===----------------------------------------------------------------------===//
add_llvm_tool (rld
    rld.cpp
)
target_link_libraries (rld PRIVATE LLVMrld)

# This isn't typical of LLVM projects, but I want to be certain that all of
# the rld library's unit tests pass before the executable is linked.
add_custom_command (TARGET rld
    PRE_LINK
    COMMAND rldTests
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Running rldTest"
    DEPENDS rldTests
    VERBATIM
)
