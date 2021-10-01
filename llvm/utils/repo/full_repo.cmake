#===- full_repo.cmake ---------------------------------------------------------===
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

option(libcxx "Build the project using the LLVM libc++ library." OFF)
option(musl "Build the project using the musl libc library." OFF)

# The user may specify the path of the "wrap" utilities by passing
# '-Dutils_dir:STRING=/path/to/llvm/utils/repo' on the command line.
#
# We set the '_utils_dir' environment variable to communicate this path to
# child instances of cmake (during compiler identification, for example).
if (utils_dir)
    set (ENV{_utils_dir} "${utils_dir}")
elseif (DEFINED ENV{_utils_dir})
    set (utils_dir "$ENV{_utils_dir}")
else ()
    set (utils_dir /usr/share/repo)
    set (ENV{_utils_dir} "${utils_dir}")
endif ()

# libcxxabi_include: the path of the directory containing the clang's include files.
# The user may specify the libcxxabi_include by giving
# '-Dlibcxxabi_include:STRING=/path/to/clang/include' on the command line.
if (libcxxabi_include)
	set (libcxxabi_include "${libcxxabi_include}")
endif ()

# musl_install: the path of the directory in which musl libc is installed.
# The user may specify the musl_install by giving
# '-Dmusl_install:STRING=/path/to/musl/installed/directory' on the command line.
if (musl_install)
	set (musl_install "${musl_install}")
else ()
	set (musl_install "/home/prepo/musl")
endif ()

# llvm_install: the path of the directory in which llvm libraries are installed.
# The user may specify the llvm_install by giving
# '-Dllvm_install:STRING=/path/to/llvm/installed/directory' on the command line.
if (llvm_install)
	set (llvm_install "${llvm_install}")
else ()
	set (llvm_install "/home/prepo/LLVM")
endif ()

# The user may use the LLVM libc++.a by giving
# '-Dlibcxx:BOOL=Yes' on the command line
if (libcxx)
	set (libcxx_compile_flags "-nostdinc++ -isystem ${llvm_install}/include/c++/v1 -isystem ${llvm_install}/lib/clang/11.0.0/include")
	set (libcxx_link_flags "-nostdlib++ -L ${llvm_install}/lib ${llvm_install}/lib/clang/11.0.0/liblinux/clang_rt.crtbegin-x86_64.o.elf ${llvm_install}/lib/clang/11.0.0/liblinux/clang_rt.crtend-x86_64.o.elf -lc++ -lc++abi -L ${llvm_install}/lib/linux -lclang_rt.builtins-x86_64")
endif()

# The user may use the MUSL libc.a by giving
# '-Dmusl:BOOL=Yes' on the command line
if (musl)
	SET (musl_compile_flags "-D__MUSL__ -nostdinc --sysroot ${musl_install} -isystem ${musl_install}/include ${libcxxabi_include}")
	SET (musl_crt "${musl_install}/lib/crt1.t.o ${musl_install}/lib/crt1_asm.t.o")
	SET (CMAKE_EXE_LINKER_FLAGS "-nostdlib -nodefaultlibs -static --sysroot ${musl_install} -L ${musl_install}/lib ${libcxx_link_flags} -lc_elf" CACHE STRING "toolchain_exelinkflags" FORCE)
endif()

SET (CMAKE_C_FLAGS "${musl_compile_flags} -fno-exceptions -fno-rtti -fno-unwind-tables" CACHE STRING "toolchain_cflags")
SET (CMAKE_CXX_FLAGS "${libcxx_compile_flags} ${musl_compile_flags} -fno-exceptions -fno-rtti -fno-unwind-tables" CACHE STRING "toolchain_cxxflags")

SET (CMAKE_C_FLAGS_DEBUG " -O0 " CACHE STRING "Default C Flags Debug")
SET (CMAKE_CXX_FLAGS_DEBUG " -O0 " CACHE STRING "Default CXX Flags Debug")
SET (CMAKE_C_FLAGS_RELEASE " -O3 " CACHE STRING "Default C Flags Release")
SET (CMAKE_CXX_FLAGS_RELEASE " -O3 " CACHE STRING "Default CXX Flags Release")
set (CMAKE_C_COMPILER_TARGET ${triple})
set (CMAKE_CXX_COMPILER_TARGET ${triple})

set (CMAKE_LINKER ${utils_dir}/link.py)
set (CMAKE_C_LINKER ${utils_dir}/link.py)
set (CMAKE_CXX_LINKER ${utils_dir}/link++.py)

set (CMAKE_C_LINK_EXECUTABLE "${CMAKE_C_LINKER} <FLAGS> <CMAKE_C_LINK_FLAGS> ${musl_crt} <OBJECTS> <LINK_LIBRARIES> <LINK_FLAGS> -o <TARGET>")
set (CMAKE_CXX_LINK_EXECUTABLE "${CMAKE_CXX_LINKER} <FLAGS> <CMAKE_CXX_LINK_FLAGS> ${musl_crt} <OBJECTS> <LINK_LIBRARIES> <LINK_FLAGS> -o <TARGET>")

set (CMAKE_C_CREATE_SHARED_LIBRARY   "${CMAKE_C_LINKER} <FLAGS> <CMAKE_SHARED_LIBRARY_C_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")
set (CMAKE_CXX_CREATE_SHARED_LIBRARY "${CMAKE_CXX_LINKER} <FLAGS> <CMAKE_SHARED_LIBRARY_CXX_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS> <SONAME_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")

set (CMAKE_AR "${utils_dir}/archive.py" CACHE FILEPATH "Archiver")
set (CMAKE_FIND_LIBRARY_PREFIXES "")
set (CMAKE_FIND_LIBRARY_SUFFIXES ".a")

# eof: full_repo.cmake
