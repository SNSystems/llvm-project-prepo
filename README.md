# LLVM with Program Repository Support

![Commit Tests](https://github.com/SNSystems/llvm-project-prepo/workflows/Commit%20Tests/badge.svg)

This git repository contains a copy of LLVM (forked from [llvm/llvm-project@`7b556541`](https://github.com/llvm/llvm-project/commit/7b5565418f4d6e113ba805dad40d471d23bca6f6) with work-in-progress modifications to output to a Program Repository.

The changes are to add support for the program repository that was first shown at the [2016 US LLVM Developers’ meeting](https://llvm.org/devmtg/2016-11/) in the talk catchily titled “Demo of a repository for statically compiled programs”. You can relive the highs and lows by [watching it on YouTube](https://youtu.be/-pL94rqyQ6c). The early implementation demonstrated there has its [own Github repository](https://github.com/SNSystems/Toy-tools). This repository implements the same thing in LLVM to give you the anticipated build-time improvements in a C++ compiler targeting Linux. This implementation was the subject of a lightning talk at [2019 Euro LLVM Developers' meeting](https://llvm.org/devmtg/2019-04/), also [available on YouTube](https://youtu.be/mlQyEBDnDJE). Further documentation can be found on the [project wiki](https://github.com/SNSystems/llvm-project-prepo/wiki).

## Building the Compiler

The process to follow is similar to that for a conventional build of Clang+LLVM, but with an extra step to get the [pstore](https://github.com/SNSystems/pstore) back-end.

1. Clone [llvm-project-prepo](https://github.com/SNSystems/llvm-project-prepo.git) (this repository):

    ~~~bash
    git clone https://github.com/SNSystems/llvm-project-prepo.git
    ~~~

1. Clone [pstore](https://github.com/SNSystems/pstore):

    ~~~bash
    cd llvm-project-prepo
    git clone https://github.com/SNSystems/pstore.git
    cd -
    ~~~

   Ultimately, we envisage supporting multiple database back-ends to fit different needs, but there’s currently a hard dependency on the pstore (“Program Store”) key/value store as a back-end.


1. Build LLVM as [normal](https://llvm.org/docs/CMake.html) enabling the clang and pstore subprojects (e.g.):

   ~~~bash
   mkdir build && cd build
   cmake -G "Ninja" -DLLVM_ENABLE_PROJECTS="clang;pstore" -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_TOOL_CLANG_TOOLS_EXTRA_BUILD=OFF ../llvm
   ninja
   ~~~

## Using the Program Repository

### Compiling
The program-repository is implemented as a new object-file format (“repo”) in LLVM. To use it, you need to request it explicitly in the target triple:

~~~bash
clang -target x86_64-pc-linux-gnu-repo -c -o test.o test.c
~~~

Note that this is the only triple that we’re currently supporting (i.e. targeting X86-64 Linux).

Furthermore, the path to the program-repository database itself is set using an environment variable `REPOFILE`; it that variable is not set, it defaults to `./clang.db` (eventually, you’d expect to be able to specify this path with a command-line switch).

The command-line above will write the object code for `test.c` to the program-repository and emit a “ticket file” `test.o`. This tiny file contains a key to the real data in the database.

### Linking
A program-repository aware linker is very much on the project’s [“TODO” list](wiki/Limitations#missing-features). Until that happens, there's a `repo2obj` tool in the project tree. This generates a traditional ELF file from a repository ticket file. Using it is simple:

~~~bash
clang -target x86_64-pc-linux-gnu-repo -c -o test.o test.c
repo2obj test.o -o test.o.elf
~~~

The first step compiles the source file `test.c` to the repository, the second will produce a file `test.o.elf` which can be fed to a traditional ELF linker.

~~~bash
clang -o test test.o.elf
./test
~~~

# The LLVM Compiler Infrastructure

This directory and its sub-directories contain source code for LLVM,
a toolkit for the construction of highly optimized compilers,
optimizers, and run-time environments.

The README briefly describes how to get started with building LLVM.
For more information on how to contribute to the LLVM project, please
take a look at the
[Contributing to LLVM](https://llvm.org/docs/Contributing.html) guide.

## Getting Started with the LLVM System

Taken from https://llvm.org/docs/GettingStarted.html.

### Overview

Welcome to the LLVM project!

The LLVM project has multiple components. The core of the project is
itself called "LLVM". This contains all of the tools, libraries, and header
files needed to process intermediate representations and converts it into
object files.  Tools include an assembler, disassembler, bitcode analyzer, and
bitcode optimizer.  It also contains basic regression tests.

C-like languages use the [Clang](http://clang.llvm.org/) front end.  This
component compiles C, C++, Objective-C, and Objective-C++ code into LLVM bitcode
-- and from there into object files, using LLVM.

Other components include:
the [libc++ C++ standard library](https://libcxx.llvm.org),
the [LLD linker](https://lld.llvm.org), and more.

### Getting the Source Code and Building LLVM

The LLVM Getting Started documentation may be out of date.  The [Clang
Getting Started](http://clang.llvm.org/get_started.html) page might have more
accurate information.

This is an example work-flow and configuration to get and build the LLVM source:

1. Checkout LLVM (including related sub-projects like Clang):

     * ``git clone https://github.com/llvm/llvm-project.git``

     * Or, on windows, ``git clone --config core.autocrlf=false
    https://github.com/llvm/llvm-project.git``

2. Configure and build LLVM and Clang:

     * ``cd llvm-project``

     * ``mkdir build``

     * ``cd build``

     * ``cmake -G <generator> [options] ../llvm``

        Some common build system generators are:

        * ``Ninja`` --- for generating [Ninja](https://ninja-build.org)
          build files. Most llvm developers use Ninja.
        * ``Unix Makefiles`` --- for generating make-compatible parallel makefiles.
        * ``Visual Studio`` --- for generating Visual Studio projects and
          solutions.
        * ``Xcode`` --- for generating Xcode projects.

        Some Common options:

        * ``-DLLVM_ENABLE_PROJECTS='...'`` --- semicolon-separated list of the LLVM
          sub-projects you'd like to additionally build. Can include any of: clang,
          clang-tools-extra, libcxx, libcxxabi, libunwind, lldb, compiler-rt, lld,
          polly, or debuginfo-tests.

          For example, to build LLVM, Clang, libcxx, and libcxxabi, use
          ``-DLLVM_ENABLE_PROJECTS="clang;libcxx;libcxxabi"``.

        * ``-DCMAKE_INSTALL_PREFIX=directory`` --- Specify for *directory* the full
          path name of where you want the LLVM tools and libraries to be installed
          (default ``/usr/local``).

        * ``-DCMAKE_BUILD_TYPE=type`` --- Valid options for *type* are Debug,
          Release, RelWithDebInfo, and MinSizeRel. Default is Debug.

        * ``-DLLVM_ENABLE_ASSERTIONS=On`` --- Compile with assertion checks enabled
          (default is Yes for Debug builds, No for all other build types).

      * ``cmake --build . [-- [options] <target>]`` or your build system specified above
        directly.

        * The default target (i.e. ``ninja`` or ``make``) will build all of LLVM.

        * The ``check-all`` target (i.e. ``ninja check-all``) will run the
          regression tests to ensure everything is in working order.

        * CMake will generate targets for each tool and library, and most
          LLVM sub-projects generate their own ``check-<project>`` target.

        * Running a serial build will be **slow**. To improve speed, try running a
          parallel build.  That's done by default in Ninja; for ``make``, use the option
          ``-j NNN``, where ``NNN`` is the number of parallel jobs, e.g. the number of
          CPUs you have.

      * For more information see [CMake](https://llvm.org/docs/CMake.html)

Consult the
[Getting Started with LLVM](https://llvm.org/docs/GettingStarted.html#getting-started-with-llvm)
page for detailed information on configuring and compiling LLVM. You can visit
[Directory Layout](https://llvm.org/docs/GettingStarted.html#directory-layout)
to learn about the layout of the source code tree.
