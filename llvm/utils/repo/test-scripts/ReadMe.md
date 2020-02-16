# pstore Tests

A utility which automatically tests the built llvm-prepo toolchain using the pstore unit and system tests.

Automatically build and run toolchain and pstore tests includes:

1. clone or update the llvm-prepo, clang-prepo and pstore projects.
2. build llvm-repo/clang-repo/pstore toolchain with either Debug or Release configuration.
3. generate a pstore build with either Debug or Release configuration.
4. for each pstore unit test target except pstore-broker-unit-tests:
    1. compile the pstore unit test targeting the program repository using the above compiler.
    2. convert the Repo ticket files to ELF object files using repo2obj.
    3. link the ELF object files to generate an executable ELF file.
    4. run the unit test executable file.
    5. check and store the unit test result.
    6. repeat for the remaining unit test targets.
5. build and run pstore system test target except broker related system tests.
6. check and store pstore system test result.
7. clean the build by deleting all ticket files (retain the clang.db database).
8. Repeat step 4 to produce the result for the second time build (note: all fragments are in the database.)


## Prerequisites

- [Python](https://www.python.org) 3.4 or later

## Dependencies

The utility uses some third-party Python libraries which must be installed before it can be used:

    $ pip install gitpython
    $ pip install pytablewriter
    $ pip install networkx
    $ pip install decorator
    $ pip install pydot

## Using the tool

With the working directory, run the script with the default options to produce a pstore test
result Markdown file (pstore_test_results.md).

The test results (named pstore_test_results.md) could be used to update the Wiki page
[Test Results for pstore Unit and System Tests](https://github.com/SNSystems/llvm-prepo/wiki/Test-Results-for-pstore-Unit-and-System-Tests)

On success, the exit code is 0 and 1 if there are any errors.
