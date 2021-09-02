#
# Makefile for building LLVM runtime libraries using musl-prepo libc (requires GNU make).
# LLVM runtime libraries include the unwind library, compiler-rt, c++ ABI library
# and libc++ library.
# prerequisite: 1) installed the musl-prepo libc library in the $(MUSL) directory.
#               2) built llvm-project-prepo in the $(LLVM_BUILD) directory. 
#

# The path of the directory in which llvm-project-prepo sources is checked out from git.
LLVM_REPO ?= ${HOME}/github/llvm-project-prepo
# Choose the toolchina file to be used. By default, repo.cmake is used.
ifdef FULL_REPO
	TOOLCHAIN_FILE=$(LLVM_REPO)/llvm/utils/repo/full_repo.cmake
else
	TOOLCHAIN_FILE=$(LLVM_REPO)/llvm/utils/repo/repo.cmake
endif
# The path of the llvm-project-prepo build directory.
LLVM_BUILD = ${LLVM_REPO}/build
# The path of the directory in which llvm libraries (except libcompiler-rt.a) are installed.
LLVM = ${HOME}/LLVM
# The LLVM version. Currently, the llvm-project-prepo repository is based on LLVM 11.0.0,
# therefore the value is set to 11.0.0. Once the repository is updated to the later LLVM
# version, this value needs to be updated correspondingly.
LLVM_VERSION = 11.0.0
# The path of the directory in which compiler-rt project is installed.
CLANG_RT = $(LLVM)/lib/clang/$(LLVM_VERSION)
# The path of the directory in which libclang_rt.a, crtBegin.o, crtEnd.o and libclang_rt.db are installed.
CLANG_RT_LIB = $(CLANG_RT)/lib/linux
# The path of the directory in which musl libc is installed.
MUSL = ${HOME}/musl

VPATH = $(LLVM_REPO)/build_libunwind/lib/:\
        $(LLVM_REPO)/build_libcompiler-rt/lib/linux/:\
        $(LLVM_REPO)/build_libcxxabi/lib/:\
        $(LLVM_REPO)/build_libcxx/lib/:\
        $(CLANG_RT_LIB)/

ALL_LIBS = $(LLVM_REPO)/build_libunwind/lib/libunwind.a  \
           $(LLVM_REPO)/build_libunwind/lib/libunwind.db \
           $(LLVM_REPO)/build_libcompiler-rt/lib/linux/libclang_rt.a \
           $(LLVM_REPO)/build_libcompiler-rt/lib/linux/libclang_rt.db \
           $(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.t \
           $(CLANG_RT_LIB)/clang_rt.crtend-x86_64.t \
           $(LLVM_REPO)/build_libcxxabi/lib/libc++abi.a \
           $(LLVM_REPO)/build_libcxxabi/lib/libc++abi.db \
           $(LLVM_REPO)/build_libcxx/lib/libc++.a \
           $(LLVM_REPO)/build_libcxx/lib/libc++.db

.ONESHELL:
$(LLVM_REPO)/build_libunwind/lib/%.a \
$(LLVM_REPO)/build_libunwind/lib/%.db:
	-rm -rf $(LLVM_REPO)/build_libunwind
	cd $(LLVM_REPO)
	mkdir build_libunwind && cd build_libunwind
	mkdir lib
	export REPOFILE=$(LLVM_REPO)/build_libunwind/lib/libunwind.db
	cmake -D musl=Yes                                                     \
		-D CMAKE_BUILD_TYPE=Release                                   \
		-D LLVM_PATH=../                                              \
		-D LIBUNWIND_ENABLE_SHARED=No                                 \
		-D LIBUNWIND_ENABLE_STATIC=Yes                                \
		-D LLVM_ENABLE_UNWIND_TABLES=OFF                              \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                     \
		-D CMAKE_CXX_COMPILER=$(LLVM_BUILD)/bin/clang++               \
		-D CMAKE_C_COMPILER=$(LLVM_BUILD)/bin/clang                   \
		-D utils_dir=$(LLVM_REPO)/llvm/utils/repo                     \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                               \
		-D musl_install=$(MUSL)                                       \
		../libunwind
	make -j 8
	make install

.ONESHELL:
$(LLVM_REPO)/build_libcompiler-rt/lib/linux/lib%.a \
$(LLVM_REPO)/build_libcompiler-rt/lib/linux/lib%.db \
$(CLANG_RT_LIB)/%.crtbegin-x86_64.t \
$(CLANG_RT_LIB)/%.crtend-x86_64.t:
	-rm -rf $(LLVM_REPO)/build_libcompiler-rt
	cd $(LLVM_REPO)
	mkdir build_libcompiler-rt && cd build_libcompiler-rt
	mkdir -p lib/linux
	export REPOFILE=$(LLVM_REPO)/build_libcompiler-rt/lib/linux/libclang_rt.db
	cmake  -D musl=Yes                                                    \
		-D CMAKE_BUILD_TYPE=Release                                   \
		-D LLVM_CONFIG_PATH=$(LLVM_BUILD)/bin/llvm-config             \
		-D COMPILER_RT_DEFAULT_TARGET_ONLY=ON                         \
		-D COMPILER_RT_BUILD_BUILTINS=ON                              \
		-D COMPILER_RT_BUILD_SANITIZERS=OFF                           \
		-D COMPILER_RT_BUILD_XRAY=OFF                                 \
		-D COMPILER_RT_BUILD_LIBFUZZER=OFF                            \
		-D COMPILER_RT_BUILD_PROFILE=OFF                              \
		-D LLVM_ENABLE_UNWIND_TABLES=OFF                              \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                     \
		-D CMAKE_CXX_COMPILER=$(LLVM_BUILD)/bin/clang++               \
		-D CMAKE_C_COMPILER=$(LLVM_BUILD)/bin/clang                   \
		-D utils_dir=$(LLVM_REPO)/llvm/utils/repo                     \
		-D CMAKE_INSTALL_PREFIX=$(CLANG_RT)                           \
		-D musl_install=$(MUSL)                                       \
		../compiler-rt
	make -j 8
	make install
	mv $(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.o $(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.t
	mv $(CLANG_RT_LIB)/clang_rt.crtend-x86_64.o $(CLANG_RT_LIB)/clang_rt.crtend-x86_64.t

$(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.o: clang_rt.crtbegin-x86_64.t
	repo2obj --repo=$(LLVM_REPO)/build_libcompiler-rt/lib/linux/libclang_rt.db $< -o $@

$(CLANG_RT_LIB)/clang_rt.crtend-x86_64.o: clang_rt.crtend-x86_64.t
	repo2obj --repo=$(LLVM_REPO)/build_libcompiler-rt/lib/linux/libclang_rt.db $< -o $@

.ONESHELL:
$(LLVM_REPO)/build_libcxxabi/lib/%.a \
$(LLVM_REPO)/build_libcxxabi/lib/%.db: libunwind.a libclang_rt.a
	-rm -rf $(LLVM_REPO)/build_libcxxabi
	cd $(LLVM_REPO)
	mkdir build_libcxxabi && cd build_libcxxabi
	mkdir -p lib
	export REPOFILE=$(LLVM_REPO)/build_libcxxabi/lib/libc++abi.db
	cmake -D musl=Yes                                                           \
		-D CMAKE_BUILD_TYPE=Release                                         \
		-D libcxxabi_include=-I$(LLVM_BUILD)/lib/clang/11.0.0/include/      \
		-D LIBCXXABI_ENABLE_EXCEPTIONS=OFF                                  \
		-D LLVM_PATH=../                                                    \
		-D LIBCXXABI_USE_COMPILER_RT=YES                                    \
		-D LIBCXXABI_ENABLE_SHARED=No                                       \
		-D LIBCXXABI_ENABLE_STATIC=Yes                                      \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                           \
		-D CMAKE_CXX_COMPILER=$(LLVM_BUILD)/bin/clang++                     \
		-D CMAKE_C_COMPILER=$(LLVM_BUILD)/bin/clang                         \
		-D LLVM_ENABLE_UNWIND_TABLES=OFF                                    \
		-D utils_dir=$(LLVM_REPO)/llvm/utils/repo                           \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                                     \
		-D musl_install=$(MUSL)                                             \
		../libcxxabi
	make -j 8
	make install

.ONESHELL:
$(LLVM_REPO)/build_libcxx/lib/%.a \
$(LLVM_REPO)/build_libcxx/lib/%.db: libc++abi.a
	-rm -rf $(LLVM_REPO)/build_libcxx
	cd $(LLVM_REPO)
	mkdir build_libcxx && cd build_libcxx
	mkdir -p lib
	export REPOFILE=$(LLVM_REPO)/build_libcxx/lib/libc++.db
	cmake -D musl=Yes                                                     \
		-D CMAKE_BUILD_TYPE=Release                                   \
		-D LIBCXX_ENABLE_SHARED=No                                    \
		-D LIBCXX_ENABLE_STATIC=Yes                                   \
		-D LIBCXX_CXX_ABI=libcxxabi                                   \
		-D LIBCXX_CXX_ABI_INCLUDE_PATHS=../libcxxabi/include          \
		-D LIBCXX_HAS_MUSL_LIBC=ON                                    \
		-D LLVM_ENABLE_UNWIND_TABLES=OFF                              \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                     \
		-D CMAKE_CXX_COMPILER=$(LLVM_BUILD)/bin/clang++               \
		-D CMAKE_C_COMPILER=$(LLVM_BUILD)/bin/clang                   \
		-D utils_dir=$(LLVM_REPO)/llvm/utils/repo                     \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                               \
		-D musl_install=$(MUSL)                                       \
		../libcxx
	make -j 8
	make install

$(LLVM)/lib/libunwind.db: libunwind.db
	cp $< $@

$(CLANG_RT_LIB)/libclang_rt.db: libclang_rt.db
	cp $< $@

$(LLVM)/lib/libc++abi.db: libc++abi.db
	cp $< $@

$(LLVM)/lib/libc++.db: libc++.db
	cp $< $@

install-libs: $(ALL_LIBS) \
              $(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.o \
              $(CLANG_RT_LIB)/clang_rt.crtend-x86_64.o

install-repo-libs-dbs: $(ALL_LIBS) \
                       $(LLVM)/lib/libunwind.db \
                       $(CLANG_RT_LIB)/libclang_rt.db \
                       $(LLVM)/lib/libc++abi.db \
                       $(LLVM)/lib/libc++.db \
                       clang_rt.crtbegin-x86_64.t \
                       clang_rt.crtend-x86_64.t
	mv $(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.t $(CLANG_RT_LIB)/clang_rt.crtbegin-x86_64.o
	mv $(CLANG_RT_LIB)/clang_rt.crtend-x86_64.t $(CLANG_RT_LIB)/clang_rt.crtend-x86_64.o

.PHONY: install
install:
ifdef FULL_REPO
	$(MAKE) install-repo-libs-dbs
else
	$(MAKE) install-libs
endif

.PHONY: clean
clean:
	-rm -rf "$(LLVM_REPO)/build_libunwind"                \
                "$(LLVM_REPO)/build_libcompiler-rt"   \
                "$(LLVM_REPO)/build_libcxxabi"        \
                "$(LLVM_REPO)/build_libcxx"
