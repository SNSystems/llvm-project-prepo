#
# Makefile for building LLVM runtime libraries using musl-prepo libc (requires GNU make).
# LLVM runtime libraries include the unwind library, compiler-rt, c++ ABI library
# and libc++ library.
# prerequisite: 1) installed the musl-prepo libc library in the $(MUSL) directory.
#               2) built llvm-project-prepo in the $(LLVM_BUILD) directory. 
#

# The path of the directory in which llvm-project-prepo sources is checked out from git.
LLVM_REPO = ${HOME}/llvm-project-prepo
# Choose the toolchina file to be used. By default, repo.cmake is used.
ifdef FULL_REPO
	TOOLCHAIN_FILE=$(LLVM_REPO)/llvm/utils/repo/full_repo.cmake
else
	TOOLCHAIN_FILE=$(LLVM_REPO)/llvm/utils/repo/repo.cmake
endif
# The path of the llvm-project-prepo build directory.
LLVM_BUILD = ${LLVM_REPO}/build
# The path of the directory in which llvm libraries are installed.
LLVM = ${HOME}/LLVM
# The path of the directory in which musl libc is installed.
MUSL = ${HOME}/musl

libunwind.a:
	if [ -d "$(LLVM_REPO)/build_libunwind" ];then                         \
		rm -rf $(LLVM_REPO)/build_libunwind;                          \
	fi
	cd $(LLVM_REPO)  &&                                                   \
	mkdir build_libunwind && cd build_libunwind  &&                       \
	export REPOFILE=$(LLVM_REPO)/build_libunwind/libunwind.db &&          \
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
		../libunwind &&                                               \
	make -j 8 &&                                                          \
	make install

libcompiler-rt.a:
	if [ -d "$(LLVM_REPO)/build_libcompiler-rt" ];then                    \
		rm -rf $(LLVM_REPO)/build_libcompiler-rt;                     \
	fi
	cd $(LLVM_REPO)  &&                                                   \
	mkdir build_libcompiler-rt && cd build_libcompiler-rt  &&             \
	export REPOFILE=$(LLVM_REPO)/build_libcompiler-rt/libcompiler-rt.db &&\
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
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                               \
		-D musl_install=$(MUSL)                                       \
		../compiler-rt &&                                             \
	make -j 8 &&                                                          \
	make install

$(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o: libcompiler-rt.a
$(LLVM)/lib/linux/clang_rt.crtend-x86_64.o: libcompiler-rt.a

$(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o.elf: $(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o
	repo2obj --repo=$(LLVM_REPO)/build_libcompiler-rt/libcompiler-rt.db $< -o $@

$(LLVM)/lib/linux/clang_rt.crtend-x86_64.o.elf: $(LLVM)/lib/linux/clang_rt.crtend-x86_64.o
	repo2obj --repo=$(LLVM_REPO)/build_libcompiler-rt/libcompiler-rt.db $< -o $@

libcxxabi.a: libunwind.a libcompiler-rt.a                                          \
             $(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o.elf                      \
             $(LLVM)/lib/linux/clang_rt.crtend-x86_64.o.elf
	if [ -d "$(LLVM_REPO)/build_libcxxabi" ];then                               \
		rm -rf $(LLVM_REPO)/build_libcxxabi;                                \
	fi
	cd $(LLVM_REPO) &&                                                          \
	mkdir build_libcxxabi && cd build_libcxxabi  &&                             \
	export REPOFILE=$(LLVM_REPO)/build_libcxxabi/libc++abi.db &&                \
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
		../libcxxabi &&                                                     \
	make -j 8 &&                                                                \
	make install

libcxx.a: libcxxabi.a
	if [ -d "$(LLVM_REPO)/build_libcxx" ];then                            \
		rm -rf $(LLVM_REPO)/build_libcxx;                             \
	fi
	cd $(LLVM_REPO)  &&                                                   \
	mkdir build_libcxx && cd build_libcxx  &&                             \
	export REPOFILE=$(LLVM_REPO)/build_libcxx/libc++.db &&                \
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
		../libcxx &&                                                  \
	make -j 8 &&                                                          \
	make install

$(LLVM)/lib/libunwind.db: libunwind.a
	cp $(LLVM_REPO)/build_libunwind/libunwind.db $@

$(LLVM)/lib/linux/libcompiler-rt.db: libcompiler-rt.a
	cp $(LLVM_REPO)/build_libcompiler-rt/libcompiler-rt.db $@

$(LLVM)/lib/libc++abi.db: libcxxabi.a
	cp $(LLVM_REPO)/build_libcxxabi/libc++abi.db $@

$(LLVM)/lib/libc++.db: libcxx.a
	cp $(LLVM_REPO)/build_libcxx/libc++.db $@

install-libs: libunwind.a \
	libcompiler-rt.a \
	libcxxabi.a \
	libcxx.a

install-repo-libs-dbs: $(LLVM)/lib/libunwind.db \
	$(LLVM)/lib/linux/libcompiler-rt.db \
	$(LLVM)/lib/libc++abi.db \
	$(LLVM)/lib/libc++.db

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
