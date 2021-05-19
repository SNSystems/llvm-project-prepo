#
# Makefile for building LLVM runtime libraries using musl-prepo libc (requires GNU make).
# LLVM runtime libraries include the unwind library, compiler-rt, c++ ABI library
# and libc++ library.
# prerequisite: 1) installed the musl-prepo libc library in the $(MUSL) directory.
#               2) built llvm-project-prepo in the $(LLVM_BUILD) directory. 
#

# The path of the directory in which llvm-project-prepo sources is checked out from git.
LLVM_REPO = ${HOME}/llvm-project-prepo
TOOLCHAIN_FILE=$(LLVM_REPO)/llvm/utils/repo/repo.cmake
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
	cmake -D musl=Yes                                                     \
		-D CMAKE_BUILD_TYPE=Release                                   \
		-D LLVM_PATH=../                                              \
		-D LIBUNWIND_ENABLE_SHARED=No                                 \
		-D LIBUNWIND_ENABLE_STATIC=Yes                                \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                     \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                               \
		../libunwind &&                                               \
	make -j 8 &&                                                          \
	make install

libcompiler-rt.a:
	if [ -d "$(LLVM_REPO)/build_libcompiler-rt" ];then                    \
		rm -rf $(LLVM_REPO)/build_libcompiler-rt;                     \
	fi
	cd $(LLVM_REPO)  &&                                                   \
	mkdir build_libcompiler-rt && cd build_libcompiler-rt  &&             \
	cmake  -D musl=Yes                                                    \
		-D CMAKE_BUILD_TYPE=Release                                   \
		-D LLVM_CONFIG_PATH=$(LLVM_BUILD)/bin/llvm-config             \
		-D COMPILER_RT_DEFAULT_TARGET_ONLY=ON                         \
		-D COMPILER_RT_BUILD_BUILTINS=ON                              \
		-D COMPILER_RT_BUILD_SANITIZERS=OFF                           \
		-D COMPILER_RT_BUILD_XRAY=OFF                                 \
		-D COMPILER_RT_BUILD_LIBFUZZER=OFF                            \
		-D COMPILER_RT_BUILD_PROFILE=OFF                              \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                     \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                               \
		../compiler-rt &&                                             \
	make -j 8 &&                                                          \
	make install

$(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o: libcompiler-rt.a
$(LLVM)/lib/linux/clang_rt.crtend-x86_64.o: libcompiler-rt.a

$(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o.elf: $(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o
	repo2obj --repo=$(LLVM_REPO)/build_libcompiler-rt/lib/crt/clang.db $< -o $@

$(LLVM)/lib/linux/clang_rt.crtend-x86_64.o.elf: $(LLVM)/lib/linux/clang_rt.crtend-x86_64.o
	repo2obj --repo=$(LLVM_REPO)/build_libcompiler-rt/lib/crt/clang.db $< -o $@

libcxxabi.a: libunwind.a libcompiler-rt.a
	if [ -d "$(LLVM_REPO)/build_libcxxabi" ];then                               \
		rm -rf $(LLVM_REPO)/build_libcxxabi;                                \
	fi
	cd $(LLVM_REPO) &&                                                          \
	mkdir build_libcxxabi && cd build_libcxxabi  &&                             \
	cmake -D musl=Yes                                                           \
		-D CMAKE_BUILD_TYPE=Release                                         \
		-D libcxxabi_include=-I$(LLVM_BUILD)/lib/clang/11.0.0/include/      \
		-D LIBCXXABI_ENABLE_EXCEPTIONS=OFF                                  \
		-D LLVM_PATH=../                                                    \
		-D LIBCXXABI_USE_COMPILER_RT=YES                                    \
		-D LIBCXXABI_ENABLE_SHARED=No                                       \
		-D LIBCXXABI_ENABLE_STATIC=Yes                                      \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                           \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                                     \
		../libcxxabi &&                                                     \
	make -j 8 &&                                                                \
	make install

libcxx.a: libcxxabi.a
	if [ -d "$(LLVM_REPO)/build_libcxx" ];then                            \
		rm -rf $(LLVM_REPO)/build_libcxx;                             \
	fi
	cd $(LLVM_REPO)  &&                                                   \
	mkdir build_libcxx && cd build_libcxx  &&                             \
	cmake -D musl=Yes  -D libcxx=Yes                                      \
		-D CMAKE_BUILD_TYPE=Release                                   \
		-D LIBCXX_ENABLE_SHARED=No                                    \
		-D LIBCXX_ENABLE_STATIC=Yes                                   \
		-D LIBCXX_CXX_ABI=libcxxabi                                   \
		-D LIBCXX_CXX_ABI_INCLUDE_PATHS=../libcxxabi/include          \
		-D LIBCXX_HAS_MUSL_LIBC=ON                                    \
		-D CMAKE_TOOLCHAIN_FILE=$(TOOLCHAIN_FILE)                     \
		-D CMAKE_INSTALL_PREFIX=$(LLVM)                               \
		../libcxx &&                                                  \
	make -j 8 &&                                                          \
	make install

.PHONY: all
all :
	$(MAKE) libunwind.a \
		libcompiler-rt.a \
		libcxxabi.a \
		libcxx.a \
		$(LLVM)/lib/linux/clang_rt.crtbegin-x86_64.o.elf \
		$(LLVM)/lib/linux/clang_rt.crtend-x86_64.o.elf

.PHONY: clean
clean:
	-rm -rf "$(LLVM_REPO)/build_libunwind"                \
			"$(LLVM_REPO)/build_libcompiler-rt"   \
			"$(LLVM_REPO)/build_libcxx"           \
			"$(LLVM_REPO)/build_libcxx"

.PHONY: distclean
distclean: clean
	-rm -f clang.db
