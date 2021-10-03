#===- llvm/utils/repo/docker/pstore/Dockerfile ----------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//

# Stage 1. Check out LLVM and pstore source code and run the build.
FROM ubuntu:20.04 as builder
LABEL maintainer "paulhuggett@mac.com"

# Install LLVM's build dependencies.

# First, Update the apt's source list and include the sources of the packages.
RUN grep deb /etc/apt/sources.list | \
    sed 's/^deb/deb-src /g' >> /etc/apt/sources.list

# Install compiler, python, ninja and cmake.
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC \
    apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC \
    apt-get install -y --no-install-recommends \
            apt-utils       \
            build-essential \
            ca-certificates \
            cmake           \
            git             \
            ninja-build     \
            python

RUN rm -rf /var/lib/apt/lists/*

# Miscellaneous directories.
ARG install_dir="/tmp/install"
ARG scripts_dir="/tmp/scripts"
ARG user_dir="/usr"
ARG workspace_dir="/tmp/workspace"

# Add scripts to create the image.
ADD scripts ${scripts_dir}

# Add local repository. It will be empty, when using remote repository.
RUN echo "Adding local repository."
ADD repository ${workspace_dir}/src
RUN ls -l ${workspace_dir}/src

# Checkout the source code.
ARG checkout_args
RUN echo "checkout_args: " ${checkout_args}
RUN ${scripts_dir}/checkout_llvm_prepo.sh \
    --workspace ${workspace_dir} \
    ${checkout_args}

# Run the build. Results of the build will be available at install directory.
ARG build_args
RUN echo "build_args: " ${build_args}
RUN ${scripts_dir}/build_install_llvm_prepo.sh \
    --workspace ${workspace_dir} \
    --install-dir ${install_dir} \
    ${build_args}

RUN ninja -C ${workspace_dir}/build llvm-config

# Generate the prepo user's password
RUN apt-get install -y openssl
RUN openssl passwd -1 prepo > "${workspace_dir}/prepo_pwd.txt"



RUN echo "{ \"ar\": \"/usr/bin/ar\", \"link\": \"${install_dir}/bin/clang\", \"link++\": \"${install_dir}/bin/clang++\", \"repo2obj\": \"${install_dir}/bin/repo2obj\" }" > ${workspace_dir}/repo.json


#############
# musl libc #
#############

ARG musl_install_dir="/tmp/muslinstall"
ARG musl_workspace=${workspace_dir}/musl
RUN git clone --depth 1 --progress https://github.com/SNSystems/musl-prepo ${musl_workspace}
ENV CC=${install_dir}/bin/clang
ENV PATH=${install_dir}/bin:${PATH}
RUN cd ${musl_workspace}; ./configure --disable-shared --prefix=${musl_install_dir}
RUN rm -f ${musl_workspace}/clang.db
# TODO: remove install-repo2obj-libs.
RUN make -C ${musl_workspace} -j4 install-headers install-ticket-libs install-repo2obj-libs CC=${install_dir}/bin/clang  utils_dir=${workspace_dir}/src/llvm/utils/repo



#############
# libunwind #
#############

ARG stdlib_db="${install_dir}/lib/stdlib.db"
ARG cmake_db="${workspace_dir}/cmake.db"

RUN REPOFILE=${cmake_db} cmake       \
  -S ${workspace_dir}/src/libunwind                \
  -B ${workspace_dir}/libbuild/unwind              \
  -D LIBUNWIND_ENABLE_SHARED=No                    \
  -D LIBUNWIND_ENABLE_STATIC=Yes                   \
\
  -G Ninja \
  -D CMAKE_BUILD_TYPE=Debug                        \
  -D CMAKE_CXX_COMPILER=${install_dir}/bin/clang++ \
  -D CMAKE_C_COMPILER=${install_dir}/bin/clang     \
  -D CMAKE_INSTALL_PREFIX=${install_dir}           \
  -D CMAKE_TOOLCHAIN_FILE=${workspace_dir}/src/llvm/utils/repo/full_repo.cmake \
  -D LLVM_CONFIG_PATH=${workspace_dir}/build/bin/llvm-config \
  -D LLVM_ENABLE_LIBCXX=Yes                        \
  -D LLVM_PATH=${workspace_dir}/src/llvm           \
  -D musl=Yes                                      \
  -D musl_install=${musl_install_dir}              \
  -D utils_dir=${workspace_dir}/src/llvm/utils/repo

RUN REPOFILE=${stdlib_db} ninja -C ${workspace_dir}/libbuild/unwind install


##################
# libcompiler-rt #
##################

RUN ls -l ${workspace_dir}/build/bin

RUN REPOFILE=${cmake_db} cmake \
  -S ${workspace_dir}/src/compiler-rt        \
  -B ${workspace_dir}/libbuild/compiler-rt      \
  -D COMPILER_RT_DEFAULT_TARGET_ONLY=Yes     \
  -D COMPILER_RT_BUILD_BUILTINS=Yes          \
  -D COMPILER_RT_BUILD_SANITIZERS=No         \
  -D COMPILER_RT_BUILD_XRAY=No               \
  -D COMPILER_RT_BUILD_LIBFUZZER=No          \
  -D COMPILER_RT_BUILD_PROFILE=No            \
\
  -G Ninja \
  -D CMAKE_BUILD_TYPE=Debug                        \
  -D CMAKE_CXX_COMPILER=${install_dir}/bin/clang++ \
  -D CMAKE_C_COMPILER=${install_dir}/bin/clang     \
  -D CMAKE_INSTALL_PREFIX=${install_dir}           \
  -D CMAKE_TOOLCHAIN_FILE=${workspace_dir}/src/llvm/utils/repo/full_repo.cmake \
  -D LLVM_CONFIG_PATH=${workspace_dir}/build/bin/llvm-config \
  -D LLVM_PATH=${workspace_dir}/src/llvm           \
  -D musl=Yes                                      \
  -D musl_install=${musl_install_dir}              \
  -D utils_dir=${workspace_dir}/src/llvm/utils/repo

RUN REPOFILE=${stdlib_db} ninja -C ${workspace_dir}/libbuild/compiler-rt install


#############
# libcxxabi #
#############

RUN REPOFILE=${cmake_db} cmake \
  -S ${workspace_dir}/src/libcxxabi \
  -B ${workspace_dir}/libbuild/cxxabi \
  -D libcxxabi_include=-I${workspace_dir}/build/lib/clang/11.0.0/include/ \
  -D LIBCXXABI_ENABLE_EXCEPTIONS=No \
  -D LIBCXXABI_USE_COMPILER_RT=Yes                                    \
  -D LIBCXXABI_ENABLE_SHARED=No                                       \
  -D LIBCXXABI_ENABLE_STATIC=Yes                                      \
  -D LLVM_ENABLE_LIBCXX=Yes                        \
  \
  -G Ninja \
  -D CMAKE_BUILD_TYPE=Debug                        \
  -D CMAKE_CXX_COMPILER=${install_dir}/bin/clang++ \
  -D CMAKE_C_COMPILER=${install_dir}/bin/clang     \
  -D CMAKE_INSTALL_PREFIX=${install_dir}           \
  -D CMAKE_TOOLCHAIN_FILE=${workspace_dir}/src/llvm/utils/repo/full_repo.cmake \
  -D LLVM_CONFIG_PATH=${workspace_dir}/build/bin/llvm-config \
  -D LLVM_PATH=${workspace_dir}/src/llvm           \
  -D musl=Yes                                      \
  -D musl_install=${musl_install_dir}              \
  -D utils_dir=${workspace_dir}/src/llvm/utils/repo

RUN REPOFILE=${stdlib_db} ninja -C ${workspace_dir}/libbuild/cxxabi install


##########
# libcxx #
##########

RUN REPOFILE=${cmake_db} cmake \
  -S ${workspace_dir}/src/libcxx \
  -B ${workspace_dir}/libbuild/cxx \
  -D LIBCXX_ENABLE_SHARED=No                                    \
  -D LIBCXX_ENABLE_STATIC=Yes                                   \
  -D LIBCXX_CXX_ABI=libcxxabi                                   \
  -D LIBCXX_CXX_ABI_INCLUDE_PATHS=${workspace_dir}/src/libcxxabi/include          \
  -D LIBCXX_HAS_MUSL_LIBC=No                                   \
\
  -G Ninja \
  -D CMAKE_BUILD_TYPE=Debug                        \
  -D CMAKE_CXX_COMPILER=${install_dir}/bin/clang++ \
  -D CMAKE_C_COMPILER=${install_dir}/bin/clang     \
  -D CMAKE_INSTALL_PREFIX=${install_dir}           \
  -D CMAKE_TOOLCHAIN_FILE=${workspace_dir}/src/llvm/utils/repo/full_repo.cmake \
  -D LLVM_CONFIG_PATH=${workspace_dir}/build/bin/llvm-config \
  -D LLVM_ENABLE_LIBCXX=Yes                        \
  -D LLVM_PATH=${workspace_dir}/src/llvm           \
  -D musl=Yes                                      \
  -D musl_install=${musl_install_dir}              \
  -D utils_dir=${workspace_dir}/src/llvm/utils/repo

# FIXME: Fails at time of writing.
# RUN REPOFILE={stdlib_db} ninja -C ${workspace_dir}/libbuild/cxx install


#####################################################################################################
# Stage 2. Produce a minimal release image with build results.
FROM ubuntu:20.04 as llvm-prepo-stage
LABEL maintainer "paulhuggett@mac.com"
ENTRYPOINT /bin/bash

# Copy build results of stage 1 to /usr/local.
#COPY --from=builder ${install_dir}/ ${user_dir}/
# Unfortunately, the COPY command does not support variables.
COPY --from=builder /tmp/install/bin /usr/bin/
COPY --from=builder /tmp/install/lib /usr/lib/
COPY --from=builder /tmp/install/etc/init.d/pstore-brokerd /etc/init.d/

COPY --from=builder /tmp/muslinstall/ /usr/local/musl

# Copy the wrap tools (which can run repo2obj before passing the command to a
# traditional ELF linker or archiver).
COPY repo/wrap /usr/share/repo/

# The wrap tools configuration file.
COPY repo/repo.json /usr/share/repo/

# A CMake toolchain file for building with traditional ELF linker or archives.
COPY repo/repo.cmake /usr/share/repo/
# A CMake toolchain file for building without intermediate ELF files.
COPY repo/full_repo.cmake /usr/share/repo/

# Add links from cc/c++ to clang/clang++
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang 30
RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++ 30

# Install a collection of packages that will allow us to build things.
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
            binutils                           \
            make                               \
            sudo                               \
            linux-headers-$(uname -r)

# GNU libc and libstd++. TODO: remove once musl libc and LLVM libstdc++ are done.
RUN apt-get install -y --no-install-recommends libc6-dev
RUN apt-get install -y --no-install-recommends libgcc-10-dev
RUN apt-get install -y --no-install-recommends libstdc++-10-dev

RUN rm -rf /var/lib/apt/lists/*




# Create the "prepo" user (password "prepo") with sudo privileges.
COPY --from=builder /tmp/workspace/prepo_pwd.txt /tmp/prepo_pwd.txt
RUN useradd --system --create-home --shell /bin/bash \
            --gid root --groups sudo,staff --uid 1000 \
            --password $(cat "/tmp/prepo_pwd.txt") \
            prepo
RUN rm /tmp/prepo_pwd.txt
USER prepo
WORKDIR /home/prepo

# Cleanup.
RUN rm -rf ${install_dir}
RUN rm -rf ${scripts_dir}
RUN rm -rf ${workspace_dir}
