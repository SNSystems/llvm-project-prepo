#===- llvm/utils/repo/docker/llvm-prepo/Dockerfile ------------------------===//
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
# TODO: we'll eventually want linux-headers for building the standard
# libraries, but they're not yet being used properly.
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC \
    apt-get install -y --no-install-recommends \
            apt-utils                 \
            build-essential           \
            ca-certificates           \
            cmake                     \
            git                       \
            linux-headers-$(uname -r) \
            ninja-build               \
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



# Write the repo.json file which will tell the "wrap" tools how to archive
# things, link, and run repo2obj.
RUN echo "{ \
  \"ar\":       \"/usr/bin/ar\",\
  \"link\":     \"$install_dir/bin/clang\",\
  \"link++\":   \"$install_dir/bin/clang++\",\
  \"repo2obj\": \"$install_dir/bin/repo2obj\"\
}" > $workspace_dir/repo.json


ARG stdlib_repo="$install_dir/lib/stdlib.repo"
ARG configure_repo="$workspace_dir/configure.repo"

# The directory containing the "wrap" tools.
ARG wrap_dir="$workspace_dir/src/llvm/utils/repo/wrap"


#############
# musl libc #
#############

ARG musl_install_dir="/tmp/muslinstall"
ARG musl_workspace=$workspace_dir/musl

RUN git clone --depth 1 --single-branch --progress \
  https://github.com/SNSystems/musl-prepo          \
  $musl_workspace
ENV CC=$install_dir/bin/clang
ENV PATH=$install_dir/bin:$PATH
RUN cd $musl_workspace; \
  REPOFILE=$configure_repo ./configure \
    --disable-shared                   \
    --prefix=$musl_install_dir

RUN REPOFILE=$stdlib_repo make -C $musl_workspace \
  -j $(nproc)                                     \
  install-headers                                 \
  install-ticket-libs                             \
  CC=$install_dir/bin/clang                       \
  utils_dir=$wrap_dir

# TODO: remove once running cmake doesn't require an ELF linker.
RUN REPOFILE=$stdlib_repo make -C $musl_workspace \
  -j $(nproc)                                     \
  install-repo2obj-libs                           \
  CC=$install_dir/bin/clang                       \
  utils_dir=$wrap_dir
RUN mkdir -p $musl_install_dir/include/linux


# TODO: libcxx requires linux/futex.h and linux/version.h. ATM I can't use
# copies in /usr/include because of the risk of mixing pieces of musl libc and
# glibc. Need to move the build of standard libraries onto a third docker image
# so that the build environment can be carefully controlled.
COPY llvm-prepo/futex.h $musl_install_dir/include/linux/futex.h
RUN cp /usr/include/linux/version.h $musl_install_dir/include/linux/version.h

#############
# libunwind #
#############

# A directory to contain the standard library builds.
ARG libbuild_dir="$workspace_dir/libbuild"

# Arguments common to the runs of cmake for each of the standard libraries.
ARG cmake_common="\
  -G Ninja                                                 \
  -D CMAKE_BUILD_TYPE=Debug                                \
  -D CMAKE_CXX_COMPILER=$install_dir/bin/clang++           \
  -D CMAKE_C_COMPILER=$install_dir/bin/clang               \
  -D CMAKE_INSTALL_PREFIX=$install_dir                     \
  -D CMAKE_TOOLCHAIN_FILE=$workspace_dir/src/llvm/utils/repo/full_repo.cmake \
  -D LLVM_CONFIG_PATH=$workspace_dir/build/bin/llvm-config \
  -D LLVM_ENABLE_LIBCXX=Yes                                \
  -D LLVM_PATH=$workspace_dir/src/llvm                     \
  -D musl=Yes                                              \
  -D musl_install=$musl_install_dir                        \
  -D utils_dir=$wrap_dir                                   \
"

RUN REPOFILE=$configure_repo cmake \
  -S $workspace_dir/src/libunwind  \
  -B $libbuild_dir/unwind          \
  -D LIBUNWIND_ENABLE_SHARED=No    \
  -D LIBUNWIND_ENABLE_STATIC=Yes   \
  $cmake_common

RUN REPOFILE=$stdlib_repo ninja -C $libbuild_dir/unwind install


##################
# libcompiler-rt #
##################

RUN REPOFILE=$configure_repo cmake       \
  -S $workspace_dir/src/compiler-rt      \
  -B $libbuild_dir/compiler-rt           \
  -D COMPILER_RT_DEFAULT_TARGET_ONLY=Yes \
  -D COMPILER_RT_BUILD_BUILTINS=Yes      \
  -D COMPILER_RT_BUILD_SANITIZERS=No     \
  -D COMPILER_RT_BUILD_XRAY=No           \
  -D COMPILER_RT_BUILD_LIBFUZZER=No      \
  -D COMPILER_RT_BUILD_PROFILE=No        \
  $cmake_common

RUN REPOFILE=$stdlib_repo ninja -C $libbuild_dir/compiler-rt install


#############
# libcxxabi #
#############

RUN REPOFILE=$configure_repo cmake                                     \
  -S $workspace_dir/src/libcxxabi                                      \
  -B $libbuild_dir/cxxabi                                              \
  -D libcxxabi_include=-I$workspace_dir/build/lib/clang/11.0.0/include \
  -D LIBCXXABI_ENABLE_EXCEPTIONS=No                                    \
  -D LIBCXXABI_USE_COMPILER_RT=Yes                                     \
  -D LIBCXXABI_ENABLE_SHARED=No                                        \
  -D LIBCXXABI_ENABLE_STATIC=Yes                                       \
  -D LLVM_ENABLE_LIBCXX=Yes                                            \
  $cmake_common

RUN REPOFILE=$stdlib_repo ninja -C $libbuild_dir/cxxabi install


##########
# libcxx #
##########

RUN REPOFILE=$configure_repo cmake                                     \
  -S $workspace_dir/src/libcxx                                         \
  -B $libbuild_dir/cxx                                                 \
  -D LIBCXX_ENABLE_SHARED=No                                           \
  -D LIBCXX_ENABLE_STATIC=Yes                                          \
  -D LIBCXX_CXX_ABI=libcxxabi                                          \
  -D LIBCXX_CXX_ABI_INCLUDE_PATHS=$workspace_dir/src/libcxxabi/include \
  -D LIBCXX_HAS_MUSL_LIBC=Yes                                          \
  $cmake_common

RUN REPOFILE=$stdlib_repo ninja -C $libbuild_dir/cxx install

# Give all users read permission for the stdlib database.
RUN chmod a+r $stdlib_repo

#####################################################################################################
# Stage 2. Produce a minimal release image with build results.
FROM ubuntu:20.04 as llvm-prepo-stage
LABEL maintainer "paulhuggett@mac.com"
ENTRYPOINT /bin/bash

# Copy build results of stage 1 to /usr/local.
#COPY --from=builder $install_dir/ $user_dir/
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
  binutils                                     \
  linux-headers-$(uname -r)                    \
  make                                         \
  sudo


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
RUN rm -rf $install_dir
RUN rm -rf $scripts_dir
RUN rm -rf $workspace_dir
