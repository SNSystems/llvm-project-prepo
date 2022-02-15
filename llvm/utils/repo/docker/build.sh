#!/bin/bash
#
#===- llvm/utils/repo/docker/build.sh -------------------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//
./build_docker_image.sh                             \
    --branch master                                 \
    --docker-tag latest                             \
    --install-target install-clang                  \
    --install-target install-clang-resource-headers \
    --install-target install-pstore                 \
    --install-target install-repo2obj               \
    --install-target install-repo-ticket-dump       \
    --install-target install-repo-create-ticket     \
    --install-target install-repo-fragments         \
    --install-target install-rld                    \
    --                                              \
    -D CMAKE_BUILD_TYPE=Release                     \
    -D PSTORE_POSIX_SMALL_FILES=Yes                 \
    -D LLVM_PARALLEL_LINK_JOBS=1                    \
    -D LLVM_DEFAULT_TARGET_TRIPLE=x86_64-pc-linux-musl-repo
