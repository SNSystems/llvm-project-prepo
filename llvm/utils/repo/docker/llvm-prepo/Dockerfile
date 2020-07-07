#===- llvm/utils/repo/docker/pstore/Dockerfile ----------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//

# Stage 1. Check out LLVM and pstore source code and run the build.
FROM ubuntu:18.04 as builder
#LABEL maintainer "LLVM Developers"

# Install LLVM's build dependencies.

# First, Update the apt's source list and include the sources of the packages.
RUN grep deb /etc/apt/sources.list | \
    sed 's/^deb/deb-src /g' >> /etc/apt/sources.list

# Install compiler, python, ninja and cmake.
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    apt-utils build-essential ca-certificates cmake git ninja-build python

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

RUN apt-get install -y openssl
RUN openssl passwd -1 prepo > "${workspace_dir}/prepo_pwd.txt"

# Stage 2. Produce a minimal release image with build results.
FROM ubuntu:18.04 as llvm-prepo-stage
#LABEL maintainer "LLVM Developers"
ENTRYPOINT /bin/bash

# Copy build results of stage 1 to /usr/local.
#COPY --from=builder ${install_dir}/ ${user_dir}/
# Unfortunately, the COPY command does not support variables.
COPY --from=builder /tmp/install/bin /usr/bin/
COPY --from=builder /tmp/install/lib /usr/lib/
COPY --from=builder /tmp/install/etc/init.d/pstore-brokerd /etc/init.d/

# Add links from cc/c++ to clang/clang++
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang 30
RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++ 30

# Install a collection of packages that will allow us to build things.
RUN apt-get update
RUN apt-get install -y --no-install-recommends  sudo
RUN apt-get install -y binutils libstdc++-8-dev
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

