#===- llvm/utils/repo/docker/pstore/Dockerfile ----------------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//

# Stage 1. Check out LLVM and PStore source code and run the build.
FROM launcher.gcr.io/google/ubuntu16_04 as builder
#LABEL maintainer "LLVM Developers"

# Install build dependencies of llvm.
# First, Update the apt's source list and include the sources of the packages.
RUN grep deb /etc/apt/sources.list | \
    sed 's/^deb/deb-src /g' >> /etc/apt/sources.list

# Install compiler, python, ninja and cmake.
RUN apt-get update && apt-get install -y --no-install-recommends apt-utils \
    build-essential python cmake git ninja-build && \
    rm -rf /var/lib/apt/lists/*

# Miscellaneous directories.
ARG install_dir="/tmp/install"
ARG scripts_dir="/tmp/scripts"
ARG user_dir="/usr/local"
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
RUN ${scripts_dir}/checkout_pstore.sh \
    --workspace ${workspace_dir} ${checkout_args}

# Run the build. Results of the build will be available at install directory.
ARG build_args
RUN echo "build_args: " ${build_args}
RUN ${scripts_dir}/build_install_pstore.sh \
    --workspace ${workspace_dir} --install-dir ${install_dir} ${build_args}

# Stage 2. Produce a minimal release image with build results.
FROM launcher.gcr.io/google/ubuntu16_04 as pstore-stage
#LABEL maintainer "LLVM Developers"

# Copy build results of stage 1 to /usr/local.
#COPY --from=builder ${install_dir}/ ${user_dir}/
# The COPY command does not support variables.
COPY --from=builder /tmp/install/ /usr/local/

# Cleanup.
RUN rm -rf ${install_dir}
RUN rm -rf ${scripts_dir}
RUN rm -rf ${workspace_dir}
