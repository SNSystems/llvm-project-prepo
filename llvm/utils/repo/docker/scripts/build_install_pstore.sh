#!/usr/bin/env bash
#===- llvm/utils/repo/docker/scripts/build_install_pstore.sh --------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//

set -e

function show_usage() {
  cat << EOF
Usage: build_install_pstore.sh [options] -- [cmake-args]

Run cmake with the specified arguments. Used inside docker container.
Passes additional -DCMAKE_INSTALL_PREFIX and puts the build results into
the directory specified by --to option.

Available options:
  -h|--help           show this help message
  -d|--install-dir    destination directory where to install the targets.
  -i|--install-target name of a cmake install target to build and include in
                      the resulting archive. Can be specified multiple times.
  -w|--workspace      checkout and build workspace (internal)

Required options: --to, at least one --install-target.

All options after '--' are passed to CMake invocation.
EOF
}

CMAKE_ARGS=""
CMAKE_INSTALL_TARGETS=""
INSTALL_DIR=""
WORKSPACE_DIR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -d|--install-dir)
      shift
      INSTALL_DIR="$1"
      shift
      ;;
    -i|--install-target)
      shift
      CMAKE_INSTALL_TARGETS="$CMAKE_INSTALL_TARGETS $1"
      shift
      ;;
    -w|--workspace)
      shift
      WORKSPACE_DIR="$1"
      shift
      ;;
    --)
      shift
      CMAKE_ARGS="$*"
      shift $#
      ;;
    -h|--help)
      show_usage
      exit 0
      ;;
    *)
      echo "Unknown option: '$1'"
      exit 1
  esac
done

if [ "$WORKSPACE_DIR" == "" ]; then
  echo "Invalid workspace."
  exit 1
fi

if [ "$CMAKE_INSTALL_TARGETS" == "" ]; then
  echo "No install targets. Please pass one or more --install-target."
  exit 1
fi

if [ "$INSTALL_DIR" == "" ]; then
  echo "No install directory. Please specify the --to argument."
  exit 1
fi

mkdir -p "$INSTALL_DIR"

mkdir -p "$WORKSPACE_DIR/build"
pushd "$WORKSPACE_DIR/build"

# Run the build as specified in the build arguments.
echo "Running build on: '$(pwd)'"
echo "cmake -GNinja"
echo "      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR"
echo "      -DLLVM_ENABLE_PROJECTS=\"clang;pstore\""
echo "      -DLLVM_TOOL_CLANG_TOOLS_EXTRA_BUILD=OFF"
echo "      -DLLVM_TARGETS_TO_BUILD=X86"
echo "      $CMAKE_ARGS"
echo "      $WORKSPACE_DIR/src/llvm"
cmake -GNinja \
  -DCMAKE_INSTALL_PREFIX="$INSTALL_DIR" \
  -DLLVM_ENABLE_PROJECTS="clang;pstore" \
  -DLLVM_TOOL_CLANG_TOOLS_EXTRA_BUILD=OFF \
  -DLLVM_TARGETS_TO_BUILD=X86 \
  $CMAKE_ARGS \
  "$WORKSPACE_DIR/src/llvm"
ninja $CMAKE_INSTALL_TARGETS

popd

# Cleanup.
rm -rf "$WORKSPACE_DIR/build"

echo "Done"
