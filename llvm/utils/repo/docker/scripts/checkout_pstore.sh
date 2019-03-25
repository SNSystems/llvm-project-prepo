#!/usr/bin/env bash
#===- llvm/utils/repo/docker/scripts/checkout_pstore.sh -------------------===//
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===-----------------------------------------------------------------------===//

set -e

function show_usage() {
  cat << EOF
Usage: checkout_pstore.sh [options]

Checkout git sources into /tmp/pstore-build/src. Used inside a docker container.

Available options:
  -h|--help           show this help message
  -b|--branch         git branch to checkout, i.e. 'trunk',
                      'branches/release_40'
                      (default: 'trunk')
  -l|--local          use local repository
  -w|--workspace      checkout and build workspace (internal)
EOF
}

function clone_project() {
  local PROJECT="$1"
  local DIRECTORY="$2"
  local DESTINATION="$3"
  local SOURCE=$BRANCH

  # Check if remote branch exists.
  set +e
  git ls-remote --heads --exit-code $GIT_REPOSITORY/$PROJECT $SOURCE
  if [ "$?" == "2" ]; then
    echo "Branch '$SOURCE' does not exist. Using 'master'."
    SOURCE="master"
  fi
  set -e
  echo "Checking out '$SOURCE/$PROJECT' into '$DIRECTORY/$DESTINATION'"
  git clone --branch $SOURCE $GIT_REPOSITORY/$PROJECT $DESTINATION
}

BRANCH=""
GIT_REPOSITORY=""
LOCAL_REPOSITORY=""
WORKSPACE_DIR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -b|--branch)
      shift
      BRANCH="$1"
      shift
      ;;
    -l|--local)
      shift
      LOCAL_REPOSITORY="$1"
      shift
      ;;
    -h|--help)
      show_usage
      exit 0
      ;;
    -w|--workspace)
      shift
      WORKSPACE_DIR="$1"
      shift
      ;;
    *)
      echo "Unknown option: '$1'"
      exit 1
  esac
done

if [ "$WORKSPACE_DIR" == "" ]; then
  echo "Invalid workspace"
  exit 1
fi

# Clone external git repository.
if [ "$LOCAL_REPOSITORY" == "" ]; then
  if [ "$BRANCH" == "" ]; then
    BRANCH="master"
  fi

  # Get the sources from git.
  echo "Checking out sources from git into '$WORKSPACE_DIR'"
  mkdir -p $WORKSPACE_DIR

  GIT_REPOSITORY="https://github.com/SNSystems"
  LLVM_PROJECT="llvm-project-prepo.git"
  PSTORE_PROJECT="pstore.git"

  cd $WORKSPACE_DIR
  clone_project $LLVM_PROJECT $WORKSPACE_DIR src
  echo "Done llvm"

  cd $WORKSPACE_DIR/src
  clone_project $PSTORE_PROJECT $WORKSPACE_DIR/src pstore
  echo "Done pstore"
fi

cd $WORKSPACE_DIR
echo "Done"
