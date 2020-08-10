#!/bin/bash
./build_docker_image.sh \
    --branch master \
    --docker-tag latest \
    --install-target install-clang \
    --install-target install-pstore \
    --install-target install-repo2obj \
    --install-target install-repo-ticket-dump \
    --install-target install-repo-fragments \
    --install-target install-clang-resource-headers \
    -- \
    -D CMAKE_BUILD_TYPE=Release \
    -D LLVM_PARALLEL_LINK_JOBS=1
