#!/bin/bash
./build_docker_image.sh \
    --branch master \
    --docker-tag latest \
    --install-target install-clang \
    --install-target install-pstore \
    --install-target install-repo2obj \
    --install-target install-repo-ticket-dump \
    --install-target install-clang-headers \
    -- \
    -D CMAKE_BUILD_TYPE=Release
