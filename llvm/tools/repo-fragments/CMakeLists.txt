set(LLVM_LINK_COMPONENTS
    Object
    Support
)
add_llvm_tool (repo-fragments
    RepoFragments.cpp
)
target_link_libraries (repo-fragments PRIVATE pstore-core)
#eof CMakeLists.txt
