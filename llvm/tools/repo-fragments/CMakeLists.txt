add_llvm_tool (repo-fragments
    RepoFragments.cpp
)
target_link_libraries (repo-fragments PRIVATE pstore-dump-lib)
#eof CMakeLists.txt
