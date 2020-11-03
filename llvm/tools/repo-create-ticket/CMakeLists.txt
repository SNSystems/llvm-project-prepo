set(LLVM_LINK_COMPONENTS
    Object
    Support
)
add_llvm_tool (repo-create-ticket
    RepoCreateTicket.cpp
)
target_link_libraries (repo-create-ticket PRIVATE pstore-core)
#eof CMakeLists.txt
