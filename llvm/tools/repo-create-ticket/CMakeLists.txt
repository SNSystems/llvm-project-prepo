set(LLVM_LINK_COMPONENTS
    Object
    MC
    Support
)
add_llvm_tool (repo-create-ticket
    RepoCreateTicket.cpp
)
if (WIN32)
  target_compile_definitions (repo-create-ticket PRIVATE _ENABLE_EXTENDED_ALIGNED_STORAGE)
endif (WIN32)

target_link_libraries (repo-create-ticket PRIVATE pstore-core)
#eof CMakeLists.txt
