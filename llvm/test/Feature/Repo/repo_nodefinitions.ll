; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj -debug-only repo-object %s -o %t.o

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"wchar_size", i32 4}

