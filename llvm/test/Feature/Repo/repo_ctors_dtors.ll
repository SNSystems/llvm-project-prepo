; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -mtriple=x86_64-pc-linux-gnu-repo -o %t
; RUN: env REPOFILE=%t.db llc -filetype=obj -mtriple=x86_64-pc-linux-gnu-repo %t
; RUN: env REPOFILE=%t.db opt -S -mtriple=x86_64-pc-linux-gnu-repo %s -o %t1
; RUN: env REPOFILE=%t.db llc -filetype=obj -mtriple=x86_64-pc-linux-gnu-repo %t1

target triple = "x86_64-pc-linux-gnu-repo"

@llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] [ { i32, void ()*, i8* } { i32 65535, void ()* @__mf_init, i8* null } ]
@llvm.global_dtors = appending global [1 x { i32, void ()*, i8* }] [ { i32, void ()*, i8* } { i32 65535, void ()* @__mf_fini, i8* null } ]

define void @__mf_init() {
entry:
        ret void
}

define void @__mf_fini() {
entry:
        ret void
}
