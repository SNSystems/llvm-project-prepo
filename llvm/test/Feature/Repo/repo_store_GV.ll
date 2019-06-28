; Test the store instruction which store new value to a global variable.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@X = global i32 0

define void @foov() {
entry:
    store i32 2, i32* @X
    ret void
}

;CHECK: !TicketNode(name: "X", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: false)
