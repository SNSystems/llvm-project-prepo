; Test the store instruction which store new value to a global variable using getelementptr.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@xs = global [2 x i32] zeroinitializer, align 4

define void @test1() {
entry:
  store i32 2, i32* getelementptr inbounds ([2 x i32], [2 x i32]* @xs, i64 0, i64 0)
  ret void
}

;CHECK: !TicketNode(name: "xs", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: false)
