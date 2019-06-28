; Test the store instruction which store new value to a global variable through load instruction.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@a = global i32 0, align 4
@b = global i32* @a, align 8

; Function Attrs: noinline nounwind optnone uwtable
define void @test2() {
entry:
  %0 = load i32*, i32** @b, align 8
  store i32 2, i32* %0, align 4
  ret void
}

;CHECK: !TicketNode(name: "a", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: false)
;CHECK: !TicketNode(name: "b", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: false)
