; Test the call instruction which change the value of global variable .
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@Z = global i32 1
define void @test4() {
	call void @setto( i32* @Z, i32 3 )
	ret void
}

define void @setto(i32* %P, i32 %V) {
	store i32 %V, i32* %P
	ret void
}

;CHECK: !TicketNode(name: "Z", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: false)
