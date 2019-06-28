; Test the store instruction which store new value to a global variable using bitcase.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

%TS = type { i64* }
%TA = type { i64 }

@s = local_unnamed_addr global %TS zeroinitializer, align 8
@gA = local_unnamed_addr global %TA* inttoptr (i64 69925 to %TA*), align 8

define void @test3() {
  %1 = load i64, i64* bitcast (%TA** @gA to i64*), align 8
  store i64 %1, i64* bitcast (%TS* @s to i64*), align 8
  ret void
}

;CHECK: !TicketNode(name: "s", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: false)
