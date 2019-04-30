; This testcase tests the program repository pruning pass.
;
; If function 'A' is dependent on function 'B' and 'B' is dependent
; on function 'C', both TickeNodes of 'B' and 'C' need to be added
; into in the 'repo.tickets' during the pruning.

; The testcase includes three steps:
; Step 1: Build repo_prunning_dependents.ll and create the database 'clang.db' which contains all Tickets.
; Step 2: Build this IR code.
; Step 3: Check the TicketNodes which contain all dependents of the funciton 'A' and the pruning flags are true.

; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -mtriple="x86_64-pc-linux-gnu-repo" -filetype=obj %S/Inputs/repo_prunning_dependents.ll -o %t
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -S -std-link-opts -disable-opt -disable-prepo -prepo-pruning %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @A() !repo_ticket !0 {
entry:
  %call = call i32 @D()
  ret i32 %call
}

define available_externally i32 @D() {
entry:
  %call = call i32 @E()
  ret i32 %call
}

define available_externally i32 @E() {
entry:
  ret i32 1
}


!repo.tickets = !{!0}

!0 = !TicketNode(name: "A", digest: [16 x i8] c"^\05\06\f8\aaE\c5\84\1a\a1c\e2\8dn\1dm", linkage: external, pruned: false)

;CHECK:      !0 = !TicketNode(name: "A", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: true)
;CHECK-NEXT: !1 = !TicketNode(name: "D", digest: [16 x i8] c"{{.*}}", linkage: internal, pruned: true)
;CHECK-NEXT: !2 = !TicketNode(name: "E", digest: [16 x i8] c"{{.*}}", linkage: internal, pruned: true)
