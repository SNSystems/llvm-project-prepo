; This testcase tests the program repository pruning pass.
;
; If functions 'A' and 'B' are both dependent on function 'C',
; only add a single TicketNode of 'C' to the 'repo.tickets'
; in order to avoid multiple compilation_members of function
; 'C' in the compilation.

; The testcase includes three steps:
; Step 1: Build repo_prunning_dependents.ll and create the database 'clang.db' which contains all Tickets.
; Step 2: Build this IR code.
; Step 3: Check the TicketNodes which only contain a single compilation_member of the funciton 'C'.


; This test only works for the Debug build because the digest of A is calculated for the Debug build.

; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -mtriple="x86_64-pc-linux-gnu-repo" -filetype=obj %S/Inputs/repo_prunning_dependents.ll -o %t
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -S -std-link-opts -disable-opt -disable-prepo -prepo-pruning %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @B() !repo_ticket !0 {
entry:
  %call = call i32 @E()
  ret i32 %call
}

define i32 @C() !repo_ticket !1 {
entry:
  %call = call i32 @E()
  %add = add nsw i32 %call, 1
  ret i32 %add
}


define available_externally i32 @E() {
entry:
  ret i32 1
}

!repo.tickets = !{!0, !1}

!0 = !TicketNode(name: "B", digest: [16 x i8] c"\22+\12\d0\8a\10\deN\f4R\0d\bc{^\c8\84", linkage: external, pruned: false)
!1 = !TicketNode(name: "C", digest: [16 x i8] c"@\b5\f3\96\91-s*\8dT\1c\ca\8d\b8@\bc", linkage: external, pruned: false)

;CHECK:      !0 = !TicketNode(name: "B", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: true)
;CHECK-NEXT: !1 = !TicketNode(name: "C", digest: [16 x i8] c"{{.*}}", linkage: external, pruned: true)
;CHECK-NEXT: !2 = !TicketNode(name: "E", digest: [16 x i8] c"{{.*}}", linkage: internal, pruned: true)
;CHECK-NOT: !TicketNode(name: "E"
