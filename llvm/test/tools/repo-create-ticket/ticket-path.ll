; REQUIRES: asserts
; RUN: rm -rf %t && mkdir -p %t
; RUN: env REPOFILE=%t/db.db llc -filetype=obj %s -o %t/t1.o
; RUN: repo-ticket-dump %t/t1.o > %t/out1.txt
; RUN: echo 'CHECK: ticket directory:%t' > %t/expected.txt
; RUN: cat %t/out1.txt | repo-create-ticket -debug -debug-only repo-create-ticket -repo=%t/db.db -o %t/t2.o - 2>&1 | FileCheck %t/expected.txt


target triple = "x86_64-pc-linux-gnu-repo"

define void @f() !repo_definition !0 {
entry:
  ret void
}

!repo.definitions = !{!0}
!0 = !RepoDefinition(name: "f", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: external, visibility: default, pruned: false)
