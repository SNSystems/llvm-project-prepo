; Compile to two different databases. Run repo-fragments with a ticket
; associated with one database but specifying the other to ensure that it
; returns failure.
;
; RUN: rm -rf %t && mkdir -p %t
;
; RUN: env REPOFILE=%t/db1.db clang -c -x ir %s -o %t/ticket1.o
; RUN: env REPOFILE=%t/db2.db clang -c -x ir %s -o %t/ticket2.o
; RUN: repo-fragments -repo=%t/db1.db %t/ticket1.o
; RUN: not repo-fragments -repo=%t/db2.db %t/ticket1.o

target triple = "x86_64-pc-linux-gnu-repo"

define void @f() {
entry:
  ret void
}

