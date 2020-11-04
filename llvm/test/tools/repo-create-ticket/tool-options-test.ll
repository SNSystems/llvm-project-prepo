; Verify the behavior of repo-create-ticket when used with a valid compilation digest.
;
; The stages are:
; 1. Compile a minimal function to a repository.
; 2. Extract the compilation digest from the ticket produced in step 1.
; 3. Use repo-create-ticket to create a ticket from the digest in step 2.
; 4. Dump the compilation digest from the ticket made by step 3.
; 5. Match the digests from steps 2 and 4.
;
; RUN: rm -rf %t && mkdir -p %t
; RUN: env REPOFILE=%t/db.db llc -filetype=obj %s -o %t/t1.o
; RUN: repo-ticket-dump %t/t1.o > %t/out1.txt
; RUN: cat %t/out1.txt | repo-create-ticket -repo=%t/db.db -o %t/t2.o -
; RUN: repo-ticket-dump %t/t2.o > %t/out2.txt
; RUN: diff %t/out1.txt %t/out2.txt

; Check what happens when repo-create-ticket is fed bad input:
; - A value that's not a valid digest.
; - A valid digest, but one that's not found in the index.
;
; RUN: not repo-create-ticket -repo=%t/db.db -o %t/t3.o bad
; RUN: not repo-create-ticket -repo=%t/db.db -o %t/t4.o 00000000000000000000000000000000

target triple = "x86_64-pc-linux-gnu-repo"

define void @f() !repo_definition !0 {
entry:
  ret void
}

!repo.definitions = !{!0}
!0 = !RepoDefinition(name: "f", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: external, visibility: default, pruned: false)




