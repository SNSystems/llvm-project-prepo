; This test checks that no redundant compilation members are added into the compilation.
; The explanation of the dependents is given in the repo_pruning_dependents.ll,
; Please refer to the file for details.
;
; In the test, functions A and B are both dependent on function C. During the pruning
; pass, only add a single TicketNode of C to the 'repo.tickets' in order to avoid multiple
; compilation members of function C in the compilation.
;
; The steps in this test are:
; Step 1: Remove the database.
; Step 2: Start from an ELF IR, insert the TicketNode to generate IR targeting the Repo.
; Step 3: Remove the TicketNode metadata from the function C.
; Step 4: Since the function C doesn't have the TicketNode, the digest of function C is generated
;         by the RepoObjectWriter and added into the dependents of the functions A and B.
; Step 5: Dump all compilation members in the database. There are 3 compilation members, which are
;         functions A, B and C.
; Step 6: Re-build the same source IR code again. In this step, the IR code firstly pass the
;         RepoMetadataGenerator pass to generate the digests of functions A, B and C.
;         During the RepoPruning pass, the function A and B are pruned, only add a single
;         TicketNode of their dependent C is added to the 'repo.tickets'.
; Step 7: Dump all compilation members in the database again. The compilations, which are generated
;         by step 4 and step 6, should be the same. Therefore, there are only one compilation, which
;         includes three members: functions A, B and C.
; Step 8: Check that the step 5 and step 7 generate the same compilation members.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db clang -O0 -S -emit-llvm --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t.repo.ll
; RUN: %python %S/Inputs/remove_GO_ticketnode.py -r C -i %t.repo.ll -o %t.ll
; RUN: env REPOFILE=%t.db llc -mtriple="x86_64-pc-linux-gnu-repo" -filetype=obj %t.ll -o %t.1.o
; RUN: env REPOFILE=%t.db pstore-dump --all-compilations %t.db > %t.1.log
; RUN: env REPOFILE=%t.db clang -O0 -c --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t.2.o
; RUN: env REPOFILE=%t.db pstore-dump --all-compilations %t.db > %t.2.log
; RUN: diff %t.1.log %t.2.log

target triple = "x86_64-pc-linux-gnu-elf"

define i32 @A() {
entry:
  %call = call i32 @C()
  ret i32 %call
}

define i32 @B() {
entry:
  %call = call i32 @C()
  %add = add nsw i32 %call, 1
  ret i32 %add
}

define internal i32 @C() {
entry:
  ret i32 1
}
