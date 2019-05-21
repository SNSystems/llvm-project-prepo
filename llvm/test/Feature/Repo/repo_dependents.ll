; This testcase tests the fragment dependents.
;
; If functions 'A' and 'B' are both dependent on global variable 'input_str',
; only add a single TicketNode of '.str.2' to the 'repo.tickets'
; in order to avoid multiple compilation members of global variable
; '.str.2' in the compilation.

; The testcase includes two steps:
; Step 1: Build repo_prunning_dependents_test2.ll and create the database 'clang.db' which contains all Tickets.
; Step 2: Check the compilation members which only contain a single compilation_member of the global variable '.str.2'.

; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db clang --target="x86_64-pc-linux-gnu-repo" -O3 -c %s -o t.o
; RUN: env REPOFILE=%t.db pstore-dump -all-compilations %t.db | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@.input_str = private unnamed_addr constant [7 x i8] c"hello\0A\00", align 1

declare i32 @printf(i8* nocapture readonly, ...)

define void @A() {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.input_str, i32 0, i32 0))
  ret void
}

define void @B() align 16 {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.input_str, i32 0, i32 0))
  %call1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.input_str, i32 0, i32 0))
  ret void
}


;CHECK:      members :
;CHECK:      name    : A
;CHECK:      name    : B
;CHECK:      name    : str.2
;CHECK-NOT:  name    : str.2
