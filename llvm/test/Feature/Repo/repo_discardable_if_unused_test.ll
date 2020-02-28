; This test is used to check the following issue.
;
; If the caller and callee satisfies the following conditions, the error of undefined reference to the callee is given.
;    1) Caller is pruned and deleted during the RepoPruning pass.
;    2) Callee isn’t pruned, has noinline attribute and may be discarded if it is not used.
;
; The reason is that the callee is removed during the Dead Global Elimination pass, which is after the repo pruning pass.
; The current hash calculation algorithm doesn’t accumulate the callee's hash value to the caller if the callee has
; noinline attribute.
; To solve the issue, the caller always accumulates the callee’s hash value if callee may be discarded if it is not used.
;
; To reproduce the issue, the testcase includes three steps:
; Step 1: Generate the repo IR code which contains the TicketNode metadata. The functions `g` and `callee` are
;         defined in repo_discardable_GOs.ll file. The `callee` has noinline attribute and the internal linkage type;
; Step 2: Create the database 'clang.db' which contains the fragments of `g` and `callee`.
; Step 3: Run the opt to genearte the repo IR code defined in this file.
;         Compared to step 1, we change the function `callee` return value, the hash value of `callee` is changed.
;         However, the hash value of its caller `q` keeps the same as the function `g` defined in step 1).
;         Before Fix:
;             During the step 3), the function `q` is pruned and `callee` is not pruned.
;             Since the function q is pruned and has an empty UseList, it will be deleted.
;             After repo pruning, the function q has been changed from definition to declaration,
;             which will result in changing the UseList of function `callee` from 1 to 0.
;             The Dead Global Elimination pass is after the repo pruning pass. Since the function `callee`
;             is local function and has an empty UseList, it is removed as well. Therefore, there is only one
;             compilation member q for this file. There is a missing compilation member `callee`.
;             This causes the Link error.
;        After Fix:
;             Both function `q` and `callee` shouldn't be removed by DCE.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %S/Inputs/repo_discardable_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

;CHECK: define dso_local i32 @q()
define dso_local i32 @q() {
entry:
  %call = call i32 @callee()
  ret i32 %call
}

;CHECK: define internal i32 @callee()
define internal i32 @callee() noinline {
entry:
  ret i32 2
}

;CHECK: !TicketNode(name: "q", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: false)
;CHECK: !TicketNode(name: "callee", digest: [16 x i8] c"{{.*}}", linkage: internal, visibility: default, pruned: false)
