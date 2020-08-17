; Test the fragment extent of compilation member.
;
; The test case1 includes four steps:
; Step 1&2: Create a database which contains the 't1' fragment.
; Step 3&4: Generate IR targeting the repo and run llc again to check the
;           compilation member's fragment extent.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S -filetype=obj %s -o %t1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t1 -o /dev/null 2>&1
;
; The test case2: check error pof the missing fragment for the pruned GO.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db not --crash llc -filetype=obj %t1 -o /dev/null 2>&1 | FileCheck %s
;
target triple = "x86_64-pc-linux-gnu-repo"

define i32 @t1() {
entry:
  ret i32 1
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @t2() {
entry:
  %call = call i32 @t1()
  ret i32 %call
}

; CHECK: LLVM ERROR: The digest of missing repository fragment {{[0-9a-fA-F]+}} was found in a compilation member.
