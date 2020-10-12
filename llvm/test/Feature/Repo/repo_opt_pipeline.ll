; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O0 -debug-pass=Structure < %s -o /dev/null 2>&1 | FileCheck %s
; RUN: rm -f %t1.db
; RUN: env REPOFILE=%t1.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O1 -debug-pass=Structure < %s -o /dev/null 2>&1 | FileCheck %s
; RUN: rm -f %t2.db
; RUN: env REPOFILE=%t2.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O2 -debug-pass=Structure < %s -o /dev/null 2>&1 | FileCheck %s
; RUN: rm -f %t3.db
; RUN: env REPOFILE=%t3.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O3 -debug-pass=Structure < %s -o /dev/null 2>&1 | FileCheck %s
; RUN: rm -f %t4.db
; RUN: env REPOFILE=%t4.db opt -mtriple="x86_64-pc-linux-gnu-repo" -Os -debug-pass=Structure < %s -o /dev/null 2>&1 | FileCheck %s
; RUN: rm -f %t5.db
; RUN: env REPOFILE=%t5.db opt -mtriple="x86_64-pc-linux-gnu-repo" -debug-pass=Structure < %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

; CHECK:       ModulePass Manager
; CHECK-NEXT:     RepoMetadataGenerationPass
; CHECK-NEXT:     RepoPruningPass
; CHECK-NEXT:     Dead Global Elimination

define void @f() {
  ret void
}
