; Check that the GOs' hash caculation with a graph of the form:
;
; digraph G {
;   c -> a;
;   c -> b;
; }
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -stats -debug-only prepo -debug-only prepo-digest -mtriple x86_64-pc-linux-gnu-repo %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-elf"

define i32 @A() {
entry:
  ret i32 1
}

define i32 @B() {
entry:
  ret i32 2
}

define i32 @C() {
entry:
  %call = call i32 @A()
  %call1 = call i32 @B()
  %add = add nsw i32 %call, %call1
  ret i32 %add
}

;CHECK: Computing hash for "A" (#0)
;CHECK: Recording result for "A"
;CHECK: Computing hash for "B" (#0)
;CHECK: Recording result for "B"
;CHECK: Computing hash for "C" (#0)
;CHECK: Computing hash for "A" (#1)
;CHECK: Returning pre-computed hash for "A"
;CHECK: Computing hash for "B" (#1)
;CHECK: Returning pre-computed hash for "B"
;CHECK: Recording result for "C"
;
;CHECK:      3 prepo-digest - Number of functions hashed
;CHECK-NEXT: 3 prepo-digest - Number of memoized hashes
;CHECK-NEXT: 5 prepo-digest - Visited times of memoized hashes
