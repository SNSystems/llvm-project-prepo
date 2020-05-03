; Check that the GOs' hash caculation with a graph of the form:
;
; digraph G {
;   a -> b -> b;
; }
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -stats -debug-only prepo -debug-only prepo-digest -mtriple x86_64-pc-linux-gnu-repo %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-elf"

; Function Attrs: noinline nounwind optnone uwtable
define void @B() {
entry:
  call void @B()
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @A() {
entry:
  call void @B()
  ret void
}

;CHECK: Computing hash for "B" (#0)
;CHECK: Computing hash for "B" (#1)
;CHECK: Hashing back reference to #0
;CHECK: Recording result for "B"
;CHECK: Computing hash for "A" (#0)
;CHECK: Computing hash for "B" (#1)
;CHECK: Returning pre-computed hash for "B"
;CHECK: Recording result for "A"
;
;CHECK:      2 prepo-digest - Number of functions hashed
;CHECK-NEXT: 2 prepo-digest - Number of memoized hashes
;CHECK-NEXT: 3 prepo-digest - Visited times of memoized hashes
