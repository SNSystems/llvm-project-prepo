; Check that the GOs' hash caculation with a graph of the form:
;
; digraph G {
;   c -> a -> b -> a;
; }
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -stats -debug-only prepo -debug-only prepo-digest -mtriple x86_64-pc-linux-gnu-repo %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-elf"

define void @A() {
entry:
  call void @B()
  ret void
}

define void @B() {
entry:
  call void @A()
  ret void
}

define void @C() {
entry:
  call void @A()
  ret void
}

;CHECK: Computing hash for "A" (#0)
;CHECK: Computing hash for "B" (#1)
;CHECK: Computing hash for "A" (#2)
;CHECK-NOT: Returning pre-computed hash for "A"
;CHECK: Hashing back reference to #0
;CHECK-NOT: Recording result for "A"
;CHECK: Computing hash for "B" (#0)
;CHECK: Computing hash for "A" (#1)
;CHECK: Computing hash for "B" (#2)
;CHECK-NOT: Returning pre-computed hash for "B"
;CHECK: Hashing back reference to #0
;CHECK-NOT: Recording result for "B"
;CHECK: Computing hash for "C" (#0)
;CHECK: Computing hash for "A" (#1)
;CHECK: Computing hash for "B" (#2)
;CHECK: Computing hash for "A" (#3)
;CHECK: Hashing back reference to #1
;CHECK: Recording result for "C"
;
;CHECK:      3 prepo-digest - Number of functions hashed
;CHECK-NEXT: 1 prepo-digest - Number of memoized hashes
;CHECK-NEXT: 1 prepo-digest - Visited times of memoized hashes
;CHECK-NEXT: 6 prepo-digest - Visited times of unmemoized hashes
