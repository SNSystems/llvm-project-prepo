; Check that the GOs' hash caculation with a graph of the form:
;
; digraph G {
;   a -> b -> c -> b;
;   a -> d -> e;
;   d -> f;
; }
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -stats -debug-only prepo -debug-only prepo-digest -mtriple x86_64-pc-linux-gnu-repo %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-elf"

define void @B() {
entry:
  call void @C()
  ret void
}

define void @C() {
entry:
  call void @B()
  ret void
}

define i32 @E() {
entry:
  ret i32 1
}

define i32 @F() {
entry:
  ret i32 2
}

define i32 @D() {
entry:
  %call = call i32 @E()
  %call1 = call i32 @F()
  %add = add nsw i32 %call, %call1
  ret i32 %add
}

define i32 @A() #0 {
entry:
  call void @B()
  %call = call i32 @D()
  ret i32 %call
}

;CHECK: Computing hash for "B" (#0)
;CHECK: Computing hash for "C" (#1)
;CHECK: Computing hash for "B" (#2)
;CHECK: Hashing back reference to #0
;CHECK-NOT: Recording result for "B"
;CHECK: Computing hash for "C" (#0)
;CHECK: Computing hash for "B" (#1)
;CHECK: Computing hash for "C" (#2)
;CHECK: Hashing back reference to #0
;CHECK-NOT: Recording result for "C"
;CHECK: Computing hash for "E" (#0)
;CHECK: Recording result for "E"
;CHECK: Computing hash for "F" (#0)
;CHECK: Recording result for "F"
;CHECK: Computing hash for "D" (#0)
;CHECK: Computing hash for "E" (#1)
;CHECK: Returning pre-computed hash for "E"
;CHECK: Computing hash for "F" (#1)
;CHECK: Returning pre-computed hash for "F"
;CHECK: Recording result for "D"
;CHECK: Computing hash for "A" (#0)
;CHECK: Computing hash for "B" (#1)
;CHECK: Computing hash for "C" (#2)
;CHECK: Computing hash for "B" (#3)
;CHECK: Hashing back reference to #1
;CHECK: Computing hash for "D" (#3)
;CHECK: Returning pre-computed hash for "D"
;CHECK: Recording result for "A"
;
;CHECK:      6 prepo-digest - Number of functions hashed
;CHECK-NEXT: 4 prepo-digest - Number of memoized hashes
;CHECK-NEXT: 7 prepo-digest - Visited times of memoized hashes
;CHECK-NEXT: 6 prepo-digest - Visited times of unmemoized hashes
