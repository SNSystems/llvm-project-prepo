; Check that @Z is @f's only dependency. @g is not included because it is a noinline function.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -O3 -debug-only ir -mtriple x86_64-pc-linux-gnu-repo %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@Z = global i32 1
define void @f() noinline {
    call void @g( i32* @Z, i32 3 )
    ret void
}

define void @g(i32* %P, i32 %V) noinline {
    store i32 %V, i32* %P
    ret void
}

;CHECK: GO Name:f
;CHECK:  Contributions: [ Z]
