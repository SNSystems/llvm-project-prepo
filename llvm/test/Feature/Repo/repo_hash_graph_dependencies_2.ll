; Test that we correctly generate dependencies from function.
; function g has a reference to function f which will be a Dependencies for g.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only ir %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @f() {
entry:
    ret i32 2
}
define i32 @g() {
entry:
    %call = call i32 @f()
    %add = add nsw i32 %call, 1
    ret i32 %add
}

;CHECK: GO Name:f
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:g
;CHECK:     Initial Dependencies: [ f]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:f
;CHECK:     Final Dependencies: [ ]
;CHECK: GO Name:g
;CHECK:     Final Dependencies: [ f]
