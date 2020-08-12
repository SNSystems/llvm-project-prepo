; Test that we correctly generate dependencies from a global variable whose initial value directly referenced to a function.
; global variable FPtr has a reference to function add which will be a Dependencies for FPtr.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only prepo-digest %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@FPtr = global i32 (i32, i32)* @add, align 8
define i32 @add(i32 %a, i32 %b)  {
entry:
    %add = add nsw i32 %a, %b
    ret i32 %add
}

;CHECK: GO Name:FPtr
;CHECK:     Initial Dependencies: [ add]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:add
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:add
;CHECK:     Final Dependencies: [ ]
;CHECK: GO Name:FPtr
;CHECK:     Final Dependencies: [ add]
