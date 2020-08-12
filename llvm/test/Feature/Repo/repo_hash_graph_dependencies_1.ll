; Test that we correctly generate dependencies from a global variable whose initial value indirectly referenced to a function through the bitcast instruction.
; global variable A has a reference to function func which will be a Dependencies for A.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only prepo-digest %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@A = local_unnamed_addr global i8* bitcast (i32 ()* @func to i8*), align 8
define i32 @func() #0 {
entry:
  ret i32 2
}

;CHECK: GO Name:A
;CHECK:     Initial Dependencies: [ func]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:func
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:func
;CHECK:     Final Dependencies: [ ]
;CHECK: GO Name:A
;CHECK:     Final Dependencies: [ func]
