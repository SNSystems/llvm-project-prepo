; Test that we correctly generate contributions from load and store instructions.
; global variable B has a reference to global variable A which will be a Contribution for B.
; function setB has a reference to global variable B which will be a Contributions for setB.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only prepo-digest %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@A = global i32 1, align 4
@B = global i32* @A, align 8

define void @setB() {
entry:
    %0 = load i32*, i32** @B, align 8
    store i32 3, i32* %0, align 4
    ret void
}

;CHECK: GO Name:A
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:B
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ A]
;CHECK: GO Name:setB
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ B]
;CHECK: GO Name:setB
;CHECK:     Final Dependencies: [ B]
;CHECK: GO Name:A
;CHECK:     Final Dependencies: [ B]
;CHECK: GO Name:B
;CHECK:     Final Dependencies: [ A,setB]
