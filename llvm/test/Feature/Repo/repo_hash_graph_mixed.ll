; Test GO's information of Dependencies and Contributions.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only ir -mtriple x86_64-pc-linux-gnu-repo %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@G = internal global i32 0, align 4
define void @setTo(i32* %P, i32 %V) {
entry:
    %0 = load i32, i32* %P, align 4
    %add = add nsw i32 %0, %V
    store i32 %add, i32* %P, align 4
    ret void
}
define i32 @test() {
entry:
    call void @setTo(i32* @G, i32 2)
    %0 = load i32, i32* @G, align 4
    ret i32 %0
}


;CHECK: GO Name:G
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:setTo
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:test
;CHECK:     Initial Dependencies: [ setTo]
;CHECK:     Contributions: [ G]
;CHECK: GO Name:setTo
;CHECK:     Final Dependencies: [ ]
;CHECK: GO Name:test
;CHECK:     Final Dependencies: [ setTo,G]
;CHECK: GO Name:G
;CHECK:     Final Dependencies: [ test]
