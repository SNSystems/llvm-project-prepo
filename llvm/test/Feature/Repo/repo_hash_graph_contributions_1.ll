; Test that we correctly generate contributions from load and call instructions.
; global variable Str has a reference to global variable .str which will be a Contributions for Str.
; function sayHello has a reference to global variable Str which will be a Contributions for sayHello.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only ir %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@Str = internal global i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str, i32 0, i32 0), align 8
@.str = private unnamed_addr constant [14 x i8] c"Hello, World\0A\00", align 1

define void @sayHello() {
entry:
  %0 = load i8*, i8** @Str, align 8
  %call = call i32 (i8*, ...) @printf(i8* %0)
  ret void
}

declare i32 @printf(i8*, ...)

;CHECK: GO Name:Str
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ .str]
;CHECK: GO Name:.str
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:sayHello
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ Str]
;CHECK: GO Name:sayHello
;CHECK:     Final Dependencies: [ Str]
;CHECK: GO Name:Str
;CHECK:     Final Dependencies: [ .str,sayHello]
;CHECK: GO Name:.str
;CHECK:     Final Dependencies: [ Str]
