; Test that we correctly generate contributions from store instruction.
; function main has a reference to global variable Var which will be a Contributions for main.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -debug-only prepo-digest %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@Var = global i32 1, align 4
define i32 @main() {
entry:
  store i32 2, i32* @Var, align 4
  ret i32 2
}

;CHECK: GO Name:Var
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ ]
;CHECK: GO Name:main
;CHECK:     Initial Dependencies: [ ]
;CHECK:     Contributions: [ Var]
;CHECK: GO Name:main
;CHECK:     Final Dependencies: [ Var]
;CHECK: GO Name:Var
;CHECK:     Final Dependencies: [ main]
