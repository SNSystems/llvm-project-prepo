; Check the weak symbol binding for the following 4 cases:
;   Case 1: when the symbol is declared as extern_weak function `h()`, it is referenced by global variable `c`.
;   Case 2: when the symbol is declared as extern_weak function `h()`, it is referenced by function `g`.
;   Case 3: when the symbol is declared as extern_weak variable `a`, it is referenced by global variable `b`.
;   Case 4: when the symbol is declared as extern_weak variable `a`, it is referenced by function `use-a`.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -o %t
; RUN: env REPOFILE=%t.db llc -filetype=obj -debug-only repo-object %t -o %t1 2>&1  | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@a = extern_weak global i32
@b = global i32* @a
define i32 * @use_a() {
    ret i32* @a
}

declare extern_weak i32 @h()
@c = global i32 () * @h
define i32 ()* @g() {
entry:
	ret i32 ()* @h
}

;CHECK-DAG: fragment {{[0-9a-fA-F]+}} has an xfixup to an external weak symbol 'a'
;CHECK-DAG: fragment {{[0-9a-fA-F]+}} has an xfixup to an external weak symbol 'a'
;CHECK-DAG: fragment {{[0-9a-fA-F]+}} has an xfixup to an external weak symbol 'h'
;CHECK-DAG: fragment {{[0-9a-fA-F]+}} has an xfixup to an external weak symbol 'h'
;CHECK-NOT: fragment {{[0-9a-fA-F]+}} has an xfixup to an external weak symbol
