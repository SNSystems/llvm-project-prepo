; The 'insertvalue' instruction hash is calculated incorrectly,
; which prevent the repository pruning.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -o %t
; RUN: env REPOFILE=%t.db opt -S %t | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@.str = private unnamed_addr constant [9 x i8] c"test.cpp\00", align 1

define internal void @__cxx_global_var_init() section ".text.startup" personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %exn.slot = alloca i8*
  %ehselector.slot = alloca i32

  %exn = load i8*, i8** %exn.slot, align 8
  %sel = load i32, i32* %ehselector.slot, align 4
  %lpad.val = insertvalue { i8*, i32 } undef, i8* %exn, 0
  %lpad.val1 = insertvalue { i8*, i32 } %lpad.val, i32 %sel, 1
  resume { i8*, i32 } %lpad.val1
}

declare i32 @__gxx_personality_v0(...)

;CHECK: !0 = !RepoDefinition(name: ".str",
;CHECK-NEXT: !1 = !RepoDefinition(name: "__cxx_global_var_init",
;CHECK-NOT: !2 = !RepoDefinition(name:
