; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

; CHECK: !0 = !RepoDefinition(name: "fp_foo", digest: [16 x i8] c"[[FP_FOO:.+]]", linkage: external, visibility: default, pruned: false)
; CHECK-NOT: !1 = !RepoDefinition(name: "fp_bar", digest: [16 x i8] c"[[FP_FOO]]", linkage: external, visibility: default, pruned: false)
; CHECK-NOT: !2 = !RepoDefinition(name: "fp_baz", digest: [16 x i8] c"[[FP_FOO]]", linkage: external, visibility: default, pruned: false)
@fp_foo = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @foo to i32 (...)*)], align 8
@fp_bar = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @bar to i32 (...)*)], align 8
@fp_baz = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @baz to i32 (...)*)], align 8

; CHECK: !3 = !RepoDefinition(name: "a", digest: [16 x i8] c"[[A:.+]]", linkage: internal, visibility: default, pruned: false)
; CHECK-NOT: !4 = !RepoDefinition(name: "b", digest: [16 x i8] c"[[A]]", linkage: internal, visibility: default, pruned: false)
; CHECK-NOT: !5 = !RepoDefinition(name: "c", digest: [16 x i8] c"[[A]]", linkage: internal, visibility: default, pruned: false)
@a = internal global i32 1, align 4
@b = internal global i32 1, align 4
@c = internal global i32 2, align 4

; CHECK: !6 = !RepoDefinition(name: "vp_a", digest: [16 x i8] c"[[VP_A:.+]]", linkage: external, visibility: default, pruned: false)
; CHECK-NOT: !7 = !RepoDefinition(name: "vp_b", digest: [16 x i8] c"[[VP_A]]", linkage: external, visibility: default, pruned: false)
; CHECK-NOT: !8 = !RepoDefinition(name: "vp_c", digest: [16 x i8] c"[[VP_A]]", linkage: external, visibility: default, pruned: false)
@vp_a = global [1 x i32*] [i32* @a], align 8
@vp_b = global [1 x i32*] [i32* @b], align 8
@vp_c = global [1 x i32*] [i32* @c], align 8

; CHECK: !9 = !RepoDefinition(name: "foo", digest: [16 x i8] c"[[FOO:.+]]", linkage: internal, visibility: default, pruned: false)
; CHECK: !10 = !RepoDefinition(name: "bar", digest: [16 x i8] c"[[FOO]]", linkage: internal, visibility: default, pruned: false)
; CHECK-NOT: !11 = !RepoDefinition(name: "baz", digest: [16 x i8] c"[[FOO]]", linkage: internal, visibility: default, pruned: false)
define internal i32 @foo() {
entry:
  ret i32 1
}
define internal i32 @bar() {
entry:
  ret i32 1
}
define internal i32 @baz() {
entry:
  ret i32 2
}
