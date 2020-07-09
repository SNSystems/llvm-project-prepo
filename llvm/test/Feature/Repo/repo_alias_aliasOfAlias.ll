; Check the pruning for an alias (C) whose aliasee is another alias (B).
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s -o %t
; RUN: env REPOFILE=%t.db llc -filetype=obj %t
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

;CHECK-NOT: @A = alias void ()
;CHECK-NOT: declare void @A()
@A = alias void (), void ()* @D

;CHECK-NOT: @B = alias void ()
;CHECK-NOT: declare void @B()
@B = alias void (), bitcast (void ()* @D to void ()*)

;CHECK-NOT: @C = alias void ()
;CHECK-NOT: declare void @C()
@C = alias void (), void ()* @B

;CHECK-NOT: @D = alias void ()
;CHECK-NOT: declare void @D()
define void @D() {
entry:
  ret void
}

;CHECK: !0 = !RepoDefinition(name: {{.*}}, digest: [16 x i8] c"[[FOO:.+]]", linkage: external, visibility: default, pruned: true)
;CHECK: !1 = !RepoDefinition(name: {{.*}}, digest: [16 x i8] c"[[FOO]]", linkage: external, visibility: default, pruned: true)
;CHECK: !2 = !RepoDefinition(name: {{.*}}, digest: [16 x i8] c"[[FOO]]", linkage: external, visibility: default, pruned: true)
;CHECK: !3 = !RepoDefinition(name: {{.*}}, digest: [16 x i8] c"[[FOO]]", linkage: external, visibility: default, pruned: true)

