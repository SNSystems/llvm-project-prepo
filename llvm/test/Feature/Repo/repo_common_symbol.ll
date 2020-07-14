; Check Repo supports common symbols.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t
; RUN: repo2obj %t --repo %t.db -o %t1
; RUN: llvm-readobj -t %t1 | FileCheck %s


target triple = "x86_64-pc-linux-gnu-repo"

@a = common global i32 0, align 4, !repo_definition !0

!repo.definitions = !{!0}
!0 = !RepoDefinition(name: "a", digest: [16 x i8] c"\22\CE\E5\A0\D2t\C9h\9D\D1M\15\F7L\B4\A2", linkage: common, visibility: default, pruned: false)

; CHECK: Name: a (
; CHECK-NEXT: Value: 0x4
; CHECK-NEXT: Size: 4
; CHECK-NEXT: Binding: Global (0x1)
; CHECK-NEXT: Type: Object (0x1)
; CHECK-NEXT: Other: 0
; CHECK-NEXT: Section: Common (0xFFF2)
