; RUN: opt -passes=elim-avail-extern -S < %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

; CHECK: declare void @f()
define available_externally void @f() !repo_definition !2 {
  ret void
}

; CHECK: available_externally void @k()
define available_externally void @k() !repo_definition !0 {
  ret void
}

define void @g() !repo_definition !1 {
  call void @f()
  call void @k()
  ret void
}

!repo.tickets = !{!0, !1}

!0 = !RepoDefinition(name: "k", digest: [16 x i8] c")\03/o;t+3Q\0A\1D\BBD\9D\999", linkage: internal, visibility: default, pruned: true)
!1 = !RepoDefinition(name: "g", digest: [16 x i8] c"\86bx\AB\19\A64\98\91X\DE\FA\8C\7F{\E7", linkage: external, visibility: default, pruned: false)
!2 = !RepoDefinition(name: "f", digest: [16 x i8] c")\03/o;t+3Q\0A\1D\BBD\9D\999", linkage: available_externally, visibility: default, pruned: true)
