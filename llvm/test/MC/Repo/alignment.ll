; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj -debug-only repo-object %s -o %t 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

define double @_Z4testv() !repo_definition !0 {
entry:
  ret double 1.900000e+00
}

!repo.definitions = !{!0}

!0 = !RepoDefinition(name: "_Z4testv", digest: [16 x i8] c"\B1\8C\0A\BD\C2P~\D7\12xf\7F\B5\D2a\88", linkage: external, visibility: default,  pruned: false)

;CHECK: section type 'mergeable_const_8' and alignment 8
;CHECK: section type 'text' and alignment 16
