; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj -debug-only repo-object %s -o %t 2>&1 | FileCheck %s

; REQUIRES: asserts

target datalayout = "e-m:r-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu-repo"

@.str = private unnamed_addr constant [14 x i8] c"hello, world\0A\00", align 1, !repo_definition !0

define i32 @main() !repo_definition !1 {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str, i32 0, i32 0))
  ret i32 0
}

declare i32 @printf(i8*, ...)

!repo.definitions = !{!0, !1}

!0 = !RepoDefinition(name: ".str", digest: [16 x i8] c"\90\FB$Vd\95\00\A7C\FD\C5\B8\94v\19\08", linkage: private, visibility: default, pruned: false)
!1 = !RepoDefinition(name: "main", digest: [16 x i8] c"\B5\E18\DD4~}\D4\0C\B2\C5I\AF\0E\F2q", linkage: external, visibility: default, pruned: false)

;CHECK: section type 'mergeable_1_byte_c_string' and alignment 1
