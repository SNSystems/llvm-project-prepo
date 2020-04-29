; This test checks that the ticket file digests are the same between two time
; builds if the input file does not change.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj -debug-only repo-object %s -o %t1 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@a = global i32 0, align 4
@b = local_unnamed_addr global i32* @a, align 8, !repo_definition !0

!repo.definitions = !{!0}

!0 = !RepoDefinition(name: "b", digest: [16 x i8] c"~6\BE\1B\E6>\ED5s\17\B2\F6\8B\91\C8_", linkage: external, visibility: default, pruned: false)

;CHECK: compilation {{[0-9a-fA-F]+}} exists. skipping

