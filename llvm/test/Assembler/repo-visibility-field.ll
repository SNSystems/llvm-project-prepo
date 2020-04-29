; RUN: not llvm-as < %s 2>&1 | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@v = global i32 0, !repo_ticket !0

!0 = !RepoDefinition(name: "v", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: external, visibility: external, pruned: 0)

; CHECK: Invalid visibility type!
