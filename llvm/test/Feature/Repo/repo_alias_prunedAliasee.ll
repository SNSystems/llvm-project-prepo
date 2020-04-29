; Test an alias if its aliasee is pruned.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple=x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

@foo = weak global i32 0

@bar = alias i32, i32* @foo

@foo2 = weak global i32 0

@bar2 = alias i32, i32* @foo2

;CHECK: !RepoDefinition(name: "foo", digest: [16 x i8] c"{{.*}}", linkage: weak, visibility: default, pruned: false)
;CHECK: !RepoDefinition(name: "foo2", digest: [16 x i8] c"{{.*}}", linkage: weak, visibility: default, pruned: true)
;CHECK: !RepoDefinition(name: "bar", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: true)
;CHECK: !RepoDefinition(name: "bar2", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: true)
