; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

define available_externally i32 @foo() {
entry:
  ret i32 2
}

define void @bar() {
  %call = call i32 @foo()
  ret void
}

;CHECK: !0 = !RepoDefinition(name: "bar", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: false)
