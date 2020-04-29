; Test the handling of global aliases for function.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db opt -S %S/Inputs/repo_shared_GOs.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @Fn() {
entry:
  ret i32 2
}

@bar = alias i32(), i32()* @Fn
define i32 @test() {
entry:
  %0 = call i32() @bar()
  ret i32 %0
}

;CHECK: !RepoDefinition(name: "test", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: false)
