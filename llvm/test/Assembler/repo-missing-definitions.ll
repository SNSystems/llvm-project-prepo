; RUN: not --crash llc -filetype=obj < %s 2>&1 | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

; Function Attrs: noinline nounwind optnone uwtable
define i32 @sum(i32 %a, i32 %b) #0 !repo_definition !0 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  %0 = load i32, i32* %a.addr, align 4
  %1 = load i32, i32* %b.addr, align 4
  %add = add nsw i32 %0, %1
  ret i32 %add
}

!0 = !RepoDefinition(name: "sum", digest: [16 x i8] c"qd\BD6r\8A=\BB\05\8B\D8.\AA\BA\04P", linkage: external, visibility: default, pruned: false)

; CHECK: Failed to get 'repo.definitions' module metadata!

