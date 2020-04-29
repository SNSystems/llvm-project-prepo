; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t

target triple = "x86_64-pc-linux-gnu-repo"

@GVar = global i32 0, align 4

define i32 @GFn(i32 %a, i32 %b) {
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

$_ZL4fact = comdat any
@_ZL4fact = internal global i32 1, align 4, !fragment !0, comdat($_ZL4fact)

!repo.definitions = !{!0}

!0 = !RepoDefinition(name: "_ZL4fact", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: internal, visibility: default, pruned: false)


