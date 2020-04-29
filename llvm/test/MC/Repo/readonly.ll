; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t

target triple = "x86_64-pc-linux-gnu-repo"

define double @_Z4testv() !repo_definition !0 {
entry:
  %a = alloca i64, align 8
  %0 = load i64, i64* %a, align 8
  %conv = uitofp i64 %0 to double
  %add = fadd double %conv, 1.900000e+00
  ret double %add
}

!repo.definitions = !{!0}

!0 = !RepoDefinition(name: "_Z4testv", digest: [16 x i8] c"1\DD\AFx\94\B0\12\0D\0C\94\C60B\0F\CC2", linkage: external, visibility: default, pruned: false)
