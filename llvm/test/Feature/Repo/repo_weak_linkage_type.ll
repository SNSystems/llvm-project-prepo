; RUN: rm -rf %t && mkdir -p %t
; RUN: env REPOFILE=%t/repo.db llc -filetype=obj -mtriple=x86_64-pc-linux-gnu-repo -o %t/ticket.o %s

@WeakData = weak global i32 0, align 4, !repo_definition !0

!repo.definitions = !{!0}
!0 = !RepoDefinition(name: "WeakData",
                     digest: [16 x i8] c"W6c\E4\A2\C0\0BH\D2_\C7\E5\91\E9\13\C9",
                     linkage: weak,
                     visibility: default,
                     pruned: false)

