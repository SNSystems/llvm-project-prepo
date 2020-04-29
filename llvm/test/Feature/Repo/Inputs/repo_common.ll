target triple = "x86_64-pc-linux-gnu-repo"

@.str = private unnamed_addr constant [13 x i8] c"Hello World\0A\00", align 1, !repo_ticket !0

!repo.definitions = !{!0}

!0 = !RepoDefinition(name: ".str", digest: [16 x i8] c"\FE\15n\B4!\B1\FC{(\E5^>\E5\E2\F5\00", linkage: private, visibility: default, pruned: false)
