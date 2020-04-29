; RUN: not llvm-as < %s -o /dev/null 2>&1 | FileCheck %s
; Verify that repo.definitions is properly structured.
; repo.definitions takes a list of metadata entries.
; Each metadata entry can contain one definition only.

!repo.definitions = !{!0, !1}

!0 = !RepoDefinition(name: "factorial", digest: [16 x i8] c"+Th8\90\1D\9E/\A3\CF=\01\B3<v\DB", linkage: external, visibility: default, pruned: false)

!1 = !{i32 1}
; CHECK: assembly parsed, but does not verify as correct!
; CHECK-NEXT: invalid value for repo.definitions metadata entry operand (the operand should be a repo definition)
