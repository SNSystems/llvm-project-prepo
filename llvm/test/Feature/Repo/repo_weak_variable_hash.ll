; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@GlobalData = external dso_local global i32, align 4
@WeakData = weak dso_local local_unnamed_addr global i32* @GlobalData, align 8
@Data = dso_local local_unnamed_addr global i32* null, align 8

;CHECK:      !0 = !RepoDefinition(name: "WeakData", digest: [16 x i8]  c"{{.+}}", linkage: weak, visibility: default, pruned: false)
;CHECK-NEXT: !1 = !RepoDefinition(name: "Data", digest: [16 x i8]  c"{{.+}}", linkage: external, visibility: default, pruned: false)
