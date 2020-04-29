; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

%"Loc" = type { i32 }

%"OMP" = type { %"Loc", %"Loc", i32 }
%"OMPRead" = type { %"OMP" }
%"OMPWrite" = type { %"OMP" }

define %"OMP"* @_Read(%"OMPRead"* readnone) align 2 {
  %2 = getelementptr inbounds %"OMPRead", %"OMPRead"* %0, i64 0, i32 0
  ret %"OMP"* %2
}

define %"OMP"* @_Write(%"OMPWrite"* readnone) align 2 {
  %2 = getelementptr inbounds %"OMPWrite", %"OMPWrite"* %0, i64 0, i32 0
  ret %"OMP"* %2
}


;CHECK: !RepoDefinition(name: "_Write", digest: [16 x i8] c"{{.+}}", linkage: external, visibility: default, pruned: false)
