; Check Repo debug line prunning.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %S/Inputs/repo_debug_loc.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

define dso_local i32 @Test() !dbg !6 {
entry:
  ret i32 1, !dbg !8
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang", isOptimized: false, runtimeVersion: 0, emissionKind: LineTablesOnly, enums: !2)
!1 = !DIFile(filename: "Fn.c", directory: "C:\5CMyWork\5Crepo_bug\5Cbug41")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = distinct !DISubprogram(name: "Test", scope: !1, file: !1, line: 1, type: !7, isLocal: false, isDefinition: true, scopeLine: 1, isOptimized: false, unit: !0, retainedNodes: !2)
!7 = !DISubroutineType(types: !2)
!8 = !DILocation(line: 2, column: 3, scope: !6)

;CHECK: !RepoDefinition(name: "Test", digest: [16 x i8] c"{{.+}}", linkage: external, visibility: default, pruned: true)
