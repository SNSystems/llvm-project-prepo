; Test the deepest level DIfile.
; A function 'test', which calls a function 'baz' and 'baz' calls another function 'foo'.
; Both funcitons 'baz' and 'foo' are inlined into the function 'test'. The file name and
; directory of the function 'foo' are contributed to the 'test' repo hash calculation.
;
; The testcase includes three steps:
; Step 1: Generate the repo IR code which contains the TicketNode metadata;
; Step 2: Create the database 'clang.db' which contains all Tickets.
; Step 3: Run 'opt' cmd for this file, check the funciton 'Fn' is not optimised;

; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %S/Inputs/repo_multiple_levels_inlinedAt.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s
target triple = "x86_64-pc-linux-gnu-elf"

define dso_local void @test() !dbg !6 {
entry:
  %x.i.i.i = alloca double, align 8
  store double 1.000000e+00, double* %x.i.i.i, align 8, !dbg !8
  %0 = load double, double* %x.i.i.i, align 8, !dbg !17
  %inc.i.i.i = fadd double %0, 1.000000e+00, !dbg !17
  store double %inc.i.i.i, double* %x.i.i.i, align 8, !dbg !17
  %conv.i.i.i = fptosi double %inc.i.i.i to i32, !dbg !17
  ret void, !dbg !18
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang", isOptimized: false, runtimeVersion: 0, emissionKind: LineTablesOnly, enums: !2)
!1 = !DIFile(filename: "inline.c", directory: "/home/maggie/case")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = distinct !DISubprogram(name: "test", scope: !1, file: !1, line: 7, type: !7, isLocal: false, isDefinition: true, scopeLine: 7, isOptimized: false, unit: !0, retainedNodes: !2)
!7 = !DISubroutineType(types: !2)
!8 = !DILocation(line: 2, column: 10, scope: !9, inlinedAt: !11)
!9 = distinct !DISubprogram(name: "foo", scope: !10, file: !10, line: 1, type: !7, isLocal: false, isDefinition: true, scopeLine: 1, isOptimized: false, unit: !0, retainedNodes: !2)
!10 = !DIFile(filename: "./header/foo1.h", directory: "/home/maggie/case2")
!11 = distinct !DILocation(line: 4, column: 10, scope: !12, inlinedAt: !14)
!12 = distinct !DISubprogram(name: "baz", scope: !13, file: !13, line: 3, type: !7, isLocal: false, isDefinition: true, scopeLine: 3, isOptimized: false, unit: !0, retainedNodes: !2)
!13 = !DIFile(filename: "./header/header.h", directory: "/home/maggie/case1")
!14 = distinct !DILocation(line: 4, column: 10, scope: !15, inlinedAt: !16)
!15 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 3, type: !7, isLocal: false, isDefinition: true, scopeLine: 3, isOptimized: false, unit: !0, retainedNodes: !2)
!16 = distinct !DILocation(line: 8, column: 3, scope: !6)
!17 = !DILocation(line: 3, column: 10, scope: !9, inlinedAt: !11)
!18 = !DILocation(line: 9, column: 1, scope: !6)

;CHECK: !RepoDefinition(name: "test", digest: [16 x i8] c"{{.+}}", linkage: external, visibility: default, pruned: false)

