; Test the inlined debug line information, which is considered to the repo hash calculation.
;
; Foo and Bar functions (defined in Inputs/repo_inline_debug_loc) are used as a reference
; funciton.  If the callee is changed by adding some tabs in the function body, the
; caller hash will change as well if the callee is inlined.
;
; Corresponding the source code for this IR:
;
; inline __attribute__ ((always_inline)) int Foo(double y) {
; /*column change*/ y++;
;   return y;
; }
;
; void Bar(double x) {
;   Foo(x);
; }
;
; The testcase includes three steps:
; Step 1: Generate the repo IR code which contains the RepoDefinition metadata (Inputs/Foo,Bar);
; Step 2: Create the database 'clang.db' which contains all Tickets.
; Step 3: Run 'opt' cmd for this file, check the funciton 'Fn' is not optimised;

; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %S/Inputs/repo_inline_debug_loc.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

define dso_local void @Bar(double %x) !dbg !6 {
entry:
  %y.addr.i = alloca double, align 8
  %x.addr = alloca double, align 8
  store double %x, double* %x.addr, align 8
  %0 = load double, double* %x.addr, align 8, !dbg !8
  store double %0, double* %y.addr.i, align 8
  %1 = load double, double* %y.addr.i, align 8, !dbg !9
  %inc.i = fadd double %1, 1.000000e+00, !dbg !9
  store double %inc.i, double* %y.addr.i, align 8, !dbg !9
  %2 = load double, double* %y.addr.i, align 8, !dbg !12
  %conv.i = fptosi double %2 to i32, !dbg !12
  ret void, !dbg !13
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang", isOptimized: false, runtimeVersion: 0, emissionKind: LineTablesOnly, enums: !2)
!1 = !DIFile(filename: "Fn.c", directory: "C:\5CMyWork\5Crepo_bug\5Cbug41")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = distinct !DISubprogram(name: "Bar", scope: !1, file: !1, line: 6, type: !7, isLocal: false, isDefinition: true, scopeLine: 6, flags: DIFlagPrototyped, isOptimized: false, unit: !0, retainedNodes: !2)
!7 = !DISubroutineType(types: !2)
!8 = !DILocation(line: 7, column: 7, scope: !6)
!9 = !DILocation(line: 2, column: 19, scope: !10, inlinedAt: !11)
!10 = distinct !DISubprogram(name: "Foo", scope: !1, file: !1, line: 1, type: !7, isLocal: false, isDefinition: true, scopeLine: 1, flags: DIFlagPrototyped, isOptimized: false, unit: !0, retainedNodes: !2)
!11 = distinct !DILocation(line: 7, column: 3, scope: !6)
!12 = !DILocation(line: 3, column: 10, scope: !10, inlinedAt: !11)
!13 = !DILocation(line: 8, column: 1, scope: !6)


;CHECK: !RepoDefinition(name: "Bar", digest: [16 x i8] c"{{.+}}", linkage: external, visibility: default, pruned: false)
