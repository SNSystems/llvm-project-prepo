; Check Repo debug line contents.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t
; RUN: repo2obj %t --repo %t.db -o %t1
; RUN: llvm-dwarfdump -debug-line %t1 | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @ultimate_answer() !dbg !9 !repo_definition !7 {
entry:
  ret i32 43, !dbg !13
}

define i32 @main() !dbg !14 !repo_definition !8 {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  %call = call i32 @ultimate_answer(), !dbg !15
  ret i32 %call, !dbg !16
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}
!repo.definitions = !{!7, !8}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 8.0.0 (https://github.com/SNSystems/clang-prepo.git e1264723eba24003590821b53c60d5268c928acb) (https://github.com/SNSystems/llvm-prepo.git 5a4ad96b98b02594566d6d1506d75bd760322d15)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2)
!1 = !DIFile(filename: "test.c", directory: "C:\5CMyWork\5Crepo_bug\5Cbug49")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 8.0.0 (https://github.com/SNSystems/clang-prepo.git e1264723eba24003590821b53c60d5268c928acb) (https://github.com/SNSystems/llvm-prepo.git 5a4ad96b98b02594566d6d1506d75bd760322d15)"}
!7 = !RepoDefinition(name: "ultimate_answer", digest: [16 x i8] c"\A9'\00\E6\97nq\C0\BC?\B8\B6,\D6!\CA", linkage: external, visibility: default, pruned: false)
!8 = !RepoDefinition(name: "main", digest: [16 x i8] c"7|\F2o%\82\DB\045\FDx\FF\C4E\EC\D2", linkage: external, visibility: default, pruned: false)
!9 = distinct !DISubprogram(name: "ultimate_answer", scope: !1, file: !1, line: 1, type: !10, isLocal: false, isDefinition: true, scopeLine: 1, isOptimized: false, unit: !0, retainedNodes: !2)
!10 = !DISubroutineType(types: !11)
!11 = !{!12}
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !DILocation(line: 2, column: 5, scope: !9)
!14 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 4, type: !10, isLocal: false, isDefinition: true, scopeLine: 4, isOptimized: false, unit: !0, retainedNodes: !2)
!15 = !DILocation(line: 5, column: 12, scope: !14)
!16 = !DILocation(line: 5, column: 5, scope: !14)

; CHECK:      .debug_line contents:
; CHECK:      Line table prologue:
; CHECK-NEXT:     total_length: 0x00000052
; CHECK-NEXT:           format: DWARF32
; CHECK-NEXT:          version: 4
; CHECK-NEXT:  prologue_length: 0x0000001e
; CHECK-NEXT:  min_inst_length: 1
; CHECK-NEXT: max_ops_per_inst: 1
; CHECK-NEXT:  default_is_stmt: 1
; CHECK-NEXT:        line_base: -5
; CHECK-NEXT:       line_range: 14
; CHECK-NEXT:      opcode_base: 13
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_copy] = 0
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_advance_pc] = 1
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_advance_line] = 1
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_set_file] = 1
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_set_column] = 1
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_negate_stmt] = 0
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_set_basic_block] = 0
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_const_add_pc] = 0
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_fixed_advance_pc] = 1
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_set_prologue_end] = 0
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_set_epilogue_begin] = 0
; CHECK-NEXT: standard_opcode_lengths[DW_LNS_set_isa] = 1
; CHECK:      1 0  1 0 0  is_stmt
; CHECK:      2 5  1 0 0  is_stmt prologue_end
; CHECK:      2 5  1 0 0  is_stmt end_sequence
; CHECK:      4 0  1 0 0  is_stmt
; CHECK:      5 12 1 0 0  is_stmt prologue_end
; CHECK:      5 5  1 0 0
; CHECK:      5 5  1 0 0  end_sequence
