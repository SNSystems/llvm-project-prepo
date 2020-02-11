; TODO: need a new yaml2repo tool to remove the llc tool dependence.
;       Once we have yaml2repo tool, the yaml2repo command will
;       be used (instead of llc) to generate the repo object file. 
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t.o
; RUN: repo2obj %t.o --repo  %t.db -o %t.elf
; RUN: llvm-readobj -elf-section-groups %t.elf | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

%class.btVector3 = type { [4 x float] }

$f = comdat any

define linkonce_odr void @f(%class.btVector3* %this, float* dereferenceable(4) %s) comdat align 2 !repo_ticket !0 {
entry:
  %s.addr = alloca float*, align 8
  %ref.tmp = alloca float, align 4
  %0 = load float*, float** %s.addr, align 8
  %1 = load float, float* %0, align 4
  %div = fdiv float 1.000000e+00, %1
  store float %div, float* %ref.tmp, align 4
  ret void
}

!repo.tickets = !{!0}

!0 = !TicketNode(name: "f", digest: [16 x i8] c"C\8A\B2\B7\D7=\EA\DA\91\CAhn\0A\C3B\82", linkage: linkonce_odr, visibility: default, pruned: false)

;CHECK:   Group {
;CHECK:     Name: .group ({{[0-9]+}})
;CHECK:     Index: {{[0-9]+}}
;CHECK:     Type: COMDAT (0x1)
;CHECK:     Signature: f
;CHECK:     Section(s) in group [
;CHECK-DAG:      .rela.text.f ({{[0-9]+}})
;CHECK-DAG:      .rodata.cst4.f ({{[0-9]+}})
;CHECK-DAG:      .text.f ({{[0-9]+}})
;CHECK:     ]
;CHECK:   }
