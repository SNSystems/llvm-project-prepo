; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -o %t.ll
; RUN: env REPOFILE=%t.db llc --filetype=obj %t.ll -o %t.o
; RUN: env REPOFILE=%t.db repo2obj --no-subsections -o %t.elf %t.o
; RUN: llvm-readobj --sections  %t.elf | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

define void @f() {
  call void @j()
  ret void
}
; CHECK-DAG: Name: .text (

define void @g() {
  ret void
}


@h = global i32 1, align 4
; CHECK-DAG: Name: .data (


$j = comdat any
define linkonce_odr void @j() comdat {
  ret void
}
; CHECK-DAG: Name: .text.j (

; CHECK-NOT: Name: .text.f (
; CHECK-NOT: Name: .text.g (
; CHECK-NOT: Name: .data.h (
