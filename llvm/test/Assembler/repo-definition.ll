; RUN: llvm-as < %s | llvm-dis | llvm-as | llvm-dis | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

; CHECK: @_ZL4fact = internal global i32 1, comdat, align 4, !repo_ticket !0
$_ZL4fact = comdat any
@_ZL4fact = internal global i32 1, align 4, !repo_ticket !0, comdat($_ZL4fact)

; CHECK: define weak i32 @_Z9factorialv() !repo_ticket !1
define weak i32 @_Z9factorialv() !repo_ticket !1 {
entry:
  %0 = load i32, i32* @_ZL4fact, align 4
  ret i32 %0
}

; CHECK: define i32 @main() !repo_ticket !2
define i32 @main() !repo_ticket !2 {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  %call = call i32 @_Z9factorialv()
  ret i32 %call
}

!repo.tickets = !{!0, !1, !2}

; CHECK: !0 = !RepoDefinition(name: "_ZL4fact", digest: [16 x i8] c"fRl\F0h\D3\10i~\87Ny\E5\FD%c", linkage: internal, visibility: default, pruned: false)
!0 = !RepoDefinition(name: "_ZL4fact", digest: [16 x i8] c"fRl\F0h\D3\10i~\87Ny\E5\FD%c", linkage: internal, visibility: default, pruned: false)
; CHECK: !1 = !RepoDefinition(name: "_Z9factorialv", digest: [16 x i8] c"\05wF\F7\ABl\D6+\CCl\D7p\0D\C4^\91", linkage: weak, visibility: default, pruned: false)
!1 = !RepoDefinition(name: "_Z9factorialv", digest: [16 x i8] c"\05wF\F7\ABl\D6+\CCl\D7p\0D\C4^\91", linkage: weak, visibility: default, pruned: false)
; CHECK: !2 = !RepoDefinition(name: "main", digest: [16 x i8] c"\C0\F7\C1|\C6\9F\BBI\BA\B4s\A9\8Av\DF\A8", linkage: external, visibility: default, pruned: false)
!2 = !RepoDefinition(name: "main", digest: [16 x i8] c"\C0\F7\C1|\C6\9F\BBI\BA\B4s\A9\8Av\DF\A8", linkage: external, visibility: default, pruned: false)
