target triple = "x86_64-pc-linux-gnu-repo"

define i32 @A() !repo_ticket !0 {
entry:
  %call = call i32 @D()
  ret i32 %call
}

define i32 @B() !repo_ticket !1 {
entry:
  %call = call i32 @E()
  ret i32 %call
}

define i32 @C() !repo_ticket !2 {
entry:
  %call = call i32 @E()
  %add = add nsw i32 %call, 1
  ret i32 %add
}

define internal i32 @D() {
entry:
  %call = call i32 @E()
  ret i32 %call
}

define internal i32 @E() {
entry:
  ret i32 1
}


!repo.tickets = !{!0, !1, !2}

!0 = !TicketNode(name: "A", digest: [16 x i8] c"v\15d\B9\DB\13\FB\0E\E56pm\9B\AA\C7\9C", linkage: external, pruned: false)
!1 = !TicketNode(name: "B", digest: [16 x i8] c"\E5\EB\97\DFy\B4\A7:-D\C6S1\D6\F4\82", linkage: external, pruned: false)
!2 = !TicketNode(name: "C", digest: [16 x i8] c"jQ7l\E6x,\1Ad\AD\BCUt\8De\B0", linkage: external, pruned: false)
