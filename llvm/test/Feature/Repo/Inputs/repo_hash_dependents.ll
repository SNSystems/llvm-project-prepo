target triple = "x86_64-pc-linux-gnu-repo"

declare i32 @printf(i8*, ...)

@.str = private unnamed_addr constant [13 x i8] c"Hello World\0A\00", align 1

define i32 @foo() !repo_ticket !0 {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str, i32 0, i32 0))
  ret i32 0
}

!repo.tickets = !{!0}

!0 = !TicketNode(name: "foo", digest: [16 x i8] c"\C5\E6\1D&$i{\1F\5C\D5\C6\F5\AA\1F\CBo", linkage: external, pruned: false)
