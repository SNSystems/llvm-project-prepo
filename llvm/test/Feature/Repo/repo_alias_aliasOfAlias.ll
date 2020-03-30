; Check the pruning for an alias (C) whose aliasee is another alias (B).
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s -o %t
; RUN: env REPOFILE=%t.db llc -filetype=obj %t
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"


;CHECK: declare void @A()
@A = alias void (), void ()* @D

;CHECK: declare void @B()
@B = alias void (), bitcast (void ()* @D to void ()*)

;CHECK: declare void @C()
@C = alias void (), void ()* @B

define void @D() {
entry:
  ret void
}
