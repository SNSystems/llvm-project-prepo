; Test alias of global variables and functions.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple=x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

@zed = global i32 42
@foo = alias i32, i32* @zed
@foo2 = alias i16, bitcast (i32* @zed to i16*)

define i32 @a() {
  ret i32 0
}
@b = internal alias i32 (), i32 ()* @a

;CHECK: !TicketNode(name: "zed", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: false)
;CHECK: !TicketNode(name: "a", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: false)
;CHECK: !TicketNode(name: "foo", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: true)
;CHECK: !TicketNode(name: "foo2", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: true)
;CHECK: !TicketNode(name: "b", digest: [16 x i8] c"{{.*}}", linkage: internal, visibility: default, pruned: true)
