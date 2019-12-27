; Test a special alias of __tls_init.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple=x86_64-pc-linux-gnu-repo %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

%class.foo = type { i8 }
@foo_instance = thread_local global %class.foo zeroinitializer, align 1
@_ZTH12foo_instance = alias void (), void ()* @__tls_init

define internal void @__tls_init() {
  ret void
}

define weak_odr hidden %class.foo* @_ZTW12foo_instance(){
  call void @_ZTH12foo_instance()
  ret %class.foo* @foo_instance
}

;CHECK: !TicketNode(name: "_ZTH12foo_instance", digest: [16 x i8] c"{{.*}}", linkage: external, visibility: default, pruned: true)
