; Check repo-fragments options, which include:
;    1) Check the default case by dumping all of the names from a compilation;
;    2) Check the -names option by explicitly requesting specific definitions by name;
;    3) Check the -digest-only option;
;    4) Check the -comma option.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db clang -c --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t.o
; RUN: repo-fragments -repo=%t.db %t.o | FileCheck --check-prefix=BASIC %s
; RUN: repo-fragments -repo=%t.db %t.o v  | FileCheck --check-prefix=VONLY %s
; RUN: repo-fragments -repo=%t.db -digest-only %t.o | FileCheck --check-prefix=DIGESTONLY %s
; RUN: repo-fragments -repo=%t.db -d %t.o | FileCheck --check-prefix=DIGESTONLY %s
; RUN: repo-fragments -repo=%t.db -comma %t.o | FileCheck --check-prefix=COMMA %s
; RUN: repo-fragments -repo=%t.db -c %t.o | FileCheck --check-prefix=COMMA %s
;
; BASIC: f: {{([[:xdigit:]]{32})}}
; BASIC: v: {{([[:xdigit:]]{32})}}
; VONLY-NOT: f: {{([[:xdigit:]]{32})}}
; VONLY: v: {{([[:xdigit:]]{32})}}
; DIGESTONLY: {{([[:xdigit:]]{32})}}
; CHECK2: {{([[:xdigit:]]{32})}}
; COMMA: f: {{([[:xdigit:]]{32})}},v: {{([[:xdigit:]]{32})}}
;
target triple = "x86_64-pc-linux-gnu-repo"

@v = global i32 1
define void @f() {
entry:
  ret void
}
