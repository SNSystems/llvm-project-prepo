; Check repo-fragments options, which include:
;    1) Check the default case by dumping all of the names from a compilation;
;    2) Check the -names option by explicitly requesting specific definitions by name;
;    3) Check the -digest-only option;
;    4) Check the -comma option.
;
; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db clang -c --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t.o
; RUN: env REPOFILE=%t.db repo-fragments %t.o -repo=%t.db | FileCheck --check-prefix=CHECK0 %s
; RUN: env REPOFILE=%t.db repo-fragments %t.o -names=v -repo=%t.db | FileCheck --check-prefix=CHECK1 %s
; RUN: env REPOFILE=%t.db repo-fragments %t.o -digest-only -repo=%t.db | FileCheck --check-prefix=CHECK2 %s
; RUN: env REPOFILE=%t.db repo-fragments %t.o -comma -repo=%t.db | FileCheck --check-prefix=CHECK3 %s
;
; CHECK0: f: {{([[:xdigit:]]{32})}}
; CHECK0: v: {{([[:xdigit:]]{32})}}
; CHECK1-NOT: f: {{([[:xdigit:]]{32})}}
; CHECK1: v: {{([[:xdigit:]]{32})}}
; CHECK2: {{([[:xdigit:]]{32})}}
; CHECK2: {{([[:xdigit:]]{32})}}
; CHECK3: f: {{([[:xdigit:]]{32})}},v: {{([[:xdigit:]]{32})}}
;
target triple = "x86_64-pc-linux-gnu-repo"

@v = global i32 1
define void @f() {
entry:
  ret void
}
