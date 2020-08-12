; This test is used to check the hash calculation for the contributions.
;
; All GOs' information are shown below:
;    | GO Name | Initial Dependencies | Contributions |
;    |   Var1  |     [ Func1 ]        |      [ ]      |
;    |   Var2  |       [ ]            |      [ ]      |
;    |   Func1 |       [ ]            |      [ ]      |
;    |   Main  |     [ Func2 ]        |     [ Var2 ]  |
;    |   Func2 |       [ ]            |     [ Var1 ]  |
;
; This test checks that `Main` digest is changed when `Var2` is modified.
;
; The test invokes five steps:
;  1) Compile the initial code `Inputs/repo_contributions_dependencies_loop.ll` to %t;
;  2) Compile `repo_hash_contributions_var.ll` to %t1.
;  3) Dump the digest of `Main` in %t.
;  4) Dump the digest of `Main` in %t1.
;  5) Check the digests of `Main` are different since the variable `Var2` is changed.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db clang -c -O0 --target=x86_64-pc-linux-gnu-repo -x ir %S/Inputs/repo_contributions_dependencies_loop.ll -o %t
; RUN: env REPOFILE=%t.db clang -c -O0 --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t1
; RUN: env REPOFILE=%t.db repo-fragments %t Main -repo=%t.db -digest-only > %t.d
; RUN: env REPOFILE=%t.db repo-fragments %t1 Main -repo=%t.db -digest-only > %t1.d
; RUN: not diff %t.d %t1.d

declare i32 @F();

@Var1 = global i32 ()* @Func1, align 8
@Var2 = global i32 ()* @F, align 16

define i32 @Func1() {
entry:
  ret i32 2
}

define void @Main() {
entry:
  call void @Func2(i32()** @Var2)
  ret void
}

define void @Func2(i32()** %P) {
entry:
  %0 = load i32() *, i32()** @Var1, align 8
  store i32()* %0, i32() ** %P, align 8
  ret void
}
