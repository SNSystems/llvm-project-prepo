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
; This test checks that `Var2` digest is changed when `Func1` is modified.
;
; The test invokes five steps:
;  1) Compile the initial code `Inputs/repo_contributions_dependencies_loop.ll` to %t.
;  2) Compile `repo_hash_contributions.ll` to %t1.
;  3) Dump the digest of `Var2` in %t.
;  4) Dump the digest of `Var2` in %t1.
;  5) Check the digests of `Var2` are different since the function `Func1` is changed.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db clang -c -O0 --target=x86_64-pc-linux-gnu-repo -x ir %S/Inputs/repo_contributions_dependencies_loop.ll -o %t
; RUN: env REPOFILE=%t.db clang -c -O0 --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t1
; RUN: env REPOFILE=%t.db repo-fragments %t Var2 -repo=%t.db -digest-only > %t.d
; RUN: env REPOFILE=%t.db repo-fragments %t1 Var2 -repo=%t.db -digest-only > %t1.d
; RUN: not diff %t.d %t1.d

declare i32 @F();

@Var1 = global i32 ()* @Func1, align 8
@Var2 = global i32 ()* @F, align 8

define i32 @Func1() {
entry:
  ret i32 5
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
