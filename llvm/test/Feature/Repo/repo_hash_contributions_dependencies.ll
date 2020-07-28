; This test is used to check the hash calculation for the contributions.
;
; Although 'contributions' and 'dependencies' respective arrows point are
; in different direction, they can affect each other and form loops.
;
; The test invokes five steps:
;  1) Compile the initial code `Inputs/repo_contributions_dependencies_loop.ll`;
;  2) Compiled `repo_hash_contributions_dependencies.ll`.
;  3) Dump the digest of `Var2` in %t.
;  4) Dump the digest of `Var2` in %t1.
;  5) Check the digests of `Var2` are different since the function `Func1` is changed.
;
; RUN: rm -rf %t.db
; RUN: env REPOFILE=%t.db clang -c -O3 --target=x86_64-pc-linux-gnu-repo -x ir %S/Inputs/repo_contributions_dependencies_loop.ll -o %t
; RUN: env REPOFILE=%t.db clang -c -O3 --target=x86_64-pc-linux-gnu-repo -x ir %s -o %t1
; RUN: env REPOFILE=%t.db %python %S/fragments.py %t Var2 -r %t.db -d > %t.d
; RUN: env REPOFILE=%t.db %python %S/fragments.py %t1 Var2 -r %t.db -d > %t1.d
; RUN: not diff %t.d %t1.d

target triple = "x86_64-pc-linux-gnu-elf"

%"T1" = type { i8* }
%"T2" = type { i32 (...)**, i32, i8* }

@Var1 = internal constant { [1 x i8*] } { [1 x i8*] [i8* bitcast (i8* (%T1*, i8*)* @Func1 to i8*)] }, align 8

@Var2 = global %"T2" zeroinitializer, align 8

@llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] [{ i32, void ()*, i8* } { i32 65535, void ()* @_GLOBAL__sub_I_test.cpp, i8* null }]

define internal i8* @Func1(%T1* %this, i8* %R) {
entry:
  %R.addr = alloca i8*, align 16
  %R2 = getelementptr inbounds %T1, %T1* %this, i32 0, i32 0
  store i8* %R, i8** %R2, align 16
  ret i8* %R
}

; Function Attrs: nounwind uwtable
define internal void @__cxx_global_var_init() {
entry:
  call void @Func2(%"T2"* @Var2)
  ret void
}

; Function Attrs: nounwind uwtable
define internal void @Func2(%"T2"* %this) {
entry:
  %0 = bitcast %"T2"* %this to i32 (...)***
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [1 x i8*] }, { [1 x i8*] }* @Var1, i32 0, inrange i32 0, i32 2) to i32 (...)**), i32 (...)*** %0, align 8
  ret void
}

; Function Attrs: nounwind uwtable
define internal void @_GLOBAL__sub_I_test.cpp() {
entry:
  call void @__cxx_global_var_init()
  ret void
}
