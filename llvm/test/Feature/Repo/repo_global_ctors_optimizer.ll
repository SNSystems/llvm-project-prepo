; This testcase tests the llvm.global_ctors optimization.
;
; This test checks that -O3 is able to delete constructors that become empty
; only after some optimization passes have run, even if repo pruning happens.
; CHECK-NOT: @_GLOBAL__sub_I_test.cpp


; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O3 -S %s -o %t.ll
; RUN: env REPOFILE=%t.db llc -mtriple="x86_64-pc-linux-gnu-repo" -filetype=obj %t.ll -o %t.o
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O3 -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

%"MapEntry" = type { i8* }

@MapSet = internal global [1 x %"MapEntry"] zeroinitializer, align 8
@.str = private unnamed_addr constant [11 x i8] c"ADD16ri_DB\00", align 1
@llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] [{ i32, void ()*, i8* } { i32 65535, void ()* @_GLOBAL__sub_I_test.cpp, i8* null }]

define internal void @_GLOBAL__sub_I_test.cpp() #0 {
entry:
  call void @__cxx_global_var_init()
  ret void
}

define internal void @__cxx_global_var_init() #0 {
entry:
  call void @MapEntryC2(%"MapEntry"* getelementptr inbounds ([1 x %"MapEntry"], [1 x %"MapEntry"]* @MapSet, i64 0, i64 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str, i32 0, i32 0))
  ret void
}

define internal void @MapEntryC2(%"MapEntry"* %this, i8* %RegInstStr) unnamed_addr #1 align 2 {
entry:
  %RegInstStr.addr = alloca i8*, align 8
  %RegInstStr2 = getelementptr inbounds %"MapEntry", %"MapEntry"* %this, i32 0, i32 0
  store i8* %RegInstStr, i8** %RegInstStr2, align 8
  ret void
}

; Function Attrs: noinline norecurse nounwind optnone uwtable
define dso_local i32 @main() #1 {
entry:
  %0 = alloca [1 x %"MapEntry"]*, align 8
  store [1 x %"MapEntry"]* @MapSet, [1 x %"MapEntry"]** %0, align 8
  ret i32 0
}

attributes #0 = { noinline }
attributes #1 = { noinline  optnone}

; CHECK-NOT: @_GLOBAL__sub_I_test.cpp
