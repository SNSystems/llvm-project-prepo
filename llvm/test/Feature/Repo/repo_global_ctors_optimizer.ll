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

%"struct.(anonymous namespace)::ManualMapEntry" = type { i8* }

@_ZN12_GLOBAL__N_112ManualMapSetE = internal global [1 x %"struct.(anonymous namespace)::ManualMapEntry"] zeroinitializer, align 8
@.str = private unnamed_addr constant [11 x i8] c"ADD16ri_DB\00", align 1
@llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] [{ i32, void ()*, i8* } { i32 65535, void ()* @_GLOBAL__sub_I_test.cpp, i8* null }]

; Function Attrs: noinline uwtable
define internal void @__cxx_global_var_init() #0 section ".text.startup" {
entry:
  call void @_ZN12_GLOBAL__N_114ManualMapEntryC2EPKc(%"struct.(anonymous namespace)::ManualMapEntry"* getelementptr inbounds ([1 x %"struct.(anonymous namespace)::ManualMapEntry"], [1 x %"struct.(anonymous namespace)::ManualMapEntry"]* @_ZN12_GLOBAL__N_112ManualMapSetE, i64 0, i64 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str, i32 0, i32 0))
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define internal void @_ZN12_GLOBAL__N_114ManualMapEntryC2EPKc(%"struct.(anonymous namespace)::ManualMapEntry"* %this, i8* %RegInstStr) unnamed_addr #1 align 2 {
entry:
  %this.addr = alloca %"struct.(anonymous namespace)::ManualMapEntry"*, align 8
  %RegInstStr.addr = alloca i8*, align 8
  store %"struct.(anonymous namespace)::ManualMapEntry"* %this, %"struct.(anonymous namespace)::ManualMapEntry"** %this.addr, align 8
  store i8* %RegInstStr, i8** %RegInstStr.addr, align 8
  %this1 = load %"struct.(anonymous namespace)::ManualMapEntry"*, %"struct.(anonymous namespace)::ManualMapEntry"** %this.addr, align 8
  %RegInstStr2 = getelementptr inbounds %"struct.(anonymous namespace)::ManualMapEntry", %"struct.(anonymous namespace)::ManualMapEntry"* %this1, i32 0, i32 0
  %0 = load i8*, i8** %RegInstStr.addr, align 8
  store i8* %0, i8** %RegInstStr2, align 8
  ret void
}

; Function Attrs: noinline norecurse nounwind optnone uwtable
define dso_local i32 @main() #2 {
entry:
  %retval = alloca i32, align 4
  %__range1 = alloca [1 x %"struct.(anonymous namespace)::ManualMapEntry"]*, align 8
  %__begin1 = alloca %"struct.(anonymous namespace)::ManualMapEntry"*, align 8
  %__end1 = alloca %"struct.(anonymous namespace)::ManualMapEntry"*, align 8
  %Entry = alloca %"struct.(anonymous namespace)::ManualMapEntry"*, align 8
  store i32 0, i32* %retval, align 4
  store [1 x %"struct.(anonymous namespace)::ManualMapEntry"]* @_ZN12_GLOBAL__N_112ManualMapSetE, [1 x %"struct.(anonymous namespace)::ManualMapEntry"]** %__range1, align 8
  store %"struct.(anonymous namespace)::ManualMapEntry"* getelementptr inbounds ([1 x %"struct.(anonymous namespace)::ManualMapEntry"], [1 x %"struct.(anonymous namespace)::ManualMapEntry"]* @_ZN12_GLOBAL__N_112ManualMapSetE, i32 0, i32 0), %"struct.(anonymous namespace)::ManualMapEntry"** %__begin1, align 8
  store %"struct.(anonymous namespace)::ManualMapEntry"* getelementptr inbounds (%"struct.(anonymous namespace)::ManualMapEntry", %"struct.(anonymous namespace)::ManualMapEntry"* getelementptr inbounds ([1 x %"struct.(anonymous namespace)::ManualMapEntry"], [1 x %"struct.(anonymous namespace)::ManualMapEntry"]* @_ZN12_GLOBAL__N_112ManualMapSetE, i32 0, i32 0), i64 1), %"struct.(anonymous namespace)::ManualMapEntry"** %__end1, align 8
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load %"struct.(anonymous namespace)::ManualMapEntry"*, %"struct.(anonymous namespace)::ManualMapEntry"** %__begin1, align 8
  %1 = load %"struct.(anonymous namespace)::ManualMapEntry"*, %"struct.(anonymous namespace)::ManualMapEntry"** %__end1, align 8
  %cmp = icmp ne %"struct.(anonymous namespace)::ManualMapEntry"* %0, %1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %2 = load %"struct.(anonymous namespace)::ManualMapEntry"*, %"struct.(anonymous namespace)::ManualMapEntry"** %__begin1, align 8
  store %"struct.(anonymous namespace)::ManualMapEntry"* %2, %"struct.(anonymous namespace)::ManualMapEntry"** %Entry, align 8
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %3 = load %"struct.(anonymous namespace)::ManualMapEntry"*, %"struct.(anonymous namespace)::ManualMapEntry"** %__begin1, align 8
  %incdec.ptr = getelementptr inbounds %"struct.(anonymous namespace)::ManualMapEntry", %"struct.(anonymous namespace)::ManualMapEntry"* %3, i32 1
  store %"struct.(anonymous namespace)::ManualMapEntry"* %incdec.ptr, %"struct.(anonymous namespace)::ManualMapEntry"** %__begin1, align 8
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 0
}

; Function Attrs: noinline uwtable
define internal void @_GLOBAL__sub_I_test.cpp() #0 section ".text.startup" {
entry:
  call void @__cxx_global_var_init()
  ret void
}

attributes #0 = { noinline }
attributes #1 = { noinline  optnone}
attributes #2 = { noinline  optnone}

; CHECK-NOT: @_GLOBAL__sub_I_test.cpp
