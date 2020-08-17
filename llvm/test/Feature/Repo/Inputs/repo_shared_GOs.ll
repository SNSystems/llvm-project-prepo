target triple = "x86_64-pc-linux-gnu-repo"

define i32 @Fn() {
entry:
  ret i32 1
}

define available_externally i32 @foo() {
  ret i32 1
}

define void @call_foo() {
  %call = call i32 @foo()
  ret void
}

@bar = alias i32(), i32()* @Fn
define i32 @test() {
entry:
  %0 = call i32() @bar()
  ret i32 %0
}

; use by repo_store_GV.ll.
@X = global i32 0

define void @foov() {
entry:
  store i32 1, i32* @X
  ret void
}

; use by repo_store_getelementptr.ll.
@xs = global [2 x i32] zeroinitializer, align 4

define void @test1() {
entry:
  store i32 1, i32* getelementptr inbounds ([2 x i32], [2 x i32]* @xs, i64 0, i64 0)
  ret void
}

; use by repo_store_load.ll.
@a = global i32 0, align 4
@b = global i32* @a, align 8

; Function Attrs: noinline nounwind optnone uwtable
define void @test2() {
entry:
  %0 = load i32*, i32** @b, align 8
  store i32 2, i32* %0, align 4
  ret void
}

; use by repo_store_bitcast.ll.
%TS = type { i64* }
%TA = type { i64 }

@s = local_unnamed_addr global %TS zeroinitializer, align 8
@gA = local_unnamed_addr global %TA* inttoptr (i64 69905 to %TA*), align 8

define void @test3() {
  %1 = load i64, i64* bitcast (%TA** @gA to i64*), align 8
  store i64 %1, i64* bitcast (%TS* @s to i64*), align 8
  ret void
}

; use by repo_call_GV.ll.
@Z = global i32 1
define void @test4() {
	call void @setto( i32* @Z, i32 2 )
	ret void
}

define void @setto(i32* %P, i32 %V) {
	store i32 %V, i32* %P
	ret void
}
