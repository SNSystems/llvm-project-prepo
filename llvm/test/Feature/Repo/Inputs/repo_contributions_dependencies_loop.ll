target triple = "x86_64-pc-linux-gnu-repo"

declare i32 @F();

@Var1 = global i32 ()* @Func1, align 8
@Var2 = global i32 ()* @F, align 8

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
