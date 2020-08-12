target triple = "x86_64-pc-linux-gnu-elf"

%"T1" = type { i8* }
%"T2" = type { i32 (...)**, i32, i8* }

@Var1 = constant { [1 x i8*] } { [1 x i8*] [i8* bitcast (i8* (%T1*, i8*)* @Func1 to i8*)] }, align 8
@Var2 = global %"T2" zeroinitializer, align 8

define i8* @Func1(%T1* %this, i8* %R) {
entry:
  %R.addr = alloca i8*, align 8
  %R2 = getelementptr inbounds %T1, %T1* %this, i32 0, i32 0
  store i8* %R, i8** %R2, align 8
  ret i8* %R
}

; Function Attrs: nounwind uwtable
define void @Main() {
entry:
  call void @Func2(%"T2"* @Var2)
  ret void
}

; Function Attrs: nounwind uwtable
define void @Func2(%"T2"* %this) {
entry:
  %0 = bitcast %"T2"* %this to i32 (...)***
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [1 x i8*] }, { [1 x i8*] }* @Var1, i32 0, inrange i32 0, i32 2) to i32 (...)**), i32 (...)*** %0, align 8
  ret void
}
