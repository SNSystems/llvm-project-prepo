target triple = "x86_64-pc-linux-gnu-elf"

@str = private constant [6 x i8] c"Ipsum\00", align 1
define void @g() {
entry:
  %puts = tail call i32 @puts(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i64 0, i64 0))
  ret void
}

declare i32 @puts(i8* nocapture readonly)
