target triple = "x86_64-pc-linux-gnu-elf"

define dso_local i32 @g() {
entry:
  %call = call i32 @callee()
  ret i32 %call
}

define internal i32 @callee() noinline {
entry:
  ret i32 1
}
