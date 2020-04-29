; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O3 -S %s | FileCheck -check-prefix=CHECK-NAME-IN-IR %s
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O3 -S %s -o %t
; RUN: env REPOFILE=%t.db llc -mtriple="x86_64-pc-linux-gnu-repo" -filetype=obj %t
; RUN: env REPOFILE=%t.db opt -mtriple="x86_64-pc-linux-gnu-repo" -O3 -S %s |  FileCheck -check-prefix=CHECK-NAME-IN-DATABASE %s

target triple = "x86_64-pc-linux-gnu-elf"

@.input_str = private unnamed_addr constant [7 x i8] c"hello\0A\00", align 1

declare i32 @printf(i8* nocapture readonly, ...)

define void @test() align 2 {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.input_str, i32 0, i32 0))
  ret void
}

;CHECK-NAME-IN-IR: @str = private 

;CHECK-NAME-IN-DATABASE: !RepoDefinition(name: "str.{{.+}}"
