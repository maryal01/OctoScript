; ModuleID = 'OctoScript'
source_filename = "OctoScript"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.1 = private unnamed_addr constant [13 x i8] c"hello world\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32 @debug()

define void @main() {
entry:
  %print = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), [12 x i8] c"hello world\00")
  ret void
}
