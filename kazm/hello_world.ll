; ModuleID = 'kazm'
source_filename = "kazm"

@arg = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

declare void @print(i8*)

declare void @println(i8*)

define void @main() {
entry:
  call void @println(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @arg, i32 0, i32 0))
  ret void
}

