; ModuleID = 'kazm'
source_filename = "kazm"

declare void @print(i8*)

declare void @println(i8*)

declare void @int_print(i32)

declare void @int_println(i32)

declare void @double_print(double)

declare void @double_println(double)

declare i32 @next_int()

define i32 @main() {
entry:
  %b = alloca i1
  store i1 true, i1* %b
  %0 = call i32 @fun()
  %i = alloca i32
  store i32 %0, i32* %i
  ret i32 0
}

define i32 @fun() {
entry:
  ret i32 1
}

