; ModuleID = 'kazm'
source_filename = "kazm"

@globalstring = private unnamed_addr constant [13 x i8] c"Before while\00"
@globalstring.1 = private unnamed_addr constant [15 x i8] c"next_int() < 3\00"
@globalstring.2 = private unnamed_addr constant [12 x i8] c"After while\00"
@globalstring.3 = private unnamed_addr constant [8 x i8] c"In test\00"
@globalstring.4 = private unnamed_addr constant [12 x i8] c"Start of fn\00"
@globalstring.5 = private unnamed_addr constant [8 x i8] c"Took if\00"
@globalstring.6 = private unnamed_addr constant [19 x i8] c"Took second branch\00"
@globalstring.7 = private unnamed_addr constant [9 x i8] c"After if\00"
@globalstring.8 = private unnamed_addr constant [11 x i8] c"thing() = \00"
@globalstring.9 = private unnamed_addr constant [18 x i8] c"without newline: \00"
@globalstring.10 = private unnamed_addr constant [14 x i8] c"! hello world\00"
@globalstring.11 = private unnamed_addr constant [36 x i8] c"Integer arithmetic: 6 should equal \00"
@globalstring.12 = private unnamed_addr constant [17 x i8] c"Double 3.14 is: \00"
@globalstring.13 = private unnamed_addr constant [12 x i8] c"Next ints: \00"
@globalstring.14 = private unnamed_addr constant [16 x i8] c"was less than 6\00"
@globalstring.15 = private unnamed_addr constant [20 x i8] c"was not less than 6\00"
@globalstring.16 = private unnamed_addr constant [16 x i8] c"was less than 6\00"
@globalstring.17 = private unnamed_addr constant [20 x i8] c"was not less than 6\00"

declare void @print(i8*)

declare void @println(i8*)

declare void @int_print(i32)

declare void @int_println(i32)

declare void @double_print(double)

declare void @double_println(double)

declare i32 @next_int()

define void @test_loops() {
entry:
  call void @println(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @globalstring, i32 0, i32 0))
  br label %start

start:                                            ; preds = %entry, %loop
  %0 = call i32 @next_int()
  %im = icmp slt i32 %0, 3
  br i1 %im, label %loop, label %end

loop:                                             ; preds = %start
  call void @println(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @globalstring.1, i32 0, i32 0))
  br label %start

end:                                              ; preds = %start
  call void @println(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @globalstring.2, i32 0, i32 0))
  ret void
}

define void @test() {
entry:
  call void @println(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @globalstring.3, i32 0, i32 0))
  call void @int_println(i32 4)
  ret void
}

define i32 @thing() {
entry:
  ret i32 5
}

define void @main() {
entry:
  call void @test_loops()
  call void @println(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @globalstring.4, i32 0, i32 0))
  br i1 true, label %take, label %dont_take

join:                                             ; preds = %dont_take, %take
  call void @println(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @globalstring.7, i32 0, i32 0))
  call void @print(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @globalstring.8, i32 0, i32 0))
  %0 = call i32 @thing()
  call void @int_println(i32 %0)
  call void @print(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @globalstring.9, i32 0, i32 0))
  call void @println(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @globalstring.10, i32 0, i32 0))
  call void @int_println(i32 27)
  call void @print(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @globalstring.11, i32 0, i32 0))
  call void @int_println(i32 6)
  call void @int_println(i32 8)
  %a = alloca i32
  store i32 20, i32* %a
  %b = alloca double
  store double 3.000000e+00, double* %b
  call void @print(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @globalstring.12, i32 0, i32 0))
  call void @double_println(double 3.140000e+00)
  call void @test()
  call void @println(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @globalstring.13, i32 0, i32 0))
  %1 = call i32 @next_int()
  call void @int_println(i32 %1)
  %2 = call i32 @next_int()
  %im = icmp slt i32 %2, 6
  br i1 %im, label %take2, label %dont_take3

take:                                             ; preds = %entry
  call void @println(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @globalstring.5, i32 0, i32 0))
  br label %join

dont_take:                                        ; preds = %entry
  call void @println(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @globalstring.6, i32 0, i32 0))
  br label %join

join1:                                            ; preds = %dont_take3, %take2
  %3 = call i32 @next_int()
  call void @int_println(i32 %3)
  %4 = call i32 @next_int()
  %im4 = icmp slt i32 %4, 6
  br i1 %im4, label %take6, label %dont_take7

take2:                                            ; preds = %join
  call void @println(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @globalstring.14, i32 0, i32 0))
  br label %join1

dont_take3:                                       ; preds = %join
  call void @println(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @globalstring.15, i32 0, i32 0))
  br label %join1

join5:                                            ; preds = %dont_take7, %take6
  %5 = call i32 @next_int()
  call void @int_println(i32 %5)
  ret void

take6:                                            ; preds = %join1
  call void @println(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @globalstring.16, i32 0, i32 0))
  br label %join5

dont_take7:                                       ; preds = %join1
  call void @println(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @globalstring.17, i32 0, i32 0))
  br label %join5
}

