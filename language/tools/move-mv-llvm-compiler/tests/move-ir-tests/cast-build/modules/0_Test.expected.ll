; ModuleID = '0x100__Test'
source_filename = "<unknown>"

define i32 @Test__cast_u32(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i32, align 4
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  store i8 %cast_src, ptr %local_2, align 1
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define i8 @Test__cast_u8(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i8, align 1
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  store i32 %cast_src, ptr %local_2, align 4
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}
