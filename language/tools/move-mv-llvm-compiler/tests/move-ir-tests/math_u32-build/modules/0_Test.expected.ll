; ModuleID = '0x100__Test'
source_filename = "<unknown>"

define i32 @Test__test(i32 %0, i32 %1) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  store i32 %0, ptr %local_0, align 4
  store i32 %1, ptr %local_1, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i32, ptr %local_1, align 4
  store i32 %load_store_tmp1, ptr %local_3, align 4
  %add_src_0 = load i32, ptr %local_2, align 4
  %add_src_1 = load i32, ptr %local_3, align 4
  %add_dst = add i32 %add_src_0, %add_src_1
  store i32 %add_dst, ptr %local_4, align 4
  %retval = load i32, ptr %local_4, align 4
  ret i32 %retval
}

define i32 @Test__test_div(i32 %0, i32 %1) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  store i32 %0, ptr %local_0, align 4
  store i32 %1, ptr %local_1, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i32, ptr %local_1, align 4
  store i32 %load_store_tmp1, ptr %local_3, align 4
  %div_src_0 = load i32, ptr %local_2, align 4
  %div_src_1 = load i32, ptr %local_3, align 4
  %div_dst = sdiv i32 %div_src_0, %div_src_1
  store i32 %div_dst, ptr %local_4, align 4
  %retval = load i32, ptr %local_4, align 4
  ret i32 %retval
}

define i32 @Test__test_mul_trunc(i32 %0, i32 %1) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  store i32 %0, ptr %local_0, align 4
  store i32 %1, ptr %local_1, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i32, ptr %local_1, align 4
  store i32 %load_store_tmp1, ptr %local_3, align 4
  %mul_src_0 = load i32, ptr %local_2, align 4
  %mul_src_1 = load i32, ptr %local_3, align 4
  %mul_dst = mul i32 %mul_src_0, %mul_src_1
  store i32 %mul_dst, ptr %local_4, align 4
  %retval = load i32, ptr %local_4, align 4
  ret i32 %retval
}

define i32 @Test__test_sub(i32 %0, i32 %1) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i32, align 4
  %local_3 = alloca i32, align 4
  %local_4 = alloca i32, align 4
  store i32 %0, ptr %local_0, align 4
  store i32 %1, ptr %local_1, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i32, ptr %local_1, align 4
  store i32 %load_store_tmp1, ptr %local_3, align 4
  %sub_src_0 = load i32, ptr %local_2, align 4
  %sub_src_1 = load i32, ptr %local_3, align 4
  %sub_dst = sub i32 %sub_src_0, %sub_src_1
  store i32 %sub_dst, ptr %local_4, align 4
  %retval = load i32, ptr %local_4, align 4
  ret i32 %retval
}
