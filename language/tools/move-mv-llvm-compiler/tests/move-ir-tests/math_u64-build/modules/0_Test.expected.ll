; ModuleID = '0x100__Test'
source_filename = "<unknown>"

define i64 @Test__test(i64 %0, i64 %1) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 4
  store i64 %1, ptr %local_1, align 4
  %load_store_tmp = load i64, ptr %local_0, align 4
  store i64 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i64, ptr %local_1, align 4
  store i64 %load_store_tmp1, ptr %local_3, align 4
  %add_src_0 = load i64, ptr %local_2, align 4
  %add_src_1 = load i64, ptr %local_3, align 4
  %add_dst = add i64 %add_src_0, %add_src_1
  store i64 %add_dst, ptr %local_4, align 4
  %retval = load i64, ptr %local_4, align 4
  ret i64 %retval
}

define i64 @Test__test_div(i64 %0, i64 %1) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 4
  store i64 %1, ptr %local_1, align 4
  %load_store_tmp = load i64, ptr %local_0, align 4
  store i64 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i64, ptr %local_1, align 4
  store i64 %load_store_tmp1, ptr %local_3, align 4
  %div_src_0 = load i64, ptr %local_2, align 4
  %div_src_1 = load i64, ptr %local_3, align 4
  %div_dst = sdiv i64 %div_src_0, %div_src_1
  store i64 %div_dst, ptr %local_4, align 4
  %retval = load i64, ptr %local_4, align 4
  ret i64 %retval
}

define i64 @Test__test_mul(i64 %0, i64 %1) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 4
  store i64 %1, ptr %local_1, align 4
  %load_store_tmp = load i64, ptr %local_0, align 4
  store i64 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i64, ptr %local_1, align 4
  store i64 %load_store_tmp1, ptr %local_3, align 4
  %mul_src_0 = load i64, ptr %local_2, align 4
  %mul_src_1 = load i64, ptr %local_3, align 4
  %mul_dst = mul i64 %mul_src_0, %mul_src_1
  store i64 %mul_dst, ptr %local_4, align 4
  %retval = load i64, ptr %local_4, align 4
  ret i64 %retval
}

define i64 @Test__test_sub(i64 %0, i64 %1) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 4
  store i64 %1, ptr %local_1, align 4
  %load_store_tmp = load i64, ptr %local_0, align 4
  store i64 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i64, ptr %local_1, align 4
  store i64 %load_store_tmp1, ptr %local_3, align 4
  %sub_src_0 = load i64, ptr %local_2, align 4
  %sub_src_1 = load i64, ptr %local_3, align 4
  %sub_dst = sub i64 %sub_src_0, %sub_src_1
  store i64 %sub_dst, ptr %local_4, align 4
  %retval = load i64, ptr %local_4, align 4
  ret i64 %retval
}
