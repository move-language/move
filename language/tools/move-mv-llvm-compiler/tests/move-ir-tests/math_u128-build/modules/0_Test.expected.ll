; ModuleID = '0x100__Test'
source_filename = "<unknown>"

define i128 @Test__test(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 4
  store i128 %1, ptr %local_1, align 4
  %load_store_tmp = load i128, ptr %local_0, align 4
  store i128 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i128, ptr %local_1, align 4
  store i128 %load_store_tmp1, ptr %local_3, align 4
  %add_src_0 = load i128, ptr %local_2, align 4
  %add_src_1 = load i128, ptr %local_3, align 4
  %add_dst = add i128 %add_src_0, %add_src_1
  store i128 %add_dst, ptr %local_4, align 4
  %retval = load i128, ptr %local_4, align 4
  ret i128 %retval
}

define i128 @Test__test_div(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 4
  store i128 %1, ptr %local_1, align 4
  %load_store_tmp = load i128, ptr %local_0, align 4
  store i128 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i128, ptr %local_1, align 4
  store i128 %load_store_tmp1, ptr %local_3, align 4
  %div_src_0 = load i128, ptr %local_2, align 4
  %div_src_1 = load i128, ptr %local_3, align 4
  %div_dst = udiv i128 %div_src_0, %div_src_1
  store i128 %div_dst, ptr %local_4, align 4
  %retval = load i128, ptr %local_4, align 4
  ret i128 %retval
}

define i128 @Test__test_mod(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 4
  store i128 %1, ptr %local_1, align 4
  %load_store_tmp = load i128, ptr %local_0, align 4
  store i128 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i128, ptr %local_1, align 4
  store i128 %load_store_tmp1, ptr %local_3, align 4
  %mod_src_0 = load i128, ptr %local_2, align 4
  %mod_src_1 = load i128, ptr %local_3, align 4
  %mod_dst = urem i128 %mod_src_0, %mod_src_1
  store i128 %mod_dst, ptr %local_4, align 4
  %retval = load i128, ptr %local_4, align 4
  ret i128 %retval
}

define i128 @Test__test_mul(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 4
  store i128 %1, ptr %local_1, align 4
  %load_store_tmp = load i128, ptr %local_0, align 4
  store i128 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i128, ptr %local_1, align 4
  store i128 %load_store_tmp1, ptr %local_3, align 4
  %mul_src_0 = load i128, ptr %local_2, align 4
  %mul_src_1 = load i128, ptr %local_3, align 4
  %mul_dst = mul i128 %mul_src_0, %mul_src_1
  store i128 %mul_dst, ptr %local_4, align 4
  %retval = load i128, ptr %local_4, align 4
  ret i128 %retval
}

define i128 @Test__test_sub(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 4
  store i128 %1, ptr %local_1, align 4
  %load_store_tmp = load i128, ptr %local_0, align 4
  store i128 %load_store_tmp, ptr %local_2, align 4
  %load_store_tmp1 = load i128, ptr %local_1, align 4
  store i128 %load_store_tmp1, ptr %local_3, align 4
  %sub_src_0 = load i128, ptr %local_2, align 4
  %sub_src_1 = load i128, ptr %local_3, align 4
  %sub_dst = sub i128 %sub_src_0, %sub_src_1
  store i128 %sub_dst, ptr %local_4, align 4
  %retval = load i128, ptr %local_4, align 4
  ret i128 %retval
}
