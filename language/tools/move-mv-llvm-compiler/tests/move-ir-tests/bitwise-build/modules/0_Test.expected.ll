; ModuleID = '0x100__Test'
source_filename = "<unknown>"

define i8 @Test__test_and(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %and_src_0 = load i8, ptr %local_2, align 1
  %and_src_1 = load i8, ptr %local_3, align 1
  %and_dst = and i8 %and_src_0, %and_src_1
  store i8 %and_dst, ptr %local_4, align 1
  %retval = load i8, ptr %local_4, align 1
  ret i8 %retval
}

define i8 @Test__test_or(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %or_src_0 = load i8, ptr %local_2, align 1
  %or_src_1 = load i8, ptr %local_3, align 1
  %or_dst = or i8 %or_src_0, %or_src_1
  store i8 %or_dst, ptr %local_4, align 1
  %retval = load i8, ptr %local_4, align 1
  ret i8 %retval
}

define i8 @Test__test_shl(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %shl_src_0 = load i8, ptr %local_2, align 1
  %shl_src_1 = load i8, ptr %local_3, align 1
  %shl_dst = shl i8 %shl_src_0, %shl_src_1
  store i8 %shl_dst, ptr %local_4, align 1
  %retval = load i8, ptr %local_4, align 1
  ret i8 %retval
}

define i8 @Test__test_shr(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %shr_src_0 = load i8, ptr %local_2, align 1
  %shr_src_1 = load i8, ptr %local_3, align 1
  %shr_dst = ashr i8 %shr_src_0, %shr_src_1
  store i8 %shr_dst, ptr %local_4, align 1
  %retval = load i8, ptr %local_4, align 1
  ret i8 %retval
}

define i8 @Test__test_xor(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i8, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %xor_src_0 = load i8, ptr %local_2, align 1
  %xor_src_1 = load i8, ptr %local_3, align 1
  %xor_dst = xor i8 %xor_src_0, %xor_src_1
  store i8 %xor_dst, ptr %local_4, align 1
  %retval = load i8, ptr %local_4, align 1
  ret i8 %retval
}
