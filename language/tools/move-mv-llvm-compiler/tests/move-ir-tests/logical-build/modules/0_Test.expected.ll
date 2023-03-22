; ModuleID = '0x100__Test'
source_filename = "<unknown>"

define i1 @Test__test_ge(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i1, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %ge_src_0 = load i8, ptr %local_2, align 1
  %ge_src_1 = load i8, ptr %local_3, align 1
  %ge_dst = icmp uge i8 %ge_src_0, %ge_src_1
  store i1 %ge_dst, ptr %local_4, align 1
  %retval = load i1, ptr %local_4, align 1
  ret i1 %retval
}

define i1 @Test__test_gt(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i1, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %gt_src_0 = load i8, ptr %local_2, align 1
  %gt_src_1 = load i8, ptr %local_3, align 1
  %gt_dst = icmp ugt i8 %gt_src_0, %gt_src_1
  store i1 %gt_dst, ptr %local_4, align 1
  %retval = load i1, ptr %local_4, align 1
  ret i1 %retval
}

define i1 @Test__test_le(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i1, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %le_src_0 = load i8, ptr %local_2, align 1
  %le_src_1 = load i8, ptr %local_3, align 1
  %le_dst = icmp ule i8 %le_src_0, %le_src_1
  store i1 %le_dst, ptr %local_4, align 1
  %retval = load i1, ptr %local_4, align 1
  ret i1 %retval
}

define i1 @Test__test_lt(i8 %0, i8 %1) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  %local_3 = alloca i8, align 1
  %local_4 = alloca i1, align 1
  store i8 %0, ptr %local_0, align 1
  store i8 %1, ptr %local_1, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_2, align 1
  %load_store_tmp1 = load i8, ptr %local_1, align 1
  store i8 %load_store_tmp1, ptr %local_3, align 1
  %lt_src_0 = load i8, ptr %local_2, align 1
  %lt_src_1 = load i8, ptr %local_3, align 1
  %lt_dst = icmp ult i8 %lt_src_0, %lt_src_1
  store i1 %lt_dst, ptr %local_4, align 1
  %retval = load i1, ptr %local_4, align 1
  ret i1 %retval
}
