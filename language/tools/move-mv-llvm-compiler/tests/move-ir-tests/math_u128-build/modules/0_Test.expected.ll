; ModuleID = '0x100__Test'
source_filename = "<unknown>"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"
target triple = "sbf-solana-solana"

declare i32 @memcmp(ptr, ptr, i64)

define private i128 @Test__test(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 8
  store i128 %1, ptr %local_1, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_2, align 8
  %load_store_tmp1 = load i128, ptr %local_1, align 8
  store i128 %load_store_tmp1, ptr %local_3, align 8
  %add_src_0 = load i128, ptr %local_2, align 8
  %add_src_1 = load i128, ptr %local_3, align 8
  %add_dst = add i128 %add_src_0, %add_src_1
  %ovfcond = icmp ult i128 %add_dst, %add_src_0
  br i1 %ovfcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  store i128 %add_dst, ptr %local_4, align 8
  %retval = load i128, ptr %local_4, align 8
  ret i128 %retval
}

define private i128 @Test__test_div(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 8
  store i128 %1, ptr %local_1, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_2, align 8
  %load_store_tmp1 = load i128, ptr %local_1, align 8
  store i128 %load_store_tmp1, ptr %local_3, align 8
  %div_src_0 = load i128, ptr %local_2, align 8
  %div_src_1 = load i128, ptr %local_3, align 8
  %zerocond = icmp eq i128 %div_src_1, 0
  br i1 %zerocond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %div_dst = udiv i128 %div_src_0, %div_src_1
  store i128 %div_dst, ptr %local_4, align 8
  %retval = load i128, ptr %local_4, align 8
  ret i128 %retval
}

define private i128 @Test__test_mod(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 8
  store i128 %1, ptr %local_1, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_2, align 8
  %load_store_tmp1 = load i128, ptr %local_1, align 8
  store i128 %load_store_tmp1, ptr %local_3, align 8
  %mod_src_0 = load i128, ptr %local_2, align 8
  %mod_src_1 = load i128, ptr %local_3, align 8
  %zerocond = icmp eq i128 %mod_src_1, 0
  br i1 %zerocond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %mod_dst = urem i128 %mod_src_0, %mod_src_1
  store i128 %mod_dst, ptr %local_4, align 8
  %retval = load i128, ptr %local_4, align 8
  ret i128 %retval
}

define private i128 @Test__test_mul(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 8
  store i128 %1, ptr %local_1, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_2, align 8
  %load_store_tmp1 = load i128, ptr %local_1, align 8
  store i128 %load_store_tmp1, ptr %local_3, align 8
  %mul_src_0 = load i128, ptr %local_2, align 8
  %mul_src_1 = load i128, ptr %local_3, align 8
  %mul_val = call { i128, i1 } @llvm.umul.with.overflow.i128(i128 %mul_src_0, i128 %mul_src_1)
  %mul_dst = extractvalue { i128, i1 } %mul_val, 0
  %mul_ovf = extractvalue { i128, i1 } %mul_val, 1
  br i1 %mul_ovf, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  store i128 %mul_dst, ptr %local_4, align 8
  %retval = load i128, ptr %local_4, align 8
  ret i128 %retval
}

define private i128 @Test__test_sub(i128 %0, i128 %1) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  %local_3 = alloca i128, align 8
  %local_4 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 8
  store i128 %1, ptr %local_1, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_2, align 8
  %load_store_tmp1 = load i128, ptr %local_1, align 8
  store i128 %load_store_tmp1, ptr %local_3, align 8
  %sub_src_0 = load i128, ptr %local_2, align 8
  %sub_src_1 = load i128, ptr %local_3, align 8
  %sub_dst = sub i128 %sub_src_0, %sub_src_1
  %ovfcond = icmp ugt i128 %sub_dst, %sub_src_0
  br i1 %ovfcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  store i128 %sub_dst, ptr %local_4, align 8
  %retval = load i128, ptr %local_4, align 8
  ret i128 %retval
}

; Function Attrs: cold noreturn
declare void @move_rt_abort(i64) #0

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare { i128, i1 } @llvm.umul.with.overflow.i128(i128, i128) #1

attributes #0 = { cold noreturn }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
