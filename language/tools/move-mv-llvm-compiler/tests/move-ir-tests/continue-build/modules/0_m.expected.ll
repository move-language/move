; ModuleID = '0x42__m'
source_filename = "<unknown>"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"
target triple = "sbf-solana-solana"

declare i32 @memcmp(ptr, ptr, i64)

define void @"0000000000000042_m_a_57P3fP7Rqn6AaT"() {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  %local_3 = alloca i64, align 8
  %local_4 = alloca i64, align 8
  %local_5 = alloca i64, align 8
  %local_6 = alloca i1, align 1
  %local_7 = alloca i64, align 8
  %local_8 = alloca i64, align 8
  %local_9 = alloca i64, align 8
  %local_10 = alloca i64, align 8
  %local_11 = alloca i1, align 1
  %local_12 = alloca i64, align 8
  %local_13 = alloca i64, align 8
  %local_14 = alloca i64, align 8
  %local_15 = alloca i64, align 8
  %local_16 = alloca i64, align 8
  %local_17 = alloca i64, align 8
  %local_18 = alloca i64, align 8
  %local_19 = alloca i64, align 8
  %local_20 = alloca i64, align 8
  %local_21 = alloca ptr, align 8
  store i64 0, ptr %local_2, align 8
  %load_store_tmp = load i64, ptr %local_2, align 8
  store i64 %load_store_tmp, ptr %local_0, align 8
  store i64 0, ptr %local_3, align 8
  %load_store_tmp1 = load i64, ptr %local_3, align 8
  store i64 %load_store_tmp1, ptr %local_1, align 8
  br label %bb_6

bb_6:                                             ; preds = %join_bb24, %join_bb7, %entry
  %load_store_tmp2 = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp2, ptr %local_4, align 8
  store i64 10, ptr %local_5, align 8
  %lt_src_0 = load i64, ptr %local_4, align 8
  %lt_src_1 = load i64, ptr %local_5, align 8
  %lt_dst = icmp ult i64 %lt_src_0, %lt_src_1
  store i1 %lt_dst, ptr %local_6, align 1
  %cnd = load i1, ptr %local_6, align 1
  br i1 %cnd, label %bb_1, label %bb_0

bb_1:                                             ; preds = %bb_6
  br label %bb_2

bb_2:                                             ; preds = %bb_1
  %load_store_tmp3 = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp3, ptr %local_7, align 8
  store i64 2, ptr %local_8, align 8
  %mod_src_0 = load i64, ptr %local_7, align 8
  %mod_src_1 = load i64, ptr %local_8, align 8
  %zerocond = icmp eq i64 %mod_src_1, 0
  br i1 %zerocond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %bb_2
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %bb_2
  %mod_dst = urem i64 %mod_src_0, %mod_src_1
  store i64 %mod_dst, ptr %local_9, align 8
  store i64 0, ptr %local_10, align 8
  %eq_src_0 = load i64, ptr %local_9, align 8
  %eq_src_1 = load i64, ptr %local_10, align 8
  %eq_dst = icmp eq i64 %eq_src_0, %eq_src_1
  store i1 %eq_dst, ptr %local_11, align 1
  %cnd4 = load i1, ptr %local_11, align 1
  br i1 %cnd4, label %bb_4, label %bb_3

bb_4:                                             ; preds = %join_bb
  br label %bb_5

bb_3:                                             ; preds = %join_bb
  %load_store_tmp5 = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp5, ptr %local_12, align 8
  store i64 1, ptr %local_13, align 8
  %add_src_0 = load i64, ptr %local_12, align 8
  %add_src_1 = load i64, ptr %local_13, align 8
  %add_dst = add i64 %add_src_0, %add_src_1
  %ovfcond = icmp ult i64 %add_dst, %add_src_0
  br i1 %ovfcond, label %then_bb6, label %join_bb7

then_bb6:                                         ; preds = %bb_3
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb7:                                         ; preds = %bb_3
  store i64 %add_dst, ptr %local_14, align 8
  %load_store_tmp8 = load i64, ptr %local_14, align 8
  store i64 %load_store_tmp8, ptr %local_0, align 8
  call void @"0000000000000042_m_bar_4HsAdEmqY5xWAy"()
  br label %bb_6

bb_5:                                             ; preds = %bb_4
  %load_store_tmp9 = load i64, ptr %local_1, align 8
  store i64 %load_store_tmp9, ptr %local_15, align 8
  %load_store_tmp10 = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp10, ptr %local_16, align 8
  %add_src_011 = load i64, ptr %local_15, align 8
  %add_src_112 = load i64, ptr %local_16, align 8
  %add_dst13 = add i64 %add_src_011, %add_src_112
  %ovfcond14 = icmp ult i64 %add_dst13, %add_src_011
  br i1 %ovfcond14, label %then_bb15, label %join_bb16

then_bb15:                                        ; preds = %bb_5
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb16:                                        ; preds = %bb_5
  store i64 %add_dst13, ptr %local_17, align 8
  %load_store_tmp17 = load i64, ptr %local_17, align 8
  store i64 %load_store_tmp17, ptr %local_1, align 8
  %load_store_tmp18 = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp18, ptr %local_18, align 8
  store i64 1, ptr %local_19, align 8
  %add_src_019 = load i64, ptr %local_18, align 8
  %add_src_120 = load i64, ptr %local_19, align 8
  %add_dst21 = add i64 %add_src_019, %add_src_120
  %ovfcond22 = icmp ult i64 %add_dst21, %add_src_019
  br i1 %ovfcond22, label %then_bb23, label %join_bb24

then_bb23:                                        ; preds = %join_bb16
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb24:                                        ; preds = %join_bb16
  store i64 %add_dst21, ptr %local_20, align 8
  %load_store_tmp25 = load i64, ptr %local_20, align 8
  store i64 %load_store_tmp25, ptr %local_0, align 8
  br label %bb_6

bb_0:                                             ; preds = %bb_6
  store ptr %local_1, ptr %local_21, align 8
  %call_arg_0 = load ptr, ptr %local_21, align 8
  call void @"0000000000000042_m_foo_372CFZN6eJ9KQL"(ptr %call_arg_0)
  ret void
}

define void @"0000000000000042_m_bar_4HsAdEmqY5xWAy"() {
entry:
  %local_0 = alloca i64, align 8
  store i64 0, ptr %local_0, align 8
  %call_arg_0 = load i64, ptr %local_0, align 8
  call void @move_rt_abort(i64 %call_arg_0)
  unreachable
}

define void @"0000000000000042_m_foo_372CFZN6eJ9KQL"(ptr nonnull readonly %0) {
entry:
  %local_0 = alloca ptr, align 8
  %local_1 = alloca i64, align 8
  store ptr %0, ptr %local_0, align 8
  store i64 0, ptr %local_1, align 8
  %call_arg_0 = load i64, ptr %local_1, align 8
  call void @move_rt_abort(i64 %call_arg_0)
  unreachable
}

; Function Attrs: cold noreturn
declare void @move_rt_abort(i64) #0

attributes #0 = { cold noreturn }
