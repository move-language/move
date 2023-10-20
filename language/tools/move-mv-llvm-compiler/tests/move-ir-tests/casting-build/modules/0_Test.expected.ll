; ModuleID = '0x100__Test'
source_filename = "<unknown>"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"
target triple = "sbf-solana-solana"

declare i32 @memcmp(ptr, ptr, i64)

define private i128 @"0000000000000100_Test_cast_u128_as_u1_EosMbgKgb89mtR"(i128 %0) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i128, align 8
  store i128 %0, ptr %local_0, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i128, ptr %local_1, align 8
  store i128 %cast_src, ptr %local_2, align 8
  %retval = load i128, ptr %local_2, align 8
  ret i128 %retval
}

define private i16 @"0000000000000100_Test_cast_u128_as_u1_DKvxE3D2MTsJDM"(i128 %0) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i16, align 2
  store i128 %0, ptr %local_0, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i128, ptr %local_1, align 8
  %castcond = icmp ugt i128 %cast_src, 65535
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i128 %cast_src to i16
  store i16 %trunc_dst, ptr %local_2, align 2
  %retval = load i16, ptr %local_2, align 2
  ret i16 %retval
}

define private i256 @"0000000000000100_Test_cast_u128_as_u2_3USaYBXhLMZNZi"(i128 %0) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i256, align 8
  store i128 %0, ptr %local_0, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i128, ptr %local_1, align 8
  %zext_dst = zext i128 %cast_src to i256
  store i256 %zext_dst, ptr %local_2, align 8
  %retval = load i256, ptr %local_2, align 8
  ret i256 %retval
}

define private i32 @"0000000000000100_Test_cast_u128_as_u3_FZpzfHEvyFzXAF"(i128 %0) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i32, align 4
  store i128 %0, ptr %local_0, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i128, ptr %local_1, align 8
  %castcond = icmp ugt i128 %cast_src, 4294967295
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i128 %cast_src to i32
  store i32 %trunc_dst, ptr %local_2, align 4
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define private i64 @"0000000000000100_Test_cast_u128_as_u6_GxMXTS12zJWHeE"(i128 %0) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i64, align 8
  store i128 %0, ptr %local_0, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i128, ptr %local_1, align 8
  %castcond = icmp ugt i128 %cast_src, 18446744073709551615
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i128 %cast_src to i64
  store i64 %trunc_dst, ptr %local_2, align 8
  %retval = load i64, ptr %local_2, align 8
  ret i64 %retval
}

define private i8 @"0000000000000100_Test_cast_u128_as_u8_3Kanynuh6xkmq4"(i128 %0) {
entry:
  %local_0 = alloca i128, align 8
  %local_1 = alloca i128, align 8
  %local_2 = alloca i8, align 1
  store i128 %0, ptr %local_0, align 8
  %load_store_tmp = load i128, ptr %local_0, align 8
  store i128 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i128, ptr %local_1, align 8
  %castcond = icmp ugt i128 %cast_src, 255
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i128 %cast_src to i8
  store i8 %trunc_dst, ptr %local_2, align 1
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}

define private i128 @"0000000000000100_Test_cast_u16_as_u12_48vvYCk9oyAVii"(i16 %0) {
entry:
  %local_0 = alloca i16, align 2
  %local_1 = alloca i16, align 2
  %local_2 = alloca i128, align 8
  store i16 %0, ptr %local_0, align 2
  %load_store_tmp = load i16, ptr %local_0, align 2
  store i16 %load_store_tmp, ptr %local_1, align 2
  %cast_src = load i16, ptr %local_1, align 2
  %zext_dst = zext i16 %cast_src to i128
  store i128 %zext_dst, ptr %local_2, align 8
  %retval = load i128, ptr %local_2, align 8
  ret i128 %retval
}

define private i16 @"0000000000000100_Test_cast_u16_as_u16_Ggg6LSEKHK9isj"(i16 %0) {
entry:
  %local_0 = alloca i16, align 2
  %local_1 = alloca i16, align 2
  %local_2 = alloca i16, align 2
  store i16 %0, ptr %local_0, align 2
  %load_store_tmp = load i16, ptr %local_0, align 2
  store i16 %load_store_tmp, ptr %local_1, align 2
  %cast_src = load i16, ptr %local_1, align 2
  store i16 %cast_src, ptr %local_2, align 2
  %retval = load i16, ptr %local_2, align 2
  ret i16 %retval
}

define private i256 @"0000000000000100_Test_cast_u16_as_u25_5f1LCGcZBMnPmg"(i16 %0) {
entry:
  %local_0 = alloca i16, align 2
  %local_1 = alloca i16, align 2
  %local_2 = alloca i256, align 8
  store i16 %0, ptr %local_0, align 2
  %load_store_tmp = load i16, ptr %local_0, align 2
  store i16 %load_store_tmp, ptr %local_1, align 2
  %cast_src = load i16, ptr %local_1, align 2
  %zext_dst = zext i16 %cast_src to i256
  store i256 %zext_dst, ptr %local_2, align 8
  %retval = load i256, ptr %local_2, align 8
  ret i256 %retval
}

define private i32 @"0000000000000100_Test_cast_u16_as_u32_HskuQVc3bds5ee"(i16 %0) {
entry:
  %local_0 = alloca i16, align 2
  %local_1 = alloca i16, align 2
  %local_2 = alloca i32, align 4
  store i16 %0, ptr %local_0, align 2
  %load_store_tmp = load i16, ptr %local_0, align 2
  store i16 %load_store_tmp, ptr %local_1, align 2
  %cast_src = load i16, ptr %local_1, align 2
  %zext_dst = zext i16 %cast_src to i32
  store i32 %zext_dst, ptr %local_2, align 4
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define private i64 @"0000000000000100_Test_cast_u16_as_u64_5o1aSw6vxP18zv"(i16 %0) {
entry:
  %local_0 = alloca i16, align 2
  %local_1 = alloca i16, align 2
  %local_2 = alloca i64, align 8
  store i16 %0, ptr %local_0, align 2
  %load_store_tmp = load i16, ptr %local_0, align 2
  store i16 %load_store_tmp, ptr %local_1, align 2
  %cast_src = load i16, ptr %local_1, align 2
  %zext_dst = zext i16 %cast_src to i64
  store i64 %zext_dst, ptr %local_2, align 8
  %retval = load i64, ptr %local_2, align 8
  ret i64 %retval
}

define private i8 @"0000000000000100_Test_cast_u16_as_u8_E6h4ahDP7DeDTS"(i16 %0) {
entry:
  %local_0 = alloca i16, align 2
  %local_1 = alloca i16, align 2
  %local_2 = alloca i8, align 1
  store i16 %0, ptr %local_0, align 2
  %load_store_tmp = load i16, ptr %local_0, align 2
  store i16 %load_store_tmp, ptr %local_1, align 2
  %cast_src = load i16, ptr %local_1, align 2
  %castcond = icmp ugt i16 %cast_src, 255
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i16 %cast_src to i8
  store i8 %trunc_dst, ptr %local_2, align 1
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}

define private i128 @"0000000000000100_Test_cast_u256_as_u1_H1vE9mLtb7F11J"(i256 %0) {
entry:
  %local_0 = alloca i256, align 8
  %local_1 = alloca i256, align 8
  %local_2 = alloca i128, align 8
  store i256 %0, ptr %local_0, align 8
  %load_store_tmp = load i256, ptr %local_0, align 8
  store i256 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i256, ptr %local_1, align 8
  %castcond = icmp ugt i256 %cast_src, 340282366920938463463374607431768211455
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i256 %cast_src to i128
  store i128 %trunc_dst, ptr %local_2, align 8
  %retval = load i128, ptr %local_2, align 8
  ret i128 %retval
}

define private i16 @"0000000000000100_Test_cast_u256_as_u1_2VpzeTuQbUiRFz"(i256 %0) {
entry:
  %local_0 = alloca i256, align 8
  %local_1 = alloca i256, align 8
  %local_2 = alloca i16, align 2
  store i256 %0, ptr %local_0, align 8
  %load_store_tmp = load i256, ptr %local_0, align 8
  store i256 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i256, ptr %local_1, align 8
  %castcond = icmp ugt i256 %cast_src, 65535
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i256 %cast_src to i16
  store i16 %trunc_dst, ptr %local_2, align 2
  %retval = load i16, ptr %local_2, align 2
  ret i16 %retval
}

define private i256 @"0000000000000100_Test_cast_u256_as_u2_J6eBUVWyksaSbN"(i256 %0) {
entry:
  %local_0 = alloca i256, align 8
  %local_1 = alloca i256, align 8
  %local_2 = alloca i256, align 8
  store i256 %0, ptr %local_0, align 8
  %load_store_tmp = load i256, ptr %local_0, align 8
  store i256 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i256, ptr %local_1, align 8
  store i256 %cast_src, ptr %local_2, align 8
  %retval = load i256, ptr %local_2, align 8
  ret i256 %retval
}

define private i32 @"0000000000000100_Test_cast_u256_as_u3_H67HkVhmACKjFV"(i256 %0) {
entry:
  %local_0 = alloca i256, align 8
  %local_1 = alloca i256, align 8
  %local_2 = alloca i32, align 4
  store i256 %0, ptr %local_0, align 8
  %load_store_tmp = load i256, ptr %local_0, align 8
  store i256 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i256, ptr %local_1, align 8
  %castcond = icmp ugt i256 %cast_src, 4294967295
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i256 %cast_src to i32
  store i32 %trunc_dst, ptr %local_2, align 4
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define private i64 @"0000000000000100_Test_cast_u256_as_u6_9ShSGoBZXMB5rk"(i256 %0) {
entry:
  %local_0 = alloca i256, align 8
  %local_1 = alloca i256, align 8
  %local_2 = alloca i64, align 8
  store i256 %0, ptr %local_0, align 8
  %load_store_tmp = load i256, ptr %local_0, align 8
  store i256 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i256, ptr %local_1, align 8
  %castcond = icmp ugt i256 %cast_src, 18446744073709551615
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i256 %cast_src to i64
  store i64 %trunc_dst, ptr %local_2, align 8
  %retval = load i64, ptr %local_2, align 8
  ret i64 %retval
}

define private i8 @"0000000000000100_Test_cast_u256_as_u8_83PatQdaFB94BM"(i256 %0) {
entry:
  %local_0 = alloca i256, align 8
  %local_1 = alloca i256, align 8
  %local_2 = alloca i8, align 1
  store i256 %0, ptr %local_0, align 8
  %load_store_tmp = load i256, ptr %local_0, align 8
  store i256 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i256, ptr %local_1, align 8
  %castcond = icmp ugt i256 %cast_src, 255
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i256 %cast_src to i8
  store i8 %trunc_dst, ptr %local_2, align 1
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}

define private i128 @"0000000000000100_Test_cast_u32_as_u12_12PvDPRobNZEgk"(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i128, align 8
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  %zext_dst = zext i32 %cast_src to i128
  store i128 %zext_dst, ptr %local_2, align 8
  %retval = load i128, ptr %local_2, align 8
  ret i128 %retval
}

define private i16 @"0000000000000100_Test_cast_u32_as_u16_6omAdQfymrkdLg"(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i16, align 2
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  %castcond = icmp ugt i32 %cast_src, 65535
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i32 %cast_src to i16
  store i16 %trunc_dst, ptr %local_2, align 2
  %retval = load i16, ptr %local_2, align 2
  ret i16 %retval
}

define private i256 @"0000000000000100_Test_cast_u32_as_u25_B1Usr4HNUQpNGW"(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i256, align 8
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  %zext_dst = zext i32 %cast_src to i256
  store i256 %zext_dst, ptr %local_2, align 8
  %retval = load i256, ptr %local_2, align 8
  ret i256 %retval
}

define private i32 @"0000000000000100_Test_cast_u32_as_u32_EcgAh6KgPGSVQF"(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i32, align 4
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  store i32 %cast_src, ptr %local_2, align 4
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define private i64 @"0000000000000100_Test_cast_u32_as_u64_5cLe7X4v97JAod"(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i64, align 8
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  %zext_dst = zext i32 %cast_src to i64
  store i64 %zext_dst, ptr %local_2, align 8
  %retval = load i64, ptr %local_2, align 8
  ret i64 %retval
}

define private i8 @"0000000000000100_Test_cast_u32_as_u8_621Fe4uwzZKLc6"(i32 %0) {
entry:
  %local_0 = alloca i32, align 4
  %local_1 = alloca i32, align 4
  %local_2 = alloca i8, align 1
  store i32 %0, ptr %local_0, align 4
  %load_store_tmp = load i32, ptr %local_0, align 4
  store i32 %load_store_tmp, ptr %local_1, align 4
  %cast_src = load i32, ptr %local_1, align 4
  %castcond = icmp ugt i32 %cast_src, 255
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i32 %cast_src to i8
  store i8 %trunc_dst, ptr %local_2, align 1
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}

define private i128 @"0000000000000100_Test_cast_u64_as_u12_Cr9Fp8Rz9e9Lug"(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i128, align 8
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i64, ptr %local_1, align 8
  %zext_dst = zext i64 %cast_src to i128
  store i128 %zext_dst, ptr %local_2, align 8
  %retval = load i128, ptr %local_2, align 8
  ret i128 %retval
}

define private i16 @"0000000000000100_Test_cast_u64_as_u16_2v27U4cWf4GFgB"(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i16, align 2
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i64, ptr %local_1, align 8
  %castcond = icmp ugt i64 %cast_src, 65535
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i64 %cast_src to i16
  store i16 %trunc_dst, ptr %local_2, align 2
  %retval = load i16, ptr %local_2, align 2
  ret i16 %retval
}

define private i256 @"0000000000000100_Test_cast_u64_as_u25_J7dXGFVM1ke1d7"(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i256, align 8
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i64, ptr %local_1, align 8
  %zext_dst = zext i64 %cast_src to i256
  store i256 %zext_dst, ptr %local_2, align 8
  %retval = load i256, ptr %local_2, align 8
  ret i256 %retval
}

define private i32 @"0000000000000100_Test_cast_u64_as_u32_3ufnzwj8S12nP2"(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i32, align 4
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i64, ptr %local_1, align 8
  %castcond = icmp ugt i64 %cast_src, 4294967295
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i64 %cast_src to i32
  store i32 %trunc_dst, ptr %local_2, align 4
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define private i64 @"0000000000000100_Test_cast_u64_as_u64_5J4YBxzZDqdXD1"(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i64, align 8
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i64, ptr %local_1, align 8
  store i64 %cast_src, ptr %local_2, align 8
  %retval = load i64, ptr %local_2, align 8
  ret i64 %retval
}

define private i8 @"0000000000000100_Test_cast_u64_as_u8_ALyQoFsJdn41Vd"(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca i8, align 1
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1, align 8
  %cast_src = load i64, ptr %local_1, align 8
  %castcond = icmp ugt i64 %cast_src, 255
  br i1 %castcond, label %then_bb, label %join_bb

then_bb:                                          ; preds = %entry
  call void @move_rt_abort(i64 4017)
  unreachable

join_bb:                                          ; preds = %entry
  %trunc_dst = trunc i64 %cast_src to i8
  store i8 %trunc_dst, ptr %local_2, align 1
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}

define private i128 @"0000000000000100_Test_cast_u8_as_u128_AJCCPEsVkgqCDf"(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i128, align 8
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  %zext_dst = zext i8 %cast_src to i128
  store i128 %zext_dst, ptr %local_2, align 8
  %retval = load i128, ptr %local_2, align 8
  ret i128 %retval
}

define private i16 @"0000000000000100_Test_cast_u8_as_u16_5LHZStPc9X5SB5"(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i16, align 2
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  %zext_dst = zext i8 %cast_src to i16
  store i16 %zext_dst, ptr %local_2, align 2
  %retval = load i16, ptr %local_2, align 2
  ret i16 %retval
}

define private i256 @"0000000000000100_Test_cast_u8_as_u256_Fm3PeifTodqbt5"(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i256, align 8
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  %zext_dst = zext i8 %cast_src to i256
  store i256 %zext_dst, ptr %local_2, align 8
  %retval = load i256, ptr %local_2, align 8
  ret i256 %retval
}

define private i32 @"0000000000000100_Test_cast_u8_as_u32_3t63QaPFCJ5VRw"(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i32, align 4
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  %zext_dst = zext i8 %cast_src to i32
  store i32 %zext_dst, ptr %local_2, align 4
  %retval = load i32, ptr %local_2, align 4
  ret i32 %retval
}

define private i64 @"0000000000000100_Test_cast_u8_as_u64_GVubcyHxLKmAwn"(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i64, align 8
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  %zext_dst = zext i8 %cast_src to i64
  store i64 %zext_dst, ptr %local_2, align 8
  %retval = load i64, ptr %local_2, align 8
  ret i64 %retval
}

define private i8 @"0000000000000100_Test_cast_u8_as_u8_CHTbdKkdKa7gKZ"(i8 %0) {
entry:
  %local_0 = alloca i8, align 1
  %local_1 = alloca i8, align 1
  %local_2 = alloca i8, align 1
  store i8 %0, ptr %local_0, align 1
  %load_store_tmp = load i8, ptr %local_0, align 1
  store i8 %load_store_tmp, ptr %local_1, align 1
  %cast_src = load i8, ptr %local_1, align 1
  store i8 %cast_src, ptr %local_2, align 1
  %retval = load i8, ptr %local_2, align 1
  ret i8 %retval
}

; Function Attrs: cold noreturn
declare void @move_rt_abort(i64) #0

attributes #0 = { cold noreturn }
