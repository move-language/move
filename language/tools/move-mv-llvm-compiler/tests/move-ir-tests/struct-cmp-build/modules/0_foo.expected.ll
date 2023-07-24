; ModuleID = '0x100__foo'
source_filename = "<unknown>"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"
target triple = "sbf-solana-solana"

%__move_rt_type = type { { ptr, i64 }, i64, ptr }

@__move_rttydesc_u64 = private unnamed_addr constant %__move_rt_type { { ptr, i64 } { ptr @__move_rttydesc_u64_name, i64 3 }, i64 5, ptr @__move_rttydesc_NOTHING_info }
@__move_rttydesc_u64_name = private unnamed_addr constant [3 x i8] c"u64"
@__move_rttydesc_NOTHING_info = private unnamed_addr constant i8 -1

declare i32 @memcmp(ptr, ptr, i64)

define private i1 @foo__doit({ ptr, i64, i64 } %0, { ptr, i64, i64 } %1) {
entry:
  %local_0 = alloca { ptr, i64, i64 }, align 8
  %local_1 = alloca { ptr, i64, i64 }, align 8
  %local_2 = alloca { ptr, i64, i64 }, align 8
  %local_3 = alloca { ptr, i64, i64 }, align 8
  %local_4 = alloca i1, align 1
  store { ptr, i64, i64 } %0, ptr %local_0, align 8
  store { ptr, i64, i64 } %1, ptr %local_1, align 8
  %load_store_tmp = load { ptr, i64, i64 }, ptr %local_0, align 8
  store { ptr, i64, i64 } %load_store_tmp, ptr %local_2, align 8
  %load_store_tmp1 = load { ptr, i64, i64 }, ptr %local_1, align 8
  store { ptr, i64, i64 } %load_store_tmp1, ptr %local_3, align 8
  %2 = call i1 @move_rt_vec_cmp_eq(ptr @__move_rttydesc_u64, ptr %local_2, ptr %local_3)
  %invert_dst = xor i1 %2, true
  store i1 %invert_dst, ptr %local_4, align 1
  %retval = load i1, ptr %local_4, align 1
  ret i1 %retval
}

declare i1 @move_rt_vec_cmp_eq(ptr nonnull readonly dereferenceable(32), ptr nonnull readonly dereferenceable(24), ptr nonnull readonly dereferenceable(24))
