; ModuleID = '<SELF>'
source_filename = "<unknown>"

%__move_rt_type = type { { ptr, i64 }, i32, ptr }

@__move_rttydesc_u64 = constant %__move_rt_type { { ptr, i64 } { ptr @__move_rttydesc_u64_name, i64 3 }, i32 3, ptr @__move_rttydesc_NOTHING_info }
@__move_rttydesc_u64_name = constant [3 x i8] c"u64"
@__move_rttydesc_NOTHING_info = constant i8 -1

define void @main() {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca i64, align 8
  %local_2 = alloca ptr, align 8
  store i64 10, ptr %local_1, align 4
  %load_store_tmp = load i64, ptr %local_1, align 4
  store i64 %load_store_tmp, ptr %local_0, align 4
  store ptr %local_0, ptr %local_2, align 8
  %loaded_alloca = load ptr, ptr %local_2, align 8
  call void @move_native_debug_print(ptr @__move_rttydesc_u64, ptr %loaded_alloca)
  ret void
}

declare void @move_native_debug_print(ptr, ptr)
