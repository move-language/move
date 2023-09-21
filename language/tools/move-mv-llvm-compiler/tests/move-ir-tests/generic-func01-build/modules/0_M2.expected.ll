; ModuleID = '0x100__M2'
source_filename = "<unknown>"
target datalayout = "e-m:e-p:64:64-i64:64-n32:64-S128"
target triple = "sbf-solana-solana"

%struct.M2__Coin_M2__Bitcoin_ = type { i64 }
%struct.M2__Coin_M2__Sol_ = type { i64 }

declare i32 @memcmp(ptr, ptr, i64)

define private %struct.M2__Coin_M2__Bitcoin_ @M2__call_mint_generic() {
entry:
  %local_0 = alloca i64, align 8
  %local_1 = alloca %struct.M2__Coin_M2__Bitcoin_, align 8
  store i64 4, ptr %local_0, align 8
  %call_arg_0 = load i64, ptr %local_0, align 8
  %retval = call %struct.M2__Coin_M2__Bitcoin_ @M2__mint_generic_M2__Bitcoin(i64 %call_arg_0)
  store %struct.M2__Coin_M2__Bitcoin_ %retval, ptr %local_1, align 8
  %retval1 = load %struct.M2__Coin_M2__Bitcoin_, ptr %local_1, align 8
  ret %struct.M2__Coin_M2__Bitcoin_ %retval1
}

define private %struct.M2__Coin_M2__Bitcoin_ @M2__mint_generic_M2__Bitcoin(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1__value = alloca i64, align 8
  %local_2 = alloca %struct.M2__Coin_M2__Bitcoin_, align 8
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1__value, align 8
  %fv.0 = load i64, ptr %local_1__value, align 8
  %insert_0 = insertvalue %struct.M2__Coin_M2__Bitcoin_ undef, i64 %fv.0, 0
  store %struct.M2__Coin_M2__Bitcoin_ %insert_0, ptr %local_2, align 8
  %retval = load %struct.M2__Coin_M2__Bitcoin_, ptr %local_2, align 8
  ret %struct.M2__Coin_M2__Bitcoin_ %retval
}

define %struct.M2__Coin_M2__Sol_ @M2__mint_concrete(i64 %0) {
entry:
  %local_0 = alloca i64, align 8
  %local_1__value = alloca i64, align 8
  %local_2 = alloca %struct.M2__Coin_M2__Sol_, align 8
  store i64 %0, ptr %local_0, align 8
  %load_store_tmp = load i64, ptr %local_0, align 8
  store i64 %load_store_tmp, ptr %local_1__value, align 8
  %fv.0 = load i64, ptr %local_1__value, align 8
  %insert_0 = insertvalue %struct.M2__Coin_M2__Sol_ undef, i64 %fv.0, 0
  store %struct.M2__Coin_M2__Sol_ %insert_0, ptr %local_2, align 8
  %retval = load %struct.M2__Coin_M2__Sol_, ptr %local_2, align 8
  ret %struct.M2__Coin_M2__Sol_ %retval
}
