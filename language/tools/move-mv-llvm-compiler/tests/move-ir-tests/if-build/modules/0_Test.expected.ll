; ModuleID = '0x100__Test'
source_filename = "<unknown>"
target triple = "bpfel-unknown-unknown"

define i8 @test(i1 %0) {
entry_:
  %local_0_ = alloca i1, align 1
  %local_1_ = alloca i8, align 1
  %local_2_ = alloca i1, align 1
  %local_3_ = alloca i8, align 1
  %local_4_ = alloca i8, align 1
  %local_5_ = alloca i8, align 1
  store i1 %0, ptr %local_0_, align 1
  %load_store_tmp_ = load i1, ptr %local_0_, align 1
  store i1 %load_store_tmp_, ptr %local_2_, align 1
  %cnd_ = load i1, ptr %local_2_, align 1
  br i1 %cnd_, label %bb_1_, label %bb_0_

bb_1_:                                            ; preds = %entry_
  store i8 2, ptr %local_3_, align 1
  %load_store_tmp_1 = load i8, ptr %local_3_, align 1
  store i8 %load_store_tmp_1, ptr %local_1_, align 1
  br label %bb_2_

bb_0_:                                            ; preds = %entry_
  store i8 3, ptr %local_4_, align 1
  %load_store_tmp_2 = load i8, ptr %local_4_, align 1
  store i8 %load_store_tmp_2, ptr %local_1_, align 1
  br label %bb_2_

bb_2_:                                            ; preds = %bb_0_, %bb_1_
  %load_store_tmp_3 = load i8, ptr %local_1_, align 1
  store i8 %load_store_tmp_3, ptr %local_5_, align 1
  %retval_ = load i8, ptr %local_5_, align 1
  ret i8 %retval_
}
