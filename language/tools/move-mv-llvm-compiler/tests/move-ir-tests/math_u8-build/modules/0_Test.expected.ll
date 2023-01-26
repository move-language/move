; ModuleID = '0x100__Test'
source_filename = "<unknown>"
target triple = "bpfel-unknown-unknown"

define i8 @test(i8 %0, i8 %1) {
entry_:
  %local_0_ = alloca i8, align 1
  %local_1_ = alloca i8, align 1
  %local_2_ = alloca i8, align 1
  %local_3_ = alloca i8, align 1
  %local_4_ = alloca i8, align 1
  store i8 %0, ptr %local_0_, align 1
  store i8 %1, ptr %local_1_, align 1
  %load_store_tmp_ = load i8, ptr %local_0_, align 1
  store i8 %load_store_tmp_, ptr %local_2_, align 1
  %load_store_tmp_1 = load i8, ptr %local_1_, align 1
  store i8 %load_store_tmp_1, ptr %local_3_, align 1
  %add_src_0_ = load i8, ptr %local_2_, align 1
  %add_src_1_ = load i8, ptr %local_3_, align 1
  %add_dst_ = add i8 %add_src_0_, %add_src_1_
  store i8 %add_dst_, ptr %local_4_, align 1
  %retval_ = load i8, ptr %local_4_, align 1
  ret i8 %retval_
}
