; ModuleID = '0x100__Test'
source_filename = "<unknown>"
target triple = "bpfel-unknown-unknown"

define void @test() {
entry_:
  %local_0_ = alloca i64, align 8
  store i64 10, ptr %local_0_, align 4
  %call_arg_0_ = load i64, ptr %local_0_, align 4
  call void @move_rt_abort(i64 %call_arg_0_)
  unreachable
}

; Function Attrs: noreturn
declare void @move_rt_abort(i64) #0

attributes #0 = { noreturn }
