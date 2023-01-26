; ModuleID = '0x100__Test'
source_filename = "<unknown>"
target triple = "bpfel-unknown-unknown"

define i8 @test() {
entry_:
  %local_0_ = alloca i8, align 1
  store i8 100, ptr %local_0_, align 1
  %retval_ = load i8, ptr %local_0_, align 1
  ret i8 %retval_
}
