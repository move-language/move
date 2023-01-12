; ModuleID = 'script'
source_filename = "script.bc"
target triple = "bpfel-unknown-unknown"

define void @main() {
entry:
  ret i64 0
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i64 3}
