; ModuleID = 'module 42.M'
source_filename = "42.M.bc"
target triple = "bpfel-unknown-unknown"

%S = type { i64 }

@S = external global %S

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i64 3}
