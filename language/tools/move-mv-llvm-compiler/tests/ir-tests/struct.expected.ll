; ModuleID = 'module 1.TestBinaryOps'
source_filename = "1.TestBinaryOps.bc"
target triple = "bpfel-unknown-unknown"

%T = type { i1 }

@T = external global %T

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i64 3}
