# Move to llvm-ir compiler
Generates llvm-ir from move bytecode. It provides a bridge between move-bytecode and llvm.
It leverages the llvm’s Rust C bindings (llvm-sys) and can support all targets llvm supports.
For example, it can generate eBPF or executable binaries.


move-mv-llvm-compiler : move-vm :: Docker : Virtual Machine

## Goals of the compiler
- Retargettable move to llvm compiler

## Non-goals
-

## Components
- move-ir translator
- move-native runtime
- testing
- FFI

## Translation
All integer arithmetic types are [checked](https://move-language.github.io/move/integers.html#arithmetic) for narrowing, overflow and undefined behavior at runtime.

## Optimizations


## Calling convention


## FFI
- Checks before and after calling external functions

## Type information
- For Debugging
- TypeInfo in the binary
- Format of typeinfo

## Testing
- ir tests
- runnable tests


## Project stages:
### Stage 1:
- Lower Move bytecode to llvm-ir
  - Move bytecode docs [[binary-format](https://github.com/solana-labs/move/blob/main/language/move-binary-format/src/file_format.rs#L1107), [spec](https://github.com/solana-labs/move/blob/main/language/documentation/spec/vm.md)]
- Encode function arguments, returns to communicate with outside contracts using Borsh
  - [Intro](https://hexdocs.pm/borsh_serializer/readme.html), [Borsh serialization](https://solanacookbook.com/guides/serialization.html#setting-up-for-borsh-serialization)
- Add debug info to types.
- Add [BTF](https://github.com/cilium/cilium/blob/master/Documentation/bpf.rst) types to provide runtime type information
- Add definitions to bytecode keywords
  - Simple operations can be codegen in llvm itself  but high level operation can be just calls.
  - E.g., Lowering BorrowField (a move bytecode instruction) can be just a function call (with right set of parameter) that does the borrow as well as does runtime checks
  - The definitions maybe put in a rust library that is linked to the executable (see stage2)
- Testing
  - Unit
  - Github Hooks Integration to run pre-merge tests

