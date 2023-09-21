# Move to llvm-ir compiler

Generates llvm-ir from move bytecode. It provides a bridge between move-bytecode and llvm.
It leverages the llvm’s Rust C bindings (llvm-sys) and can support all targets llvm supports.
For example, it can generate eBPF or executable binaries.

> move-mv-llvm-compiler : move-vm :: Docker : Virtual Machine

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

### Codegen of generics

Code generation of generics (function signatures or structs that are parameterized by types) are supported (except Move entry functions). Currently these implemented as follows:

In the case of ordinary Move function generic signatures, we expand (aka monomorphize/concretize) each function
into a concrete instantiation. This is essentially similar to what is done for C++ template instantiation
or for Rust generics. In the case of **native** Move functions, we leave them generic and pass
an implicit `MoveType` parameter for each generic type parameter. The move-native library routines
must interpret these types at runtime. This interpretation is what accounts for much of the
complexity in what is otherwise a simple library.

## Optimizations

TODO:

## Calling convention

Calls and returns are described in the [move-book#calling](https://move-language.github.io/move/functions.html#calling). For the most part,
it is similar to Rust's calling convention. For example, varargs aren't supported.

Functions that return multiple values, use a second-class tuple-like expression to bind, return, and destructure multiple values.

On exit from a function, we generate LLVM IR to wrap them up into a struct, which is returned as a single IR value. Similarly, when a callee that returns such a value is used in an expression, we generate IR to extract each actual value from the struct. (See [PR#105](https://github.com/solana-labs/move/pull/105))

## Serialization/Deserialization byte order

Data passed to the entrypoint from the VM is serialized from structs in little-endian order.
The rbpf VM [supports little endian](https://github.com/qmonnet/rbpf/blob/main/src/ebpf.rs).

## Support for native function calls

Some functions do not have a body specified, and instead have the body provided by the VM. These are called [native functions](https://move-language.github.io/move/functions.html#native-functions). These functions as part of the [move-native](https://github.com/solana-labs/move/tree/llvm-sys/language/move-native) runtime shipped with the compiler.

- [abort](https://move-language.github.io/move/abort-and-assert.html)
- [return](https://move-language.github.io/move/functions.html#return-type)
- [assert](https://move-language.github.io/move/abort-and-assert.html)
- debug_print

## FFI

- Checks before and after calling external functions

## Type information

- For Debugging
- TypeInfo in the binary
- Format of typeinfo

## Testing

Every level of translation is tested. In addition to move-language tests,
following are the [additional tests](https://github.com/solana-labs/move/tree/llvm-sys/language/tools/move-mv-llvm-compiler/tests).

- move to move-ir
- executable tests in rbpf
- move to llvm ir unit tests
- stdlib tests

## Project stages

See [project milestones](https://github.com/solana-labs/move/milestones)
