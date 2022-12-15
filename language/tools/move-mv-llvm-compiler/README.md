# move bytecode to llvm bitcode compiler
Takes move bytecode in text or binary format

See: `move-mv-llvm-compiler --help` for cli.

```sh
./target/debug/move-mv-llvm-compiler -b function.mv -o a
```

Compile move-bytecode to llvm bitcode

## Overview
The llvm-compiler uses [Inkwell](https://github.com/TheDan64/inkwell) (Safe Rust bindings to LLVM's C API) to generate llvm bitcode for Move IR.


## Dependencies:
- llvm-project
- inkwell

## Testing

Current testing is very hacky, and only uses the following .mvir file
$ cat a.mvir
```move
//# print-bytecode --input=module
module 0x3d10.Example {
    public value(): u64 {
    label a:
        return 100;
    }
}
```

To generate a move bytecode file from mvir use the move-ir-compiler
> $MOVE_HOME/target/debug/move-ir-compiler -m a.mvir

This will generate `a.mv` file in the current directory. We use `a.mv` file for testing.

```sh
# Build move-mv-llvm-compiler
$ cd $MOVE_HOME/language/tools/move-mv-llvm-compiler/ && cargo build

# Generate llvm IR file from move bytecode
$ $MOVE_HOME/target/debug/move-mv-llvm-compiler -b a.mv -S -o a.ll

# Generate readable llvm bitcode file from binary representation
$ llvm-project/build/bin/opt -S 3d10.Example.bc
```

The above command will generate a llvm ir like this:

```llvm
; ../llvm-project/build/bin/opt -S 3d10.Example.bc
; ModuleID = '3d10.Example.bc'
source_filename = "3d10.Example.bc"
target triple = "bpfel-unknown-unknown"

define i64 @value() {
entry:
  ret i64 0
}

!llvm.module.flags = !{!0}
!llvm.dbg.cu = !{!1}

!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = distinct !DICompileUnit(language: DW_LANG_C, file: !2, producer: "Move", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false)
!2 = !DIFile(filename: "3d10.Example.bc", directory: ".")
```

### TODO

- Add runtime calls to builtins (https://arxiv.org/pdf/2004.05106.pdf#page=7) if there is no direct mapping to SBF. To start with, we can have each of these as part of runtime library and make optimizations as needed.

```
local variable instructions: MvLoc ⟨x⟩ | CpLoc ⟨c⟩ | StLoc ⟨x⟩ | BorrowLoc ⟨x⟩
reference instructions: ReadRef | WriteRef | FreezeRef
record instructions: Pack | Unpack | BorrowField ⟨f ⟩
global state instructions: MoveTo ⟨s⟩ | MoveFrom ⟨s⟩ | BorrowGlobal ⟨s⟩ | Exists ⟨s⟩
stack instructions: Pop | LoadConst ⟨a⟩ | Op
procedure instructions: Call ⟨h⟩ | Ret
```

- Add integration tests
- Add unit tests
- Add verification tests
- Add target triple
- Parse globals: see parse_module
- Parse decls
- Parse functions
    - parse parameters


### Dependencies

> zlib zlib1g-dev

> LLVM: llvm-15-dev

### Protip

----
In case `cargo build` fails in **Cargo.lock** file unable to resolve dependencies, try *regenerating* the **Cargo.lock** file with the following command.

> cargo generate-lockfile

----
Did you forget to set up environment variables?

> source ~/.profile

----
To update a test's expected output based on the existing output

> export UPDATE_BASELINE=1

And then run `cargo test`

----
To generate a move bytecode module (.mv file) from mvir file

> move-ir-compiler -m a.mvir

----
To generate bytecode in text format
> move-disassembler --bytecode a.mv


----
To debug use the RUST_BACKTRACE environment variables
> RUST_BACKTRACE=<value> rust-exe [args]

> RUST_BACKTRACE=1 move-mv-llvm-compiler -b tests/BasicCoin.mv
> RUST_BACKTRACE=full move-mv-llvm-compiler -b tests/BasicCoin.mv

## ACKNOWLEDGEMENTS
Parts of [inkwell]9https://github.com/TheDan64/inkwell) code has been copied to this subfolder.
It will be rewritten based on the needs of the project. If exact code is to be adopted, they will be
put in a sub folder with appropriate acknowledgement.
