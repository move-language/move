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

This project contains two test suites: `ir-tests` and `move-ir-tests`.
The first converts Move IR (`.mvir`) to LLVM IR,
and the second converts Move source (`.move`) to LLVM IR.
Both check the results against a reference IR file.

These test require the `move-ir-compiler` and `move-build` tools,
which can be built with

```
cargo build -p move-ir-compiler && cargo build -p move-compiler
```

Run the tests with either

```
cargo test -p move-mv-llvm-compiler --test ir-tests`
```

or

```
cargo test -p move-vm-llvm-compiler --test move-ir-tests`
```

These tests work by producing `.actual.ll` files and comparing them to
`.expected.ll` files. When introducing new tests, or making changes to the code
generator that invalidate existing tests, the "actual" files need to be promoted
to "expected" files. This can be done like

```
PROMOTE_LLVM_IR=1 cargo test -p move-vm-llvm-compiler --test move-ir-tests`
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
