# move bytecode to llvm bitcode compiler
Takes move bytecode in text or binary format

See: `move-mv-llvm-compiler --help` for cli.

```sh
./target/debug/move-mv-llvm-compiler -b function.mv -o a
```

Compile move-bytecode to llvm bitcode

## Overview
The llvm-compiler uses [Inkwell](https://github.com/TheDan64/inkwell) (Safe Rust bindings to LLVM's C API) to generate llvm bitcode for Move IR.

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

> Inkwell
> { git = "https://github.com/TheDan64/inkwell", branch = "master", features = ["llvm14-0"] }

> zlib zlib1g-dev

> LLVM: llvm-14-dev

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
To generate a move bytecode module from mvir

> move-ir-compiler -m a.mvir

----
To generate bytecode in text format
> move-disassembler --bytecode a.mv

