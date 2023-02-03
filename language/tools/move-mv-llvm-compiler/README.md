# move bytecode to llvm bitcode compiler
Takes move bytecode in text or binary format

See: `move-mv-llvm-compiler --help` for cli.

```sh
./target/debug/move-mv-llvm-compiler -b function.mv -o a
```

Compile move-bytecode to llvm bitcode

## Overview

The move compiler uses llvm-sys to interface with llvm. It translates stackless bytecode representation of move to llvm-ir.
Read more about bytecode translations from [here](https://github.com/move-language/move/issues/817)

## Setup

Building requires a local build of [`llvm-project`](https://github.com/solana-labs/llvm-project)
from Solana's fork that supports the Solana variant of eBPF,
and testing requires an installation of the Solana [`bpf-tools`](https://github.com/solana-labs/bpf-tools).

Known working revisions of both:

- llvm-project: commit `a0bf4d22b6af79f5f4d9a0b42ac3ef855e79b602`,
  tag `15.0-2022-08-09`,
  from the `solana-labs` repo
- bpf-tools: version `1.32`

`bpf-tools` can be extracted from the binary release.

Export two environment variables:

- `LLVM_SYS_150_PREFIX` - the path to the LLVM build directory
- `BPF_TOOLS_ROOT` - the path at which `bpf-tools` was extracted

## Testing

This project contains three test suites:

- `ir-tests` - converts Move IR (`.mvir`) to LLVM IR,
- `move-ir-tests` - converts Move source (`.move`) to LLVM IR,
- `rbpf-tests` - runs move as BPF in the `rbpf` VM.

These test require the `move-ir-compiler` and `move-build` tools,
which can be built with

```sh
cargo build -p move-ir-compiler && cargo build -p move-compiler
```

If you forget, the test harness will remind you what commands to run to build the tools.

Run the tests with any of these commands:

```sh
cargo test -p move-mv-llvm-compiler --test ir-tests
cargo test -p move-vm-llvm-compiler --test move-ir-tests
cargo test -p move-vm-llvm-compiler --test rbpf-tests
```

The IR tests work by producing `.actual.ll` files and comparing them to
`.expected.ll` files. When introducing new tests, or making changes to the code
generator that invalidate existing tests, the "actual" files need to be promoted
to "expected" files. This can be done like

```sh
PROMOTE_LLVM_IR=1 cargo test -p move-vm-llvm-compiler --test move-ir-tests
```

Most new tests should be `move-ir-tests` or `rbpf-tests`,
as the Move IR is not stable nor easy to work with.

### TODO

- Add runtime calls to builtins (https://arxiv.org/pdf/2004.05106.pdf#page=7) if there is no direct mapping to SBF. To start with, we can have each of these as part of runtime library and make optimizations as needed.

```txt
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

- Add analysis passes for basic optimizations like [reaching definitions](https://github.com/move-language/move/blob/main/language/move-prover/bytecode/src/reaching_def_analysis.rs):

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
