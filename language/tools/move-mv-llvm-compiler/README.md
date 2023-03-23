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
and testing requires an installation of the Solana [`sbf-tools`](https://github.com/solana-labs/sbf-tools).

Known working revisions of both:

- llvm-project: commit `a0bf4d22b6af79f5f4d9a0b42ac3ef855e79b602`,
  tag `15.0-2022-08-09`,
  from the `solana-labs` repo
- sbf-tools: version `1.32`

`sbf-tools` can be extracted from the binary release.

Export two environment variables:

- `LLVM_SYS_150_PREFIX` - the path to the LLVM build directory
- `SBF_TOOLS_ROOT` - the path at which `sbf-tools` was extracted

### Instructions to build solana-labs/llvm-project

```sh
$ git clone https://github.com/solana-labs/llvm-project
$ mkdir -p llvm-project/build && cd llvm-project/build
$ cmake -GNinja -DLLVM_TARGETS_TO_BUILD="X86;SBF;BPF" -DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_ENABLE_PROJECTS="clang;libcxx;libcxxabi;libunwind" ../llvm
$ ninja clang
$ export LLVM_SYS_150_PREFIX=/path/to/llvm-project/build
```

### Instructions to get solana-labs/sbf-tools

```
$ cd /path/to/sbf-tools/releases/
# For OSX download solana-bpf-tools-osx.tar.bz2
$ wget https://github.com/solana-labs/sbf-tools/releases/download/v1.32/solana-bpf-tools-linux.tar.bz2
$ mkdir v1.32 && cd v1.32
$ tar -xf ../solana-bpf-tools-linux.tar.bz2
$ ls /path/to/sbf-tools/releases/v1.32
llvm  rust  version.md
$ export SBF_TOOLS_ROOT=/path/to/sbf-tools/releases/v1.32
```

## Testing

This project contains three test suites:

- `ir-tests` - converts Move IR (`.mvir`) to LLVM IR,
- `move-ir-tests` - converts Move source (`.move`) to LLVM IR,
- `rbpf-tests` - runs move as SBF in the `rbpf` VM.

These test require the `move-ir-compiler` and `move-build` tools,
which can be built with

```sh
cargo build -p move-ir-compiler && cargo build -p move-compiler
```

If you forget, the test harness will remind you what commands to run to build the tools.

Run the tests with any of these commands:

```sh
cargo test -p move-mv-llvm-compiler --test ir-tests
cargo test -p move-mv-llvm-compiler --test move-ir-tests
cargo test -p move-mv-llvm-compiler --test rbpf-tests
```

The IR tests work by producing `.actual.ll` files and comparing them to
`.expected.ll` files. When introducing new tests, or making changes to the code
generator that invalidate existing tests, the "actual" files need to be promoted
to "expected" files. This can be done like

```sh
PROMOTE_LLVM_IR=1 cargo test -p move-mv-llvm-compiler --test move-ir-tests
```

Most new tests should be `move-ir-tests` or `rbpf-tests`,
as the Move IR is not stable nor easy to work with.

### TODO
Create issues instead of having TODOs.

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

**NB: Not working currently**. For IR tests:

```bash
cp move/language/tools/move-mv-llvm-compiler/tests/move-ir-tests/$test-build/modules/0_Test.actual.ll tests/move-ir-tests/$test-build/modules/0_Test.expected.ll
```


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

----
Error: DEP_LLVM_CONFIG_PATH not set

DEP_LLVM_CONFIG_PATH is set by [llvm-sys](https://gitlab.com/taricorp/llvm-sys.rs/-/blob/main/build.rs#L452)
When this error occurs, it means that your llvm-sys isn't setup properly.

## ACKNOWLEDGEMENTS

- Parts of [inkwell](https://github.com/TheDan64/inkwell) code has been copied to this subfolder.
It will be rewritten based on the needs of the project. If exact code is to be adopted, they will be
put in a sub folder with appropriate acknowledgement.

- The ziglang codebase has examples of LLVM C API that has been helpful.
https://git.sr.ht/~andrewrk/ziglang/tree/master
