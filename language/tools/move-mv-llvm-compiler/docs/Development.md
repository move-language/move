# Developer guide

Use this doc to get started with move-to-llvm compiler development.
It is highly encouraged to read the documents linked in the [reference](#references) section when planning to contribute.

## Dependencies

In addition to [move-language/move](https://github.com/move-language/move/blob/main/language/documentation/tutorial/README.md) dependencies, following are needed:

> zlib (apt install zlib1g-dev)
> [lld](https://lld.llvm.org/)
> [Solana port of LLVM](https://github.com/solana-labs/llvm-project)
> [Solana platform tools](https://github.com/solana-labs/platform-tools)

## Setup

First, follow the setup instructions for the [move-language/move](https://github.com/move-language/move/blob/main/language/documentation/tutorial/README.md#step-0-installation) project as our setup assumes a working move-language repository. After that platform-tools need to be installed.

Testing requires an installation of the Solana [platform-tools](https://github.com/solana-labs/platform-tools). Platform-tools releases provide a build of llvm (packaged as move-dev tools) that supports the Solana variant of eBPF.

Known working revision:
- platform-tools: version `1.39`
- move-dev: version `1.39`

`platform-tools` can be extracted from the binary release.

Export two environment variables:

- `PLATFORM_TOOLS_ROOT` - the path at which `platform-tools` was extracted
- `LLVM_SYS_150_PREFIX` - the path at which `move-dev` tools was extracted (as it has llvm binaries, including llvm-config that llvm-sys can use)

### After a toolchain update

You might run into build errors because of incompatible artifacts etc. due to a toolchain update.
In that case you need to uninstall and reinstall. For example:

```sh
rustup toolchain uninstall 1.65.0
rustup toolchain install 1.69.0
```

### Instructions to get solana-labs/platform-tools and move-dev tools

```sh
$ cd /path/to/platform-tools/releases/
# For OSX download (platform-tools-osx-x86_64.tar.bz2, move-dev-osx-x86_64.tar.bz2) for X86, or (move-dev-osx-x86_64.tar.bz2, move-dev-osx-aarch64.tar.bz2) for arm64 devices.

$ wget https://github.com/solana-labs/platform-tools/releases/download/v1.39/move-dev-linux-x86_64.tar.bz2
$ mkdir v1.39 && cd v1.39
$ tar -xf ../platform-tools-linux-x86_64.tar.bz2
$ ls /path/to/platform-tools/releases/v1.39/move-dev
bin include lib
$ wget https://github.com/solana-labs/platform-tools/releases/download/v1.39/platform-tools-linux-x86_64.tar.bz2
$ tar -xf platform-tools-linux-x86_64.tar.bz2
$ ls /path/to/platform-tools/releases/v1.39/platform-tools
llvm rust version.md
$ export PLATFORM_TOOLS_ROOT=/path/to/platform-tools/releases/v1.39/platform-tools
$ LLVM_SYS_150_PREFIX=/path/to/platform-tools/releases/v1.39/move-dev
```

## Building

```sh
# export LLVM_SYS_150_PREFIX (See Instructions to get move-dev tools)
# export PLATFORM_TOOLS_ROOT (See Instructions to get solana-labs/platform-tools)
cargo build -p move-ir-compiler -p move-compiler -p move-stdlib
```

## Testing

This project contains three test suites:

- `ir-tests` - converts Move IR (`.mvir`) to LLVM IR
- `move-ir-tests` - converts Move source (`.move`) to LLVM IR
- `rbpf-tests` - runs move as SBF in the `rbpf` VM
- `move-stdlib-tests` - runs move-stdlib tests

These test require the `move-ir-compiler` and `move-build` tools (See: [Build instructions](#building)). If you forget, the test harness will remind you what commands to run to build the tools.

Run the tests with any of these commands:

```sh
# export LLVM_SYS_150_PREFIX (See Instructions to build solana-labs/llvm-project)
# export PLATFORM_TOOLS_ROOT (See Instructions to get solana-labs/platform-tools)
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

### Additional tests

We also run tests for move-stdlib, and move-unit-tests in our ci. These are useful to run
locally if you make changes to move-stdlib for example.

- `move-stdlib-tests-solana` - Tests in the language/move-stdlib
- `move-unit-test-framework-tests-solana` - Solana-based move unit test framework tests

```sh
# export LLVM_SYS_150_PREFIX (See Instructions to build solana-labs/llvm-project)
# export PLATFORM_TOOLS_ROOT (See Instructions to get solana-labs/platform-tools)
# export MOVE_NATIVE=/path/to/move-repo/language/move-native
cargo test --features solana-backend -p move-cli --test build_testsuite_solana --test move_unit_tests_solana -- --test-threads 1
cargo run --features solana-backend -p move-cli --bin move -- test --solana -p language/move-stdlib
```

### Environment variables to control rbpf-tests

#### `TRACE`

Enable SBF instruction tracing/disassembly for a rbpf case. This is an extremely valuable debugging tool when an rbpf test crashes in the `move-native` library-- or perhaps worse-- in core rust libraries. To enable, set environment variable `TRACE` to a filename where the output will be directed. Setting `TRACE=` or `TRACE=stdout` writes the output to stdout.

```sh
TRACE=foo.txt cargo test -p move-mv-llvm-compiler --test rbpf-tests my_test_case
```

#### `DUMP`

Setting this environment variable will enable the test driver to output `// log` messages.

```sh
DUMP=1 cargo test -p move-mv-llvm-compiler --test rbpf-tests
```

## Test directives

Tests support "directives", written as comments at the top of the file,
that are interpreted by the test runner to determine if the test is successful:

They look like this:

```move
// abort 10

script {
  fun main() {
    assert!(1 == 2, 10);
  }
}
```

Supported directives include:

- `// ignore` - don't run the test, for broken tests.
- `// abort {code}` - expect an abort with code.
- `// log {string}` - expect a string to be logged by the `debug::print` function.
- `// signers {signer0,signer1,...}` - provide a list of signers to script `main`. Each signer is injected into a corresponding argument of main with type `signer`. See example below.

```move
// signers 0xcafe,0xf00d,0xc0ffee,0xb00
   ...
script {
    fun main(s1: signer, s2: signer, s3: signer, s4: signer) {
       ...
    }
}
```

`abort`, `log`, and `signers` are only supported by the `rbpf-tests` runner.

## Debugging

### Setting up llvm, llvm-sys for debugging

- Build llvm with debug symbols

### Debugging inside rbpf vm

Install [CodeLLDB plugin](https://marketplace.visualstudio.com/items?itemName=vadimcn.vscode-lldb)

- lldb with gdbserver

To debug in VS Code add this config:
 {
    "type": "lldb",
    "request": "launch",
    "name": "dwarf-tests direct call",
    "env": {
            "RUST_BACKTRACE": "all",
            "RUST_LOG": "debug",
            "CARGO_MANIFEST_DIR": "something like /home/sol/work/git/move/language/tools/move-mv-llvm-compiler",
            "LLVM_SYS_150_PREFIX": "something like /home/sol/work/git/platform-tools/out/rust/build/x86_64-unknown-linux-gnu/llvm",
            "PLATFORM_TOOLS_ROOT": "something like /home/sol/work/git/platform-tools/out/deploy"
    },
    "program": "something like /home/sol/work/git/move/target/debug/deps/dwarf_tests-XXXXXXXXXXXXXXXX",
    "args": ["--test"],
    "cwd": "something like /home/sol/work/git/move/language/tools/move-mv-llvm-compiler",
    "stopOnEntry": false
 }

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
To debug use the `RUST_BACKTRACE` environment variables

```sh
RUST_BACKTRACE=<value> rust-exe [args]
RUST_BACKTRACE=1 move-mv-llvm-compiler -b tests/BasicCoin.mv
RUST_BACKTRACE=full move-mv-llvm-compiler -b tests/BasicCoin.mv
```

----
Error: DEP_LLVM_CONFIG_PATH not set

DEP_LLVM_CONFIG_PATH is set by [llvm-sys](https://gitlab.com/taricorp/llvm-sys.rs/-/blob/main/build.rs#L452)
When this error occurs, it means that your llvm-sys isn't setup properly.

----
Instead of calling `--help` on the move-mv-llvm-compiler use `cargo run -- --help`

----
Use [RUST_LOG](https://docs.rs/env_logger/latest/env_logger/) environment variable to print compiler logs.
For example

> RUST_LOG=info move-mv-llvm-compiler -b tests/BasicCoin.mv

----
On MacOS, some builds and tests might spuriously fail because the tools downloaded may not be trusted by the OS. To get around that run the following command in each directory where binaries are there e.g., `move-dev/bin` and `rust/bin`

> xattr -d com.apple.quarantine  *
Note that this will not remove the attribute from symlinks. For symlinks you have to do it separately or use bash tricks like
> for i in `find . -type l`; do echo $i; mv $i $i.tmp; done ; # rename all the symlinks to .tmp
> for i in `ls -1 *.tmp`; do l=$(readlink $i); b=${i%.tmp}; echo $l; ln -s $l $b; done; # create new symlinks
> rm *.tmp # remove the old ones
## Submission

Only github pull requests are accepted. Typically contributors would fork this repo
and contribute make changes to their fork in a branch. Then create a pull-request
to solana-labs/move repostitory. Add at least one reviewer.

Before creating a pull request, make sure to:

- Run all tests
- Run the code formatter `cargo x fmt`
- Run the linters to pass pre-submit checks
  - `cargo x lint`
  - `cargo x clippy --workspace --all-targets` Note that clippy sometimes [does not lint all files](https://users.rust-lang.org/t/why-does-clippy-not-always-display-suggestions-for-me/32120/4). You might want to `cargo clean` in that case.

## References

Recommended reading

- [bytecode-instruction-semantics](https://docs.google.com/spreadsheets/d/1b3ccBcM8p76GTR7p_a0Kz3cO-oIXvCa3G90bXw_W-io)
- [Tips for writing bytecode tools for Move](https://github.com/move-language/move/issues/817)

References on move

- [move book](https://move-language.github.io/move)
- [move paper](https://developers.libra-china.org/docs/assets/papers/libra-move-a-language-with-programmable-resources.pdf)
- [Presentation by Sam](https://www.youtube.com/watch?v=J1U_0exNFu0)
- [Presentation by Sam](https://www.youtube.com/watch?v=b_2jZ4YEfWc)
