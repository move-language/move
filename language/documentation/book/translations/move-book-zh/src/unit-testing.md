# Unit Tests

Unit testing for Move adds three new annotations to the Move source language:

* `#[test]`
* `#[test_only]`, and
* `#[expected_failure]`.

They respectively mark a function as a test, mark a module or module member (`use`, function, or struct) as code to be included for testing only, and mark that a test is expected to fail. These annotations can be placed on a function with any visibility. Whenever a module or module member is annotated as `#[test_only]` or `#[test]`, it will not be included in the compiled bytecode unless it is compiled for testing.
# 单元测试
Move 的单元测试为 Move 源语言添加了三个新注释：

* #[test]
* #[test_only]，和
* #[expected_failure]。

它们分别将函数标记为测试，将模块或模块成员（使用、函数或结构）标记为仅用于测试的代码，并标记预期测试将失败。这些注释可以放置在具有任何可见性的函数上。每当一个模块或模块成员被注释为 `#[test_only]` 或 `#[test]` 时，它不会包含在编译的字节码中，除非它被编译用于测试。

## Testing Annotations: Their Meaning and Usage

Both the `#[test]` and `#[expected_failure]` annotations can be used either with or without arguments.

Without arguments, the `#[test]` annotation can only be placed on a function with no parameters. This annotation simply marks this function as a test to be run by the unit testing harness.

## 测试注释：它们的含义和用法
`#[test]` 和 `#[expected_failure]` 注释都可以带或不带参数使用。

没有参数，#[test] 注释只能放在没有参数的函数上。此注释只是将此函数标记为要由单元测试工具运行的测试。

```
#[test] // OK
fun this_is_a_test() { ... }

#[test] // Will fail to compile since the test takes an argument
fun this_is_not_correct(arg: signer) { ... }
```

A test can also be annotated as an `#[expected_failure]`. This annotation marks that the test should is expected to raise an error. You can ensure that a test is aborting with a specific abort code by annotating it with `#[expected_failure(abort_code = <code>)]`, if it then fails with a different abort code or with a non-abort error the test will fail. Only functions that have the `#[test]` annotation can also be annotated as an #`[expected_failure]`.

测试也可以注释为 `#[expected_failure]`。此注释标志着测试应该会引发错误。您可以通过使用 `#[expected_failure(abort_code = code)]` 对其进行注释来确保测试使用特定的中止代码中止，如果它随后因不同的中止代码或非中止错误而失败，则测试将失败。只有具有 `#[test]` 注释的函数也可以注释为 `#[expected_failure]`。

```
#[test]
#[expected_failure]
public fun this_test_will_abort_and_pass() { abort 1 }

#[test]
#[expected_failure]
public fun test_will_error_and_pass() { 1/0; }

#[test]
#[expected_failure(abort_code = 0)]
public fun test_will_error_and_fail() { 1/0; }

#[test, expected_failure] // Can have multiple in one attribute. This test will pass.
public fun this_other_test_will_abort_and_pass() { abort 1 }
```

With arguments, a test annotation takes the form `#[test(<param_name_1> = <address>, ..., <param_name_n> = <address>)]`. If a function is annotated in such a manner, the function's parameters must be a permutation of the parameters <`param_name_1>, ..., <param_name_n>`, i.e., the order of these parameters as they occur in the function and their order in the test annotation do not have to be the same, but they must be able to be matched up with each other by name.

Only parameters with a type of `signer` are supported as test parameters. If a non-`signer` parameter is supplied, the test will result in an error when run.

带有参数的测试注解采用 `#[test( param_name_1 = address , ..., param_name_n = address )]` 的形式。如果以这种方式注释函数，则函数的参数必须是参数 param_name_1 , ..., param_name_n 的排列，即这些参数在函数中出现的顺序和它们在测试注释中的顺序不必须相同，但它们必须能够通过名称相互匹配。

仅支持具有签名者类型的参数作为测试参数。如果提供了非签名者参数，则测试将在运行时导致错误。

```
#[test(arg = @0xC0FFEE)] // OK
fun this_is_correct_now(arg: signer) { ... }

#[test(wrong_arg_name = @0xC0FFEE)] // Not correct: arg name doesn't match
fun this_is_incorrect(arg: signer) { ... }

#[test(a = @0xC0FFEE, b = @0xCAFE)] // OK. We support multiple signer arguments, but you must always provide a value for that argument
fun this_works(a: signer, b: signer) { ... }

// somewhere a named address is declared
#[test_only] // test-only named addresses are supported
address TEST_NAMED_ADDR = @0x1;
...
#[test(arg = @TEST_NAMED_ADDR)] // Named addresses are supported!
fun this_is_correct_now(arg: signer) { ... }
```

An expected failure annotation can also take the form `#[expected_failure(abort_code = <u64>)]`. If a test function is annotated in such a way, the test must abort with an abort code equal to `<u64>`. Any other failure or abort code will result in a test failure.

预期的失败注释也可以采用 #[expected_failure(abort_code = u64)] 的形式。如果以这种方式注释测试函数，则必须使用等于 u64 的中止代码中止测试。任何其他失败或中止代码都将导致测试失败。

```
#[test, expected_failure(abort_code = 1)] // This test will fail
fun this_test_should_abort_and_fail() { abort 0 }

#[test]
#[expected_failure(abort_code = 0)] // This test will pass
fun this_test_should_abort_and_pass_too() { abort 0 }
```

A module and any of its members can be declared as test only. In such a case the item will only be included in the compiled Move bytecode when compiled in test mode. Additionally, when compiled outside of test mode, any non-test `use`s of a `#[test_only]` module will raise an error during compilation.

一个模块及其任何成员都可以声明为仅测试。在这种情况下，只有在测试模式下编译时，该项目才会包含在编译后的 Move 字节码中。此外，在测试模式之外编译时，#[test_only] 模块的任何非测试使用都会在编译期间引发错误。

```
#[test_only] // test only attributes can be attached to modules
module abc { ... }

#[test_only] // test only attributes can be attached to named addresses
address ADDR = @0x1;

#[test_only] // .. to uses
use 0x1::some_other_module;

#[test_only] // .. to structs
struct SomeStruct { ... }

#[test_only] // .. and functions. Can only be called from test code, but not a test
fun test_only_function(...) { ... }
```

## Running Unit Tests

Unit tests for a Move package can be run with the [`move test`
command](./packages.md).

When running tests, every test will either `PASS`, `FAIL`, or `TIMEOUT`. If a test case fails, the location of the failure along with the function name that caused the failure will be reported if possible. You can see an example of this below.

A test will be marked as timing out if it exceeds the maximum number of instructions that can be executed for any single test. This bound can be changed using the options below, and its default value is set to 5000 instructions. Additionally, while the result of a test is always deterministic, tests are run in parallel by default, so the ordering of test results in a test run is non-deterministic unless running with only one thread (see `OPTIONS` below).

There are also a number of options that can be passed to the unit testing binary to fine-tune testing and to help debug failing tests. These can be found using the the help flag:
## 运行单元测试
可以使用 move test 命令运行 Move 包的单元测试。

运行测试时，每个测试都将通过、失败或超时。如果测试用例失败，将尽可能报告失败的位置以及导致失败的函数名称。您可以在下面看到一个示例。

如果测试超过任何单个测试可以执行的最大指令数，则测试将被标记为超时。可以使用以下选项更改此界限，其默认值设置为 5000 条指令。此外，虽然测试的结果始终是确定性的，但默认情况下测试是并行运行的，因此测试运行中测试结果的顺序是不确定的，除非仅使用一个线程运行（请参阅下面的选项）。

还有许多选项可以传递给单元测试二进制文件以微调测试并帮助调试失败的测试。这些可以使用帮助标志找到：

```
$ move -h
```

## Example

A simple module using some of the unit testing features is shown in the following example:

First create an empty package and change directory into it:

## 例子
以下示例显示了使用一些单元测试功能的简单模块：

首先创建一个空包并将目录更改为它：

```
$ move new TestExample; cd TestExample
```

Next add the following to the `Move.toml`:

接下来将以下内容添加到 Move.toml：

```
[dependencies]
MoveStdlib = { git = "https://github.com/diem/diem.git", subdir="language/move-stdlib", rev = "56ab033cc403b489e891424a629e76f643d4fb6b", addr_subst = { "std" = "0x1" } }
```

Next add the following module under the `sources` directory:

接下来在源目录下添加以下模块：

```
// filename: sources/my_module.move
module 0x1::my_module {

    struct MyCoin has key { value: u64 }

    public fun make_sure_non_zero_coin(coin: MyCoin): MyCoin {
        assert!(coin.value > 0, 0);
        coin
    }

    public fun has_coin(addr: address): bool {
        exists<MyCoin>(addr)
    }

    #[test]
    fun make_sure_non_zero_coin_passes() {
        let coin = MyCoin { value: 1 };
        let MyCoin { value: _ } = make_sure_non_zero_coin(coin);
    }

    #[test]
    // Or #[expected_failure] if we don't care about the abort code
    #[expected_failure(abort_code = 0)]
    fun make_sure_zero_coin_fails() {
        let coin = MyCoin { value: 0 };
        let MyCoin { value: _ } = make_sure_non_zero_coin(coin);
    }

    #[test_only] // test only helper function
    fun publish_coin(account: &signer) {
        move_to(account, MyCoin { value: 1 })
    }

    #[test(a = @0x1, b = @0x2)]
    fun test_has_coin(a: signer, b: signer) {
        publish_coin(&a);
        publish_coin(&b);
        assert!(has_coin(@0x1), 0);
        assert!(has_coin(@0x2), 1);
        assert!(!has_coin(@0x3), 1);
    }
}
```

### Running Tests

You can then run these tests with the `move test` command:

### 运行测试

然后，您可以使用 move test 命令运行这些测试：

```
$ move test
BUILDING MoveStdlib
BUILDING TestExample
Running Move unit tests
[ PASS    ] 0x1::my_module::make_sure_non_zero_coin_passes
[ PASS    ] 0x1::my_module::make_sure_zero_coin_fails
[ PASS    ] 0x1::my_module::test_has_coin
Test result: OK. Total tests: 3; passed: 3; failed: 0
```

### Using Test Flags

#### `-f <str>` or `--filter <str>`
This will only run tests whose fully qualified name contains `<str>`. For example if we wanted to only run tests with `"zero_coin"` in their name:

### 使用测试标志
#### `-f <str>` 或 `--filter <str>`
这只会运行完全限定名称包含 str 的测试。例如，如果我们只想运行名称中带有“zero_coin”的测试：

```
$ move test -f zero_coin
CACHED MoveStdlib
BUILDING TestExample
Running Move unit tests
[ PASS    ] 0x1::my_module::make_sure_non_zero_coin_passes
[ PASS    ] 0x1::my_module::make_sure_zero_coin_fails
Test result: OK. Total tests: 2; passed: 2; failed: 0
```

#### `-i <bound>` or `--instructions <bound>`
This bounds the number of instructions that can be executed for any one test to `<bound>`:

#### `-i <bound>` 或 `--instructions <bound>`
这将任何一个测试可以执行的指令数限制为 bound ：

```
$ move test -i 0
CACHED MoveStdlib
BUILDING TestExample
Running Move unit tests
[ TIMEOUT ] 0x1::my_module::make_sure_non_zero_coin_passes
[ TIMEOUT ] 0x1::my_module::make_sure_zero_coin_fails
[ TIMEOUT ] 0x1::my_module::test_has_coin

Test failures:

Failures in 0x1::my_module:

┌── make_sure_non_zero_coin_passes ──────
│ Test timed out
└──────────────────


┌── make_sure_zero_coin_fails ──────
│ Test timed out
└──────────────────


┌── test_has_coin ──────
│ Test timed out
└──────────────────

Test result: FAILED. Total tests: 3; passed: 0; failed: 3
```

#### `-s` or `--statistics`
With these flags you can gather statistics about the tests run and report the runtime and instructions executed for each test. For example, if we wanted to see the statistics for the tests in the example above:

#### `-s` 或 `--statistics`

使用这些标志，您可以收集有关测试运行的统计信息，并报告每个测试的运行时间和执行的指令。例如，如果我们想查看上例中测试的统计信息：

```
$ move test -s
CACHED MoveStdlib
BUILDING TestExample
Running Move unit tests
[ PASS    ] 0x1::my_module::make_sure_non_zero_coin_passes
[ PASS    ] 0x1::my_module::make_sure_zero_coin_fails
[ PASS    ] 0x1::my_module::test_has_coin

Test Statistics:

┌────────────────────────────────────────────────┬────────────┬───────────────────────────┐
│                   Test Name                    │    Time    │   Instructions Executed   │
├────────────────────────────────────────────────┼────────────┼───────────────────────────┤
│ 0x1::my_module::make_sure_non_zero_coin_passes │   0.009    │             1             │
├────────────────────────────────────────────────┼────────────┼───────────────────────────┤
│ 0x1::my_module::make_sure_zero_coin_fails      │   0.008    │             1             │
├────────────────────────────────────────────────┼────────────┼───────────────────────────┤
│ 0x1::my_module::test_has_coin                  │   0.008    │             1             │
└────────────────────────────────────────────────┴────────────┴───────────────────────────┘

Test result: OK. Total tests: 3; passed: 3; failed: 0
```

#### `-g` or `--state-on-error`
These flags will print the global state for any test failures. e.g., if we added the following (failing) test to the `my_module` example:

#### `-g` 或 `--state-on-error`
这些标志将打印任何测试失败的全局状态。例如，如果我们将以下（失败）测试添加到 my_module 示例中：

```
module 0x1::my_module {
    ...
    #[test(a = @0x1)]
    fun test_has_coin_bad(a: signer) {
        publish_coin(&a);
        assert!(has_coin(@0x1), 0);
        assert!(has_coin(@0x2), 1);
    }
}
```

we would get get the following output when running the tests:

运行测试时我们会得到以下输出：

```
$ move test -g
CACHED MoveStdlib
BUILDING TestExample
Running Move unit tests
[ PASS    ] 0x1::my_module::make_sure_non_zero_coin_passes
[ PASS    ] 0x1::my_module::make_sure_zero_coin_fails
[ PASS    ] 0x1::my_module::test_has_coin
[ FAIL    ] 0x1::my_module::test_has_coin_bad

Test failures:

Failures in 0x1::my_module:

┌── test_has_coin_bad ──────
│ error[E11001]: test failure
│    ┌─ /home/tzakian/TestExample/sources/my_module.move:47:10
│    │
│ 44 │      fun test_has_coin_bad(a: signer) {
│    │          ----------------- In this function in 0x1::my_module
│    ·
│ 47 │          assert!(has_coin(@0x2), 1);
│    │          ^^^^^^^^^^^^^^^^^^^^^^^^^^ Test was not expected to abort but it aborted with 1 here
│
│
│ ────── Storage state at point of failure ──────
│ 0x1:
│       => key 0x1::my_module::MyCoin {
│           value: 1
│       }
│
└──────────────────

Test result: FAILED. Total tests: 4; passed: 3; failed: 1
```
