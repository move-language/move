# Modules and Scripts

Move has two different types of programs: ***Modules*** and ***Scripts***. Modules are libraries that define struct types along with functions that operate on these types. Struct types define the schema of Move's [global storage](./global-storage-structure.md), and module functions define the rules for updating storage. Modules themselves are also stored in global storage. Scripts are executable entrypoints similar to a `main` function in a conventional language. A script typically calls functions of a published module that perform updates to global storage. Scripts are ephemeral code snippets that are not published in global storage.

A Move source file (or **compilation unit**) may contain multiple modules and scripts. However, publishing a module or executing a script are separate VM operations.
# 模块和脚本
Move 有两种不同类型的程序：模块和脚本。模块是定义结构类型以及对这些类型进行操作的函数的库。结构类型定义了 Move 的全局存储模式，模块函数定义了更新存储的规则。模块本身也存储在全局存储中。脚本是可执行的入口点，类似于传统语言中的 main 函数。脚本通常调用已发布模块的函数，这些函数执行对全局存储的更新。脚本是未在全局存储中发布的临时代码片段。

一个 Move 源文件（或编译单元）可能包含多个模块和脚本。但是，发布模块或执行脚本是单独的 VM 操作。

## Syntax

### Scripts

A script has the following structure:
### 脚本
脚本具有以下结构：

```text
script {
    <use>*
    <constants>*
    fun <identifier><[type parameters: constraint]*>([identifier: type]*) <function_body>
}
```

A `script` block must start with all of its [use](./uses.md) declarations, followed by any [constants](./constants.md) and (finally) the main
[function](./functions.md) declaration.
The main function can have any name (i.e., it need not be called `main`), is the only function in a script block, can have any number of
arguments, and must not return a value. Here is an example with each of these components:

脚本块必须以它的所有 use 声明开始，然后是任何常量和（最后）主函数声明。 main 函数可以有任何名称（即不必称为 main），是脚本块中的唯一函数，可以有任意数量的参数，并且不能返回值。以下是每个组件的示例：

```move
script {
    // Import the Debug module published at the named account address std.
    use std::debug;

    const ONE: u64 = 1;

    fun main(x: u64) {
        let sum = x + ONE;
        debug::print(&sum)
    }
}
```

Scripts have very limited power—they cannot declare friends, struct types or access global storage. Their primary purpose is to invoke module functions.

脚本的功能非常有限——它们不能声明朋友、结构类型或访问全局存储。它们的主要目的是调用模块函数。

### Modules

A Module has the following syntax:
### 模块
模块具有以下语法：

```text
module <address>::<identifier> {
    (<use> | <friend> | <type> | <function> | <constant>)*
}
```

where `<address>` is a valid [named or literal address](./address.md).

For example:

其中地址是有效的命名或文字地址。

例如：

```move
module 0x42::Test {
    struct Example has copy, drop { i: u64 }

    use std::debug;
    friend 0x42::AnotherTest;

    const ONE: u64 = 1;

    public fun print(x: u64) {
        let sum = x + ONE;
        let example = Example { i: sum };
        debug::print(&sum)
    }
}
```

The `module 0x42::Test` part specifies that the module `Test` will be published under the [account address](./address.md) `0x42` in [global storage](./global-storage-structure.md).

Modules can also be declared using [named addresses](./address.md). For example:

模块 0x42::Test 部分指定模块 Test 将发布在全局存储中的账户地址 0x42 下。

模块也可以使用命名地址来声明。例如：

```move
module test_addr::test {
    struct Example has copy, drop { a: address}

    use std::debug;
    friend test_addr::another_test;

    public fun print() {
        let example = Example { a: @test_addr};
        debug::print(&example)
    }
}
```

Because named addresses only exist at the source language level and during compilation,
named addresses will be fully substituted for their value at the bytecode
level. For example if we had the following code:

因为命名地址只存在于源语言级别和编译期间，所以命名地址将在字节码级别完全替换它们的值。例如，如果我们有以下代码：

```move=
script {
    fun example() {
        my_addr::m::foo(@my_addr);
    }
}
```

and we compiled it with `my_addr` set to `0xC0FFEE`, then it would be equivalent
to the following operationally:

我们在 my_addr 设置为 0xC0FFEE 的情况下编译它，那么它在操作上等同于以下内容：

```move=
script {
    fun example() {
        0xC0FFEE::m::foo(@0xC0FFEE);
    }
}
```

However at the source level, these _are not equivalent_—the function
`M::foo` _must_ be accessed through the `MyAddr` named address, and not through
the numerical value assigned to that address.

Module names can start with letters `a` to `z` or letters `A` to `Z`. After the first character, module names can contain underscores `_`, letters `a` to `z`, letters `A` to `Z`, or digits `0` to `9`.

然而，在源代码级别，这些是不等价的——函数 M::foo 必须通过 MyAddr 命名地址访问，而不是通过分配给该地址的数值。

模块名称可以以字母 a 到 z 或字母 A 到 Z 开头。在第一个字符之后，模块名称可以包含下划线 `_`、字母 a 到 z、字母 A 到 Z 或数字 0 到 9。

```move
module my_module {}
module foo_bar_42 {}
```

Typically, module names start with an uppercase letter. A module named `my_module` should be stored in a source file named `my_module.move`.

All elements inside a `module` block can appear in any order.
Fundamentally, a module is a collection of [`types`](./structs-and-resources.md) and [`functions`](./functions.md).
[Uses](./uses.md) import types from other modules.
[Friends](./friends.md) specify a list of trusted modules.
[Constants](./constants.md) define private constants that can be used in the functions of a module.

通常，模块名称以大写字母开头。名为 my_module 的模块应存储在名为 my_module.move 的源文件中。

模块块内的所有元素都可以按任意顺序出现。从根本上说，模块是类型和函数的集合。使用来自其他模块的导入类型。朋友指定受信任模块的列表。常量定义了可以在模块的函数中使用的私有常量。
