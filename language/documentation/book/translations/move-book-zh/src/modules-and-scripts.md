# 模块和脚本 (Modules and Scripts)

Move has two different types of programs: ***Modules*** and ***Scripts***. Modules are libraries that define struct types along with functions that operate on these types. Struct types define the schema of Move's [global storage](./global-storage-structure.md), and module functions define the rules for updating storage. Modules themselves are also stored in global storage. Scripts are executable entrypoints similar to a `main` function in a conventional language. A script typically calls functions of a published module that perform updates to global storage. Scripts are ephemeral code snippets that are not published in global storage.

A Move source file (or **compilation unit**) may contain multiple modules and scripts. However, publishing a module or executing a script are separate VM operations.

Move有两种不同类型的程序: ***Modules***和 ***Scripts***。模块(Modules, 相当于智能合约，译者注)是定义结构类型以及对这些类型进行操作的函数的库。结构类型定义Move的[全局存储](./global-storage-structure.md)的模式，模块函数定义更新存储的规则。模块本身也存储在全局存储中。脚本(Scripts)是可执行的入口点，类似于传统语言中的主函数 `main`。脚本通常调用已发布模块的函数来更新全局存储。Scripts是暂时的代码片段，没有发布到全局存储中。

一个Move源文件(或**编译单元**)可能包含多个模块和脚本。然而，发布模块或执行脚本都是独立的VM操作。

## 语法(Syntax)

### 脚本(Scripts)

A script has the following structure:

script具有以下结构:

```text
script {
    <use>*
    <constants>*
    fun <identifier><[type parameters: constraint]*>([identifier: type]*) <function_body>
}
```

A `script` block must start with all of its [use](./uses.md) declarations, followed by any [constants](./constants.md) and (finally) the main [function](./functions.md) declaration. The main function can have any name (i.e., it need not be called `main`), is the only function in a script block, can have any number of arguments, and must not return a value. Here is an example with each of these components:

一个 `script` 块必须在开头声明[use](./uses.md)，然后是[constants](./constants.md)的内容,最后声明主函数 [function](./functions.md)。主函数的名称可以是任意的(也就是说，它不一定命名为 `main`)，是script block中唯一的函数，可以有任意数量的参数，并且不能有返回值。下面是示例:

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

脚本(Scripts) 的功能非常有限—它们不能声明友元、结构类型或访问全局存储，他们的主要作用主要是调用模块函数。

### 模块(Modules)

Module 具有以下结构:

```text
module <address>::<identifier> {
    (<use> | <friend> | <type> | <function> | <constant>)*
}
```

where `<address>` is a valid [named or literal address](./address.md).

其中 `<address>` 是一个有效的 [命名或字面量地址](./address.md).

例子:

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

The `module 0x42::Test` part specifies that the module `Test` will be published under the [account address](./chapter_5_address.md) `0x42` in [global storage](./global-storage-structure.md).

`module 0x42::Test` 这部分指定模块 `Test` 会被发布到[全局存储](./global-storage-structure.md)中[账户地址](./address.md)为 `0x42` 之下。

Modules can also be declared using [named addresses](./address.md). For example:

模块也可以用[命名地址](./address.md) 来声明，例如：

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

Because named addresses only exist at the source language level and during compilation, named addresses will be fully substituted for their value at the bytecode level. For example if we had the following code:

因为命名地址只存在于源码级别，并且在编译期间，命名地址会被转换成字节码。例如，如果我们有下面的代码:

```move=
script {
    fun example() {
        my_addr::m::foo(@my_addr);
    }
}
```

and we compiled it with `my_addr` set to `0xC0FFEE`, then it would be equivalent to the following operationally:

我们会将 `my_addr` 编译为`0xC0FFEE`，将和下面的代码是等价的：

```move=
script {
    fun example() {
        0xC0FFEE::m::foo(@0xC0FFEE);
    }
}
```

However at the source level, these *are not equivalent*—the function `M::foo` *must* be accessed through the `MyAddr` named address, and not through the numerical value assigned to that address.

但是在源码级别，这两个*并不等价* —— 函数 `M::foo` 必须通过 `MyAddr`命名地址访问，而不是通过分配给该地址的数值访问。

Module names can start with letters `a` to `z` or letters `A` to `Z`. After the first character, module names can contain underscores `_`, letters `a` to `z`, letters `A` to `Z`, or digits `0` to `9`.

模块名称可以以字母 `a` 到 `z` 或字母 `A` 到 `Z`开头。在第一个字符之后，模块名可以包含下划线 `_`、字母 `a` 到 `z` 、字母 `A` 到 `Z` 或数字 `0` 到 `9`。

```move
module my_module {}
module foo_bar_42 {}
```

Typically, module names start with an uppercase letter. A module named `my_module` should be stored in a source file named `my_module.move`.

通常，模块名称以大写字母开头。名为 `my_module` 的模块应该存储在名为 `my_module.move` 的源文件中。

All elements inside a `module` block can appear in any order. Fundamentally, a module is a collection of [`types`](./structs-and-resources.md) and [`functions`](./functions.md). [Uses](./uses.md) import types from other modules. [Friends](./friends.md) specify a list of trusted modules. [Constants](./constants.md) define private constants that can be used in the functions of a module.

`module` 块中的所有元素都可以以任何顺序出现。从根本上说，模块是[`types`](./structs-and-resources.md)和[`functions`](./functions.md)的集合。[Uses](./uses.md)从其他模块导入类型。[Friends](./friends.md)指定一个可信模块列表。[Constants](./constants.md)定义可以在模块函数中使用的私有常量。
