# 模块和脚本（Modules and Scripts）

Move has two different types of programs: ***Modules*** and ***Scripts***. Modules are libraries that define struct types along with functions that operate on these types. Struct types define the schema of Move's [global storage](./global-storage-structure.md), and module functions define the rules for updating storage. Modules themselves are also stored in global storage. Scripts are executable entrypoints similar to a `main` function in a conventional language. A script typically calls functions of a published module that perform updates to global storage. Scripts are ephemeral code snippets that are not published in global storage.

A Move source file (or **compilation unit**) may contain multiple modules and scripts. However, publishing a module or executing a script are separate VM operations.

Move 有两种不同类型的程序：***模块（Module）***和***脚本（Script）***。模块是定义结构类型以及对这些类型进行操作的函数的库。*结构类型*定义了 Move 的[全局存储](./global-storage-structure.md)的模式，*模块函数*定义了更新存储的规则。模块本身也存储在全局存储中。脚本是[可执行文件](https://en.wikipedia.org/wiki/Executable)的入口点，类似于传统语言中的主函数 `main`。脚本通常调用已发布模块的函数来更新全局存储。脚本是临时代码片段，不会发布在全局存储中。

一个 Move 源文件（或**编译单元**）可能包含多个模块和脚本。然而，发布模块或执行脚本都是独立的虚拟机（VM）操作。

## 语法（Syntax）

### 脚本（Scripts）

A script has the following structure:

脚本具有以下结构：

```text
script {
    <use>*
    <constants>*
    fun <identifier><[type parameters: constraint]*>([identifier: type]*) <function_body>
}
```

A `script` block must start with all of its [`use`](./uses.md) declarations, followed by any [constants](./constants.md) and (finally) the main [function](./functions.md) declaration. The main function can have any name (i.e., it need not be called `main`), is the only function in a script block, can have any number of arguments, and must not return a value. Here is an example with each of these components:

一个 `script` 块必须以它的所有 [`use`](./uses.md) 声明开头，然后是[常量（constant）](./constants.md)声明，最后是主[函数](./functions.md)声明。主函数的名称可以是任意的（也就是说，它不一定命名为 `main`），它是脚本块中唯一的函数，可以有任意数量的参数，并且不能有返回值。下面是每个组件的示例：

```move
script {
    // 导入在命名账户地址 std 上发布的 debug 模块。
    use std::debug;

    const ONE: u64 = 1;

    fun main(x: u64) {
        let sum = x + ONE;
        debug::print(&sum)
    }
}
```

Scripts have very limited power—they cannot declare friends, struct types or access global storage. Their primary purpose is to invoke module functions.

脚本（Script）的功能非常有限 —— 它们不能声明友元（friend）、结构类型或访问全局存储。他们的主要作用主要是调用*模块函数*。

### 模块（Modules）

A module has the following syntax:

模块具有以下结构：

```text
module <address>::<identifier> {
    (<use> | <friend> | <type> | <function> | <constant>)*
}
```

where `<address>` is a valid [named or literal address](./address.md).

其中 `<address>` 是一个有效的[命名或字面量地址](./address.md)。

例子：

```move
module 0x42::test {
    struct Example has copy, drop { i: u64 }

    use std::debug;
    friend 0x42::another_test;

    const ONE: u64 = 1;

    public fun print(x: u64) {
        let sum = x + ONE;
        let example = Example { i: sum };
        debug::print(&sum)
    }
}
```

The `module 0x42::test` part specifies that the module `test` will be published under the [account address](./address.md) `0x42` in [global storage](./global-storage-structure.md).

Modules can also be declared using [named addresses](./address.md). For example:

`module 0x42::test` 这部分指定模块 `test` 将在[全局存储](./global-storage-structure.md)的[账户地址](./address.md) `0x42` 下发布。

模块也可以使用[命名地址](./address.md)来声明，例如：

```move
module test_addr::test {
    struct Example has copy, drop { a: address }

    use std::debug;
    friend test_addr::another_test;

    public fun print() {
        let example = Example { a: @test_addr };
        debug::print(&example)
    }
}
```

Because named addresses only exist at the source language level and during compilation, named addresses will be fully substituted for their value at the bytecode level. For example if we had the following code:

因为命名地址只存在于源语言级别和编译期间，所以命名地址将在字节码级别彻底替换它们的值。例如，如果我们有以下代码：

```move
script {
    fun example() {
        my_addr::m::foo(@my_addr);
    }
}
```

and we compiled it with `my_addr` set to `0xC0FFEE`, then it would be equivalent to the following operationally:

我们在把 `my_addr` 设置为 `0xC0FFEE` 的情况下编译它，那么它在操作上等同于以下内容：

```move
script {
    fun example() {
        0xC0FFEE::m::foo(@0xC0FFEE);
    }
}
```

However at the source level, these *are not equivalent*—the function `M::foo` *must* be accessed through the `MyAddr` named address, and not through the numerical value assigned to that address.

Module names can start with letters `a` to `z` or letters `A` to `Z`. After the first character, module names can contain underscores `_`, letters `a` to `z`, letters `A` to `Z`, or digits `0` to `9`.

然而，在源代码级别，这些是*不等价的* —— 函数 `m::foo` *必须*通过 `my_addr` 命名地址来访问，而不是通过分配给该地址的数值来访问。

模块名称可以以字母 `a` 到 `z` 或字母 `A` 到 `Z` 开头。在第一个字符之后，模块名可以包含下划线 `_`、字母 `a` 到 `z`、字母 `A` 到 `Z` 或数字 `0` 到 `9`。

```move
module my_module {}
module foo_bar_42 {}
```

Typically, module names start with an uppercase letter. A module named `my_module` should be stored in a source file named `my_module.move`.

All elements inside a `module` block can appear in any order. Fundamentally, a module is a collection of [`types`](./structs-and-resources.md) and [`functions`](./functions.md). The [`use`](./uses.md) keyword is used to import types from other modules. The [`friend`](./friends.md) keyword specifies a list of trusted modules. The [`const`](./constants.md) keyword defines private constants that can be used in the functions of a module.

通常，模块名称以小写字母开头。名为 `my_module` 的模块应该存储在名为 `my_module.move` 的源文件中。

`module` 块内的所有元素都可以按任意顺序出现。从根本上说，模块是[`类型（type）`](./structs-and-resources.md)和[`函数（function）`](./functions.md)的集合。[`use`](./uses.md) 关键字用来从其他模块导入类型。[`friend`](./friends.md) 关键字指定一个可信的模块列表。[`const`](./constants.md) 关键字定义了可以在模块函数中使用的私有常量。
