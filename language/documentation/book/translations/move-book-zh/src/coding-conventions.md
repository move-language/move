# Move Coding Conventions

This section lays out some basic coding conventions for Move that the Move team has found helpful. These are only recommendations, and you should feel free to use other formatting guidelines and conventions if you have a preference for them.

# 移动编码约定
本部分列出了 Move 团队认为有用的一些基本编码约定。这些只是建议，如果您有偏好，可以随意使用其他格式指南和约定。

## Naming

- **Module names**: should be lower snake case, e.g., `fixed_point32`, `vector`
- **Type names**: should be camel case if they are not a native type, e.g., `Coin`, `RoleId`
- **Function names**: should be lower snake case, e.g., `destroy_empty`
- **Constant names**: should be upper snake case, e.g., `REQUIRES_CAPABILITY`
- Generic types should be descriptive, or anti-descriptive where appropriate, e.g., `T` or `Element` for the Vector generic type parameter. Most of the time the "main" type in a module should be the same name as the module e.g., `option::Option`, `fixed_point32::FixedPoint32`.
- **Module file names**: should be the same as the module name e.g., `Option.move`
- **Script file names**: should be lower snake case and should match the name of the “main” function in the script.
- **Mixed file names**: If the file contains multiple modules and/or scripts, the file name should be lower_snake_case, where the name does not match any particular module/script inside.

## 命名

- 模块名称：应该是小写蛇形，例如，fixed_point32、vector
- 类型名称：如果它们不是本地类型，则应为驼峰式，例如 Coin、RoleId
- 函数名：应该是小写的蛇形，例如，destroy_empty
- 常量名称：应该是大写的蛇形，例如，REQUIRES_CAPABILITY
- 泛型类型应该是描述性的，或者在适当的情况下是反描述性的，例如 Vector 泛型类型参数的 T 或 Element。大多数情况下，模块中的“主”类型应该与模块名称相同，例如 option::Option、fixed_point32::FixedPoint32。
- 模块文件名：应与模块名称相同，例如 Option.move
- 脚本文件名：应该是小写的蛇，并且应该与脚本中“main”函数的名称相匹配。
- 混合文件名：如果文件包含多个模块和/或脚本，则文件名应为 lower_snake_case，其中名称与内部的任何特定模块/脚本不匹配。

## Imports

- All module `use` statements should be at the top of the module.
- Functions should be imported and used fully qualified from the module in which they are declared, and not imported at the top level.
- Types should be imported at the top-level. Where there are name clashes, `as` should be used to rename the type locally as appropriate.

For example, if there is a module

## 导入

所有模块使用语句都应该在模块的顶部。
函数应该从声明它们的模块完全限定地导入和使用，而不是在顶层导入。
类型应该在顶层导入。在存在名称冲突的情况下，应该使用 as 在本地适当地重命名类型。
例如，如果有一个模块

```move=
module 0x1::foo {
    struct Foo { }
    const CONST_FOO: u64 = 0;
    public fun do_foo(): Foo { Foo{} }
    ...
}
```

this would be imported and used as:

这将被导入并用作：

```move=
module 0x1::bar {
    use 0x1::foo::{Self, Foo};

    public fun do_bar(x: u64): Foo {
        if (x == 10) {
            foo::do_foo()
        } else {
            abort 0
        }
    }
    ...
}
```

And, if there is a local name-clash when importing two modules:

并且，如果在导入两个模块时存在本地名称冲突：

```move=
module other_foo {
    struct Foo {}
    ...
}

module 0x1::importer {
    use 0x1::other_foo::Foo as OtherFoo;
    use 0x1::foo::Foo;
....
}
```

## Comments

- Each module, struct, and public function declaration should be commented
- Move has doc comments `///`, regular single-line comments `//`, block comments `/* */`, and block doc comments `/** */`

## 注释

- 应注释每个模块、结构和公共函数声明
- Move 具有文档注释 `///`、常规单行注释 `//`、块注释 `/* */` 和块文档注释 `/** */`

## Formatting

The Move team plans to write an autoformatter to enforce formatting conventions. However, in the meantime:

- Four space indentation should be used except for `script` and `address` blocks whose contents should not be indented
- Lines should be broken if they are longer than 100 characters
- Structs and constants should be declared before all functions in a module

## 格式化

Move 团队计划编写一个自动格式化程序来强制执行格式化约定。然而，与此同时：

- 除了内容不应缩进的脚本和地址块外，应使用四个空格缩进
- 超过 100 个字符的行应换行
- 结构和常量应该在模块中的所有函数之前声明
