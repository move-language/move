# 地址（Address）

`address` is a built-in type in Move that is used to represent locations (sometimes called accounts) in global storage. An `address` value is a 128-bit (16 byte) identifier. At a given address, two things can be stored: [Modules](./modules-and-scripts.md) and [Resources](./structs-and-resources.md).

Although an `address` is a 128 bit integer under the hood, Move addresses are intentionally opaque---they cannot be created from integers, they do not support arithmetic operations, and they cannot be modified. Even though there might be interesting programs that would use such a feature (e.g., pointer arithmetic in C fills a similar niche), Move does not allow this dynamic behavior because it has been designed from the ground up to support static verification.

You can use runtime address values (values of type `address`) to access resources at that address. You *cannot* access modules at runtime via address values.

`地址（address）`是 Move 中的内置类型，用于表示全局存储中的的位置（有时称为账户）。`地址（address）` 值是一个 128 位（16 字节）标识符。在一个给定的地址，可以存储两样东西：[模块（Module）](./modules-and-scripts.md)和[资源（Resources）](./structs-and-resources.md)。

虽然`地址（address）`在底层是一个 128 位整数，但 Move 语言有意让其不透明 —— 它们不能从整数创建，不支持算术运算，也不能修改。即使可能有一些有趣的程序会使用这种特性（例如，C 中的指针算法实现了类似壁龛（niche）的功能），但 Move 语言不允许这种动态行为，因为它从头开始就被设计为支持静态验证。*（壁龛指安装在墙壁上的小格子或在墙身上留出的作为贮藏设施的空间，最早在宗教上是指排放佛像的小空间，现在多用在家庭装修上，因其不占建筑面积，使用比较方便，深受大家喜爱，Joe 注）*

你可以通过运行时地址值（`address` 类型的值）来访问该地址处的资源。但*无法*在运行时通过地址值访问模块。

## 地址及其语法（Addresses and Their Syntax）

Addresses come in two flavors, named or numerical. The syntax for a named address follows the
same rules for any named identifier in Move. The syntax of a numerical address is not restricted
to hex-encoded values, and any valid [`u128` numerical value](./integers.md) can be used as an
address value, e.g., `42`, `0xCAFE`, and `2021` are all valid numerical address
literals.

地址有两种形式：*命名的*或*数值的*。命名地址的语法遵循 Move 命名标识符的规则。数值地址的语法不受十六进制编码值的限制，任何有效的 [`u128` 数值](./integers.md)都可以用作地址值。例如，`42`，`0xCFAE` 和 `2021` 都是合法有效的数值地址字面量（literal）。

To distinguish when an address is being used in an expression context or not, the
syntax when using an address differs depending on the context where it's used:
* When an address is used as an expression the address must be prefixed by the `@` character, i.e., [`@<numerical_value>`](./integers.md) or `@<named_address_identifier>`.
* Outside of expression contexts, the address may be written without the leading `@` character, i.e., [`<numerical_value>`](./integers.md) or `<named_address_identifier>`.


为了区分何时在表达式上下文中使用地址，使用地址时的语法根据使用地址的上下文而有所不同：

* 当地址被用作表达式时，地址必须以 `@` 字符为前缀，例如：[`@<numerical_value>`](./integers.md) 或 `@<named_address_identifier>`。
* 在表达式上下文之外，地址可以不带前缀字符 `@`。例如：[`<numerical_value>`](./integers.md) 或 `<named_address_identifier>`。

In general, you can think of `@` as an operator that takes an address from being a namespace item to being an expression item.

通常，可以将 `@` 视为将地址从命名空间项变为表达式项的运算符。

## 命名地址（Named Addresses）

Named addresses are a feature that allow identifiers to be used in place of
numerical values in any spot where addresses are used, and not just at the
value level.  Named addresses are declared and bound as top level elements
(outside of modules and scripts) in Move Packages, or passed as arguments
to the Move compiler.

命名地址是一项特性，它允许在使用地址的任何地方使用标识符代替数值，而不仅仅是在值级别。命名地址被声明并绑定为 Move 包中的顶级元素（模块和脚本之外）或作为参数传递给 Move 编译器。

Named addresses only exist at the source language level and will be fully
substituted for their value at the bytecode level. Because of this, modules
and module members _must_ be accessed through the module's named address
and not through the numerical value assigned to the named address during
compilation, e.g., `use my_addr::foo` is _not_ equivalent to `use 0x2::foo`
even if the Move program is compiled with `my_addr` set to `0x2`. This
distinction is discussed in more detail in the section on [Modules and
Scripts](./modules-and-scripts.md).

命名地址仅存在于源语言级别，并将在字节码级别完全替代它们的值。因此，模块和模块成员*必须*通过模块的命名地址而不是编译期间分配给命名地址的数值来访问，例如：`use my_addr::foo` *不等于* `use 0x2::foo`，即使 Move 程序编译时将 `my_addr` 设置成 `0x2`。这个区别在[模块和脚本](./modules-and-scripts.md)一节中有更详细的讨论。

### 例子（Examples）

```move
let a1: address = @0x1; // 0x00000000000000000000000000000001 的缩写
let a2: address = @0x42; // 0x00000000000000000000000000000042 的缩写
let a3: address = @0xDEADBEEF; // 0x000000000000000000000000DEADBEEF 的缩写
let a4: address = @0x0000000000000000000000000000000A;
let a5: address = @std; // 将命名地址 `std` 的值赋给 `a5`
let a6: address = @66;
let a7: address = @0x42;

module 66::some_module {   // 不在表达式上下文中，所以不需要 @
    use 0x1::other_module; // 不在表达式上下文中，所以不需要 @
    use std::vector;       // 使用其他模块时，可以使用命名地址作为命名空间项
    ...
}

module std::other_module {  // 可以使用命名地址作为命名空间项来声明模块
    ...
}
```

## 全局存储操作（Global Storage Operations）

The primary purpose of `address` values are to interact with the global storage operations.

`address` values are used with the `exists`, `borrow_global`, `borrow_global_mut`, and `move_from` [operations](./global-storage-operators.md).

The only global storage operation that *does not* use `address` is `move_to`, which uses [`signer`](./signer.md).

`address` 值主要用来与全局存储操作进行交互。

`address` 值与 `exists`、`borrow_global`、`borrow_global_mut` 和 `move_from` [操作（operation）](./global-storage-operators.md)一起使用。

唯一*不使用* `address` 的全局存储操作是 `move_to`，它使用了 [`signer`](./signer.md)。

## 所有权（Ownership）

As with the other scalar values built-in to the language, `address` values are implicitly copyable, meaning they can be copied without an explicit instruction such as [`copy`](./variables.md#move-and-copy).

与 Move 语言内置的其他标量值一样，`address` 值是隐式可复制的，这意味着它们可以在没有显式指令（例如 [`copy`](./variables.md#移动和复制move-and-copy)）的情况下复制。
