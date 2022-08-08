# Functions

Function syntax in Move is shared between module functions and script functions. Functions inside of modules are reusable, whereas script functions are only used once to invoke a transaction.
# 函数
Move 中的函数语法在模块函数和脚本函数之间共享。模块内部的函数是可重用的，而脚本函数仅用于调用事务一次。

## Declaration

Functions are declared with the `fun` keyword followed by the function name, type parameters, parameters, a return type, acquires annotations, and finally the function body.

## 声明
函数用 fun 关键字声明，后跟函数名、类型参数、形参、返回类型、获取注解，最后是函数体。

```text
fun <identifier><[type_parameters: constraint],*>([identifier: type],*): <return_type> <acquires [identifier],*> <function_body>
```

For example

例如

```move
fun foo<T1, T2>(x: u64, y: T1, z: T2): (T2, T1, u64) { (z, y, x) }
```

### Visibility

Module functions, by default, can only be called within the same module. These internal (sometimes called private) functions cannot be called from other modules or from scripts.

### 可见性
默认情况下，模块函数只能在同一个模块内调用。这些内部（有时称为私有）函数不能从其他模块或脚本调用。

```move=
address 0x42 {
module m {
    fun foo(): u64 { 0 }
    fun calls_foo(): u64 { foo() } // valid
}

module other {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' is internal to '0x42::m'
    }
}
}

script {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' is internal to '0x42::m'
    }
}
```

To allow access from other modules or from scripts, the function must be declared `public` or `public(friend)`.
要允许从其他模块或脚本访问，该函数必须声明为 public 或 public(friend)。

#### `public` visibility

A `public` function can be called by *any* function defined in *any* module or script. As shown in the following example, a `public` function can be called by:
- other functions defined in the same module,
- functions defined in another module, or
- the function defined in a script.

#### `public` 可见性
公共函数可以被任何模块或脚本中定义的任何函数调用。如以下示例所示，可以通过以下方式调用公共函数：

在同一模块中定义的其他功能，
在另一个模块中定义的函数，或
脚本中定义的函数。

```move=
address 0x42 {
module m {
    public fun foo(): u64 { 0 }
    fun calls_foo(): u64 { foo() } // valid
}

module other {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // valid
    }
}
}

script {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // valid
    }
}
```

#### `public(friend)` visibility

The `public(friend)` visibility modifier is a more restricted form of the `public` modifier to give more control about where a function can be used. A `public(friend)` function can be called by:
- other functions defined in the same module, or
- functions defined in modules which are explicitly specified in the **friend list** (see [Friends](./friends.md) on how to specify the friend list).

Note that since we cannot declare a script to be a friend of a module, the functions defined in scripts can never call a `public(friend)` function.
#### `public(friend)` 可见性
public(friend) 可见性修饰符是 public 修饰符的一种更受限制的形式，可以更好地控制函数的使用位置。可以通过以下方式调用公共（朋友）函数：

在同一模块中定义的其他功能，或
在好友列表中明确指定的模块中定义的函数（请参阅好友了解如何指定好友列表）。
请注意，由于我们不能将脚本声明为模块的朋友，因此脚本中定义的函数永远不能调用 public(friend) 函数。

```move=
address 0x42 {
module m {
    friend 0x42::n;  // friend declaration
    public(friend) fun foo(): u64 { 0 }
    fun calls_foo(): u64 { foo() } // valid
}

module n {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // valid
    }
}

module other {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' can only be called from a 'friend' of module '0x42::m'
    }
}
}

script {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' can only be called from a 'friend' of module '0x42::m'
    }
}
```

### `entry` modifier

The `entry` modifier is designed to allow module functions to be safely and directly invoked much like scripts. This allows module writers to specify which functions can be to begin execution. The module writer then knows that any non-`entry` function will be called from a Move program already in execution.

Essentially, `entry` functions are the "main" functions of a module, and they specify where Move programs start executing.

Note though, an `entry` function _can_ still be called by other Move functions. So while they _can_ serve as the start of a Move program, they aren't restricted to that case.

For example:
### `entry` 修饰符
entry 修饰符旨在允许像脚本一样安全直接地调用模块函数。这允许模块编写者指定哪些函数可以开始执行。然后，模块编写者知道任何非入口函数都将从已经在执行的 Move 程序中调用。

本质上，入口函数是模块的“主要”函数，它们指定 Move 程序开始执行的位置。

但请注意，其他 Move 函数仍然可以调用入口函数。因此，虽然它们可以作为 Move 程序的开始，但它们并不局限于这种情况。

例如：

```move=
address 0x42 {
module m {
    public entry fun foo(): u64 { 0 }
    fun calls_foo(): u64 { foo() } // valid!
}

module n {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // valid!
    }
}

module other {
    public entry fun calls_m_foo(): u64 {
        0x42::m::foo() // valid!
    }
}
}

script {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // valid!
    }
}
```

Even internal functions can be marked as `entry`! This lets you guarantee that the function is called only at the beginning of execution (assuming you do not call it elsewhere in your module)
甚至内部函数也可以标记为入口！这使您可以保证仅在执行开始时调用该函数（假设您没有在模块的其他地方调用它）

```move=
address 0x42 {
module m {
    entry fun foo(): u64 { 0 } // valid! entry functions do not have to be public
}

module n {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' is internal to '0x42::m'
    }
}

module other {
    public entry fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' is internal to '0x42::m'
    }
}
}

script {
    fun calls_m_foo(): u64 {
        0x42::m::foo() // ERROR!
//      ^^^^^^^^^^^^ 'foo' is internal to '0x42::m'
    }
}
```

### Name

Function names can start with letters `a` to `z` or letters `A` to `Z`. After the first character, function names can contain underscores `_`, letters `a` to `z`, letters `A` to `Z`, or digits `0` to `9`.
### 名称
函数名称可以以字母 a 到 z 或字母 A 到 Z 开头。在第一个字符之后，函数名称可以包含下划线 `_`、字母 a 到 z、字母 A 到 Z 或数字 0 到 9。

```move
fun FOO() {}
fun bar_42() {}
fun _bAZ19() {}
```

### Type Parameters

After the name, functions can have type parameters
### 类型参数
在名称之后，函数可以有类型参数

```move
fun id<T>(x: T): T { x }
fun example<T1: copy, T2>(x: T1, y: T2): (T1, T1, T2) { (copy x, x, y) }
```

For more details, see [Move generics](./generics.md).

有关更多详细信息，请参阅移动泛型。

### Parameters

Functions parameters are declared with a local variable name followed by a type annotation
### 参数
函数参数使用局部变量名声明，后跟类型注释

```move
fun add(x: u64, y: u64): u64 { x + y }
```

We read this as `x` has type `u64`

A function does not have to have any parameters at all.

我们将其读为 x 具有 u64 类型

函数根本不需要任何参数。

```move
fun useless() { }
```

This is very common for functions that create new or empty data structures

这对于创建新数据结构或空数据结构的函数很常见

```move=
address 0x42 {
module example {
  struct Counter { count: u64 }

  fun new_counter(): Counter {
      Counter { count: 0 }
  }

}
}
```

### Acquires

When a function accesses a resource using `move_from`, `borrow_global`, or `borrow_global_mut`, the function must indicate that it `acquires` that resource. This is then used by Move's type system to ensure the references into global storage are safe, specifically that there are no dangling references into global storage.
### 收购
当函数使用 move_from、borrow_global 或 borrow_global_mut 访问资源时，该函数必须表明它获取了该资源。然后 Move 的类型系统使用它来确保对全局存储的引用是安全的，特别是没有对全局存储的悬空引用。

```move=
address 0x42 {
module example {

    struct Balance has key { value: u64 }

    public fun add_balance(s: &signer, value: u64) {
        move_to(s, Balance { value })
    }

    public fun extract_balance(addr: address): u64 acquires Balance {
        let Balance { value } = move_from(addr); // acquires needed
        value
    }
}
}
```

`acquires` annotations must also be added for transitive calls within the module. Calls to these functions from another module do not need to annotated with these acquires because one module cannot access resources declared in another module--so the annotation is not needed to ensure reference safety.

还必须为模块内的传递调用添加获取注释。从另一个模块对这些函数的调用不需要使用这些获取进行注释，因为一个模块无法访问在另一个模块中声明的资源——因此不需要注释来确保引用安全。

```move=
address 0x42 {
module example {

    struct Balance has key { value: u64 }

    public fun add_balance(s: &signer, value: u64) {
        move_to(s, Balance { value })
    }

    public fun extract_balance(addr: address): u64 acquires Balance {
        let Balance { value } = move_from(addr); // acquires needed
        value
    }

    public fun extract_and_add(sender: address, receiver: &signer) acquires Balance {
        let value = extract_balance(sender); // acquires needed here
        add_balance(receiver, value)
    }
}
}

address 0x42 {
module other {
    fun extract_balance(addr: address): u64 {
        0x42::example::extract_balance(addr) // no acquires needed
    }
}
}
```

A function can `acquire` as many resources as it needs to

一个函数可以根据需要获取尽可能多的资源

```move=
address 0x42 {
module example {
    use std::vector;

    struct Balance has key { value: u64 }
    struct Box<T> has key { items: vector<T> }

    public fun store_two<Item1: store, Item2: store>(
        addr: address,
        item1: Item1,
        item2: Item2,
    ) acquires Balance, Box {
        let balance = borrow_global_mut<Balance>(addr); // acquires needed
        balance.value = balance.value - 2;
        let box1 = borrow_global_mut<Box<Item1>>(addr); // acquires needed
        vector::push_back(&mut box1.items, item1);
        let box2 = borrow_global_mut<Box<Item2>>(addr); // acquires needed
        vector::push_back(&mut box2.items, item2);
    }
}
}
```

### Return type

After the parameters, a function specifies its return type.
### 返回类型
在参数之后，函数指定其返回类型。

```move
fun zero(): u64 { 0 }
```

Here `: u64` indicates that the function's return type is `u64`.

Using tuples, a function can return multiple values

这里：u64 表示函数的返回类型是u64。

使用元组，一个函数可以返回多个值

```move
fun one_two_three(): (u64, u64, u64) { (0, 1, 2) }
```

If no return type is specified, the function has an implicit return type of unit `()`. These functions are equivalent

如果未指定返回类型，则该函数具有隐式返回类型 unit ()。这些功能是等价的

```move
fun just_unit(): () { () }
fun just_unit() { () }
fun just_unit() { }
```

`script` functions must have a return type of unit `()`

脚本函数的返回类型必须为 unit ()

```move=
script {
    fun do_nothing() {
    }
}
```

As mentioned in the [tuples section](./tuples.md), these tuple "values" are virtual and do not exist at runtime. So for a function that returns unit `()`, it will not be returning any value at all during execution.

如元组部分所述，这些元组“值”是虚拟的，在运行时不存在。因此，对于返回 unit () 的函数，它在执行期间根本不会返回任何值。

### Function body

A function's body is an expression block. The return value of the function is the last value in the sequence

### 函数体
函数体是一个表达式块。函数的返回值是序列中的最后一个值

```move=
fun example(): u64 {
    let x = 0;
    x = x + 1;
    x // returns 'x'
}
```

See [the section below for more information on returns](#returning-values)

For more information on expression blocks, see [Move variables](./variables.md).

有关退货的更多信息，请参阅以下部分

有关表达式块的更多信息，请参阅移动变量。

### Native Functions

Some functions do not have a body specified, and instead have the body provided by the VM. These functions are marked `native`.

Without modifying the VM source code, a programmer cannot add new native functions. Furthermore, it is the intent that `native` functions are used for either standard library code or for functionality needed for the given Move environment.

Most `native` functions you will likely see are in standard library code such as `vector`
### 原生函数
有些函数没有指定主体，而是由 VM 提供的主体。这些函数被标记为原生。

如果不修改 VM 源代码，程序员就无法添加新的本地函数。此外，本机函数的意图是用于标准库代码或给定 Move 环境所需的功能。

您可能会看到的大多数本机函数都在标准库代码中，例如向量

```move=
module std::vector {
    native public fun empty<Element>(): vector<Element>;
    ...
}
```

## Calling

When calling a function, the name can be specified either through an alias or fully qualified

## 调用
调用函数时，名称可以通过别名或完全限定指定

```move=
address 0x42 {
module example {
    public fun zero(): u64 { 0 }
}
}

script {
    use 0x42::example::{Self, zero};
    fun call_zero() {
        // With the `use` above all of these calls are equivalent
        0x42::example::zero();
        example::zero();
        zero();
    }
}
```

When calling a function, an argument must be given for every parameter.

调用函数时，必须为每个参数指定一个参数。

```move=
address 0x42 {
module example {
    public fun takes_none(): u64 { 0 }
    public fun takes_one(x: u64): u64 { x }
    public fun takes_two(x: u64, y: u64): u64 { x + y }
    public fun takes_three(x: u64, y: u64, z: u64): u64 { x + y + z }
}
}

script {
    use 0x42::example;
    fun call_all() {
        example::takes_none();
        example::takes_one(0);
        example::takes_two(0, 1);
        example::takes_three(0, 1, 2);
    }
}
```

Type arguments can be either specified or inferred. Both calls are equivalent.

可以指定或推断类型参数。两个调用是等价的。

```move=
address 0x42 {
module example {
    public fun id<T>(x: T): T { x }
}
}

script {
    use 0x42::example;
    fun call_all() {
        example::id(0);
        example::id<u64>(0);
    }
}
```

For more details, see [Move generics](./generics.md).

有关更多详细信息，请参阅移动泛型。

## Returning values

The result of a function, its "return value", is the final value of its function body. For example

## 返回值
一个函数的结果，它的“返回值”，是它的函数体的最终值。例如

```move=
fun add(x: u64, y: u64): u64 {
    x + y
}
```

[As mentioned above](#function-body), the function's body is an [expression block](./variables.md). The expression block can sequence various statements, and the final expression in the block will be be the value of that block

如上所述，函数体是一个表达式块。表达式块可以对各种语句进行排序，块中的最终表达式将是该块的值

```move=
fun double_and_add(x: u64, y: u64): u64 {
    let double_x = x * 2;
    let double_y = y * 2;
    double_x + double_y
}
```

The return value here is `double_x + double_y`

这里的返回值为 double_x + double_y

### `return` expression

A function implicitly returns the value that its body evaluates to. However, functions can also use the explicit `return` expression:

### 返回表达式

函数隐式返回其主体计算的值。但是，函数也可以使用显式返回表达式：

```move
fun f1(): u64 { return 0 }
fun f2(): u64 { 0 }
```

These two functions are equivalent. In this slightly more involved example, the function subtracts two `u64` values, but returns early with `0` if the second value is too large:

这两个功能是等价的。在这个稍微复杂的示例中，该函数减去两个 u64 值，但如果第二个值太大，则提前返回 0：

```move=
fun safe_sub(x: u64, y: u64): u64 {
    if (y > x) return 0;
    x - y
}
```

Note that the body of this function could also have been written as `if (y > x) 0 else x - y`.

However `return` really shines is in exiting deep within other control flow constructs. In this example, the function iterates through a vector to find the index of a given value:

请注意，这个函数的主体也可以写成 if (y x) 0 else x - y。

然而 return 真正闪耀的是在其他控制流结构的深处退出。在此示例中，函数遍历向量以查找给定值的索引：

```move=
use std::vector;
use std::option::{Self, Option};
fun index_of<T>(v: &vector<T>, target: &T): Option<u64> {
    let i = 0;
    let n = vector::length(v);
    while (i < n) {
        if (vector::borrow(v, i) == target) return option::some(i);
        i = i + 1
    };

    option::none()
}
```

Using `return` without an argument is shorthand for `return ()`. That is, the following two functions are equivalent:

使用不带参数的 return 是 return () 的简写。即以下两个函数是等价的：

```move
fun foo() { return }
fun foo() { return () }
```
