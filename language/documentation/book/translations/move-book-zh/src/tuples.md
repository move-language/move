# 元组和单值

Move does not fully support tuples as one might expect coming from another language with them as a
[first-class value](https://en.wikipedia.org/wiki/First-class_citizen). However, in order to support multiple return values, Move has tuple-like
expressions. These expressions do not result in a concrete value at runtime (there are no tuples in
the bytecode), and as a result they are very limited: they can only appear in expressions (usually
in the return position for a function); they cannot be bound to local variables; they cannot be
stored in structs; and tuple types cannot be used to instantiate generics.

Move 不完全支持元组，因为人们可能期望像来自另一种语言的元组一样将它们作为[头等值（即头等公民）](https://zh.wikipedia.org/wiki/%E9%A0%AD%E7%AD%89%E7%89%A9%E4%BB%B6)。但是，为了支持多个返回值，Move 具有类似元组的表达式。这些表达式在运行时不会产生具体的值（字节码中没有元组），因此它们非常有限：它们只能出现在表达式中（通常在函数的返回位置）；它们不能绑定到局部变量；它们不能存储在结构中；元组类型不能用于实例化泛型。

Similarly, [unit `()`](https://en.wikipedia.org/wiki/Unit_type) is a type created by the Move source language in order to be expression based.
The unit value `()` does not result in any runtime value. We can consider unit`()` to be an empty
tuple, and any restrictions that apply to tuples also apply to unit.

类似地，[单值（unit）`()`](https://zh.wikipedia.org/wiki/%E5%8D%95%E5%80%BC%E7%B1%BB%E5%9E%8B) 是 Move 源语言创建的一种以表达式为基础的类型。单值 `()` 不会产生任何运行时值。我们可以认为单值 `()` 是一个空元组，适用于元组的任何限制也适用于单值。

It might feel weird to have tuples in the language at all given these restrictions. But one of the
most common use cases for tuples in other languages is for functions to allow functions to return
multiple values. Some languages work around this by forcing the users to write structs that contain
the multiple return values. However in Move, you cannot put references inside of
[structs](./structs-and-resources.md). This required Move to support multiple return values. These
multiple return values are all pushed on the stack at the bytecode level. At the source level, these
multiple return values are represented using tuples.

考虑到这些限制，在语言中使用元组可能会感觉很奇怪。但其他语言中，元组最常见的用例之一是函数 —— 允许函数返回多个值。一些语言通过强制用户编写包含多个返回值的结构来解决这个问题。但是在 Move 中，您不能将引用放在[结构体](./structs-and-resources.md)中。这需要 Move 支持多个返回值。这些多个返回值都在字节码级别被压入到堆栈中。在源码级别，这些多个返回值使用元组表示。

## 字面量

Tuples are created by a comma separated list of expressions inside of parentheses.

元组（tuple）是由括号内以逗号分隔的表达式列表创建的。

| Syntax          | Type                                                                         | Description                                                  |
| --------------- | ---------------------------------------------------------------------------- | ------------------------------------------------------------ |
| `()`            | `(): ()`                                                                     | Unit, the empty tuple, or the tuple of arity 0               |
| `(e1, ..., en)` | `(e1, ..., en): (T1, ..., Tn)` where `e_i: Ti` s.t. `0 < i <= n` and `n > 0` | A `n`-tuple, a tuple of arity `n`, a tuple with `n` elements |

| 语法            | 类型                                                                        | 描述                                    |
|-----------------|-----------------------------------------------------------------------------|-----------------------------------------|
| `()`            | `(): ()`                                                                    | 单值、空元组或 0 元素元组               |
| `(e1, ..., en)` | `(e1, ..., en): (T1, ..., Tn)` 其中 `e_i: Ti` 满足 `0 < i <= n` and `n > 0` | n 元组、n 元素元组、带有 n 个元素的元组 |

Note that `(e)` does not have type `(e): (t)`, in other words there is no tuple with one element. If
there is only a single element inside of the parentheses, the parentheses are only used for
disambiguation and do not carry any other special meaning.

注意 `(e)` 没有类型 `(e): (t)`，换句话说，没有一个元素的元组。如果括号内只有一个元素，则括号仅用于消除歧义，不带有任何其他特殊含义。

Sometimes, tuples with two elements are called "pairs" and tuples with three elements are called
"triples."

有时，具有两个元素的元组称为“二元组（pairs）”，而具有三个元素的元组称为“三元组（triples）”。

### 例子

```move
address 0x42 {
module example {
    // 这三个函数都是等价的

    // 当没有提供返回类型时，假定为 `()`
    fun returns_unit_1() { }

    // 空表达式块中存在隐式 () 值
    fun returns_unit_2(): () { }

    // `returns_unit_1` 和 `returns_unit_2` 的显式版本
    fun returns_unit_3(): () { () }


    fun returns_3_values(): (u64, bool, address) {
        (0, false, @0x42)
    }
    fun returns_4_values(x: &u64): (&u64, u8, u128, vector<u8>) {
        (x, 0, 1, b"foobar")
    }
}
}
```

## 操作

The only operation that can be done on tuples currently is destructuring.

目前唯一可以对元组执行的操作是解构（destructuring）。

### 解构

For tuples of any size, they can be destructured in either a `let` binding or in an assignment.

对于任何大小的元组，它们可以在 `let` 绑定或赋值中被解构。

For example:

例如：

```move
address 0x42 {
module example {
    // 这三个函数都是等价的
    fun returns_unit() {}
    fun returns_2_values(): (bool, bool) { (true, false) }
    fun returns_4_values(x: &u64): (&u64, u8, u128, vector<u8>) { (x, 0, 1, b"foobar") }

    fun examples(cond: bool) {
        let () = ();
        let (x, y): (u8, u64) = (0, 1);
        let (a, b, c, d) = (@0x0, 0, false, b"");

        () = ();
        (x, y) = if (cond) (1, 2) else (3, 4);
        (a, b, c, d) = (@0x1, 1, true, b"1");
    }

    fun examples_with_function_calls() {
        let () = returns_unit();
        let (x, y): (bool, bool) = returns_2_values();
        let (a, b, c, d) = returns_4_values(&0);

        () = returns_unit();
        (x, y) = returns_2_values();
        (a, b, c, d) = returns_4_values(&1);
    }
}
}
```

For more details, see [Move Variables](./variables.md).

有关更多详细信息，请参阅 [Move 变量](./variables.md)。

## 子类型

Along with references, tuples are the only types that have [subtyping](https://en.wikipedia.org/wiki/Subtyping) in Move. Tuples do have
subtyping only in the sense that subtype with references (in a covariant way).

除了引用，元组是唯一在 Move 中具有[子类型（subtyping）](https://zh.wikipedia.org/wiki/%E5%AD%90%E7%B1%BB%E5%9E%8B)的类型。元组只有在具有引用的子类型（以协变方式）的意义上才具有子类型。

Example:

例如:

```move
let x: &u64 = &0;
let y: &mut u64 = &mut 1;

// (&u64, &mut u64) 是 (&u64, &u64) 的子类型
// 因为 &mut u64 是 &u64 的子类型
let (a, b): (&u64, &u64) = (x, y);

// (&mut u64, &mut u64) 是 (&u64, &u64) 的子类型
// 因为 &mut u64 是 &u64 的子类型
let (c, d): (&u64, &u64) = (y, y);

// 错误！(&u64, &mut u64) 不是 (&mut u64, &mut u64) 的子类型
// 因为 &u64 不是 &mut u64 的子类型
let (e, f): (&mut u64, &mut u64) = (x, y);
```

## 所有权

As mentioned above, tuple values don't really exist at runtime. And currently they cannot be stored
into local variables because of this (but it is likely that this feature will come soon). As such,
tuples can only be moved currently, as copying them would require putting them into a local variable
first.

如上所述，元组值在运行时并不真正存在。由于这个原因，目前它们不能存储到局部变量中（但这个功能很可能很快就会出现）。因此，元组目前只能移动，因为复制它们需要先将它们放入局部变量中。
