# Equality

Move supports two equality operations `==` and `!=`
# 平等
Move 支持两个相等操作 == 和 !=

## Operations
## 操作符

| Syntax | Operation | Description                                                                 |
| ------ | --------- | --------------------------------------------------------------------------- |
| `==`   | equal     | Returns `true` if the two operands have the same value, `false` otherwise   |
| `!=`   | not equal | Returns `true` if the two operands have different values, `false` otherwise |

### Typing

Both the equal (`==`) and not-equal (`!=`) operations only work if both operands are the same type
### 打字
相等 (==) 和不相等 (!=) 操作仅在两个操作数为相同类型时才有效

```move
0 == 0; // `true`
1u128 == 2u128; // `false`
b"hello" != x"00"; // `true`
```

Equality and non-equality also work over user defined types!

相等和不相等也适用于用户定义的类型！

```move=
address 0x42 {
module example {
    struct S has copy, drop { f: u64, s: vector<u8> }

    fun always_true(): bool {
        let s = S { f: 0, s: b"" };
        // parens are not needed but added for clarity in this example
        (copy s) == s
    }

    fun always_false(): bool {
        let s = S { f: 0, s: b"" };
        // parens are not needed but added for clarity in this example
        (copy s) != s
    }
}
}
```

If the operands have different types, there is a type checking error

如果操作数具有不同的类型，则存在类型检查错误

```move
1u8 == 1u128; // ERROR!
//     ^^^^^ expected an argument of type 'u8'
b"" != 0; // ERROR!
//     ^ expected an argument of type 'vector<u8>'
```

### Typing with references

When comparing [references](./references.md), the type of the reference (immutable or mutable) does
not matter. This means that you can compare an immutable `&` reference with a mutable one `&mut` of
the same underlying type.
### 使用参考打字
比较引用时，引用的类型（不可变或可变）无关紧要。这意味着您可以将不可变的 & 引用与相同基础类型的可变 &mut 进行比较。

```move
let i = &0;
let m = &mut 1;

i == m; // `false`
m == i; // `false`
m == m; // `true`
i == i; // `true`
```

The above is equivalent to applying an explicit freeze to each mutable reference where needed

以上相当于在需要时对每个可变引用应用显式冻结

```move
let i = &0;
let m = &mut 1;

i == freeze(m); // `false`
freeze(m) == i; // `false`
m == m; // `true`
i == i; // `true`
```

But again, the underlying type must be the same type

但同样，基础类型必须是相同的类型

```move
let i = &0;
let s = &b"";

i == s; // ERROR!
//   ^ expected an argument of type '&u64'
```

## Restrictions

Both `==` and `!=` consume the value when comparing them. As a result, the type system enforces that
the type must have [`drop`](./abilities.md). Recall that without the
[`drop` ability](./abilities.md), ownership must be transferred by the end of the function, and such
values can only be explicitly destroyed within their declaring module. If these were used directly
with either equality `==` or non-equality `!=`, the value would be destroyed which would break
[`drop` ability](./abilities.md) safety guarantees!

## 限制
== 和 != 在比较它们时都会消耗值。结果，类型系统强制该类型必须具有 drop。回想一下，如果没有 drop 能力，所有权必须在函数结束时转移，并且这些值只能在其声明模块中显式销毁。如果这些直接与相等 == 或不相等 != 一起使用，则该值将被破坏，这将破坏掉落能力的安全保证！

```move=
address 0x42 {
module example {
    struct Coin has store { value: u64 }
    fun invalid(c1: Coin, c2: Coin) {
        c1 == c2 // ERROR!
//      ^^    ^^ These resources would be destroyed!
    }
}
}
```

But, a programmer can _always_ borrow the value first instead of directly comparing the value, and
reference types have the [`drop` ability](./abilities.md). For example

但是，程序员总是可以先借值而不是直接比较值，并且引用类型具有删除能力。例如

```move=
address 0x42 {
module example {
    struct Coin as store { value: u64 }
    fun swap_if_equal(c1: Coin, c2: Coin): (Coin, Coin) {
        let are_equal = &c1 == &c2; // valid
        if (are_equal) (c2, c1) else (c1, c2)
    }
}
}
```

## Avoid Extra Copies

While a programmer _can_ compare any value whose type has [`drop`](./abilities.md), a programmer
should often compare by reference to avoid expensive copies.
## 避免额外的副本
虽然程序员可以比较任何类型下降的值，但程序员应该经常通过引用进行比较以避免昂贵的副本。

```move=
let v1: vector<u8> = function_that_returns_vector();
let v2: vector<u8> = function_that_returns_vector();
assert!(copy v1 == copy v2, 42);
//     ^^^^       ^^^^
use_two_vectors(v1, v2);

let s1: Foo = function_that_returns_large_struct();
let s2: Foo = function_that_returns_large_struct();
assert!(copy s1 == copy s2, 42);
//     ^^^^       ^^^^
use_two_foos(s1, s2);
```

This code is perfectly acceptable (assuming `Foo` has [`drop`](./abilities.md)), just not efficient.
The highlighted copies can be removed and replaced with borrows

这段代码是完全可以接受的（假设 Foo 已经下降），只是效率不高。突出显示的副本可以删除并替换为借用

```move=
let v1: vector<u8> = function_that_returns_vector();
let v2: vector<u8> = function_that_returns_vector();
assert!(&v1 == &v2, 42);
//     ^      ^
use_two_vectors(v1, v2);

let s1: Foo = function_that_returns_large_struct();
let s2: Foo = function_that_returns_large_struct();
assert!(&s1 == &s2, 42);
//     ^      ^
use_two_foos(s1, s2);
```

The efficiency of the `==` itself remains the same, but the `copy`s are removed and thus the program
is more efficient.

== 本身的效率保持不变，但副本被删除，因此程序效率更高。
