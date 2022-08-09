# Vector

`vector<T>` is the only primitive collection type provided by Move. A `vector<T>` is a homogenous
collection of `T`'s that can grow or shrink by pushing/popping values off the "end".

A `vector<T>` can be instantiated with any type `T`. For example, `vector<u64>`, `vector<address>`,
`vector<0x42::MyModule::MyResource>`, and `vector<vector<u8>>` are all valid vector types.
# 向量
矢量 T 是 Move 提供的唯一原始集合类型。向量 T 是 T 的同质集合，可以通过从“末端”推/弹出值来增长或缩小。

向量 T 可以用任何类型 T 实例化。例如，向量 u64 、向量地址 、向量 0x42::MyModule::MyResource 和向量向量 u8 都是有效的向量类型。

## Literals

### General `vector` Literals

Vectors of any type can be created with `vector` literals.
## 字面量
### 一般向量字面量
任何类型的向量都可以用向量字面量创建。

| Syntax                | Type                                                                          | Description                                |
| --------------------- | ----------------------------------------------------------------------------- | ------------------------------------------ |
| `vector[]`            | `vector[]: vector<T>` where `T` is any single, non-reference type             | An empty vector                            |
| `vector[e1, ..., en]` | `vector[e1, ..., en]: vector<T>` where `e_i: T` s.t. `0 < i <= n` and `n > 0` | A vector with `n` elements (of length `n`) |

In these cases, the type of the `vector` is inferred, either from the element type or from the
vector's usage. If the type cannot be inferred, or simply for added clarity, the type can be
specified explicitly:
在这些情况下，向量的类型是根据元素类型或向量的使用来推断的。如果无法推断类型，或者只是为了更清楚起见，可以显式指定类型：

```move
vector<T>[]: vector<T>
vector<T>[e1, ..., en]: vector<T>
```

#### Example Vector Literals
#### 示例向量文字

```move
(vector[]: vector<bool>);
(vector[0u8, 1u8, 2u8]: vector<u8>);
(vector<u128>[]: vector<u128>);
(vector<address>[@0x42, @0x100]: vector<address>);
```

### `vector<u8>` literals

A common use-case for vectors in Move is to represent "byte arrays", which are represented with
`vector<u8>`. These values are often used for cryptographic purposes, such as a public key or a hash
result. These values are so common that specific syntax is provided to make the values more
readable, as opposed to having to use `vector[]` where each individual `u8` value is specified in
numeric form.

There are currently two supported types of `vector<u8>` literals, byte strings and hex strings.
### 矢量 u8 字面量
Move 中向量的一个常见用例是表示“字节数组”，用向量 u8 表示。这些值通常用于加密目的，例如公钥或哈希结果。这些值非常常见，以至于提供了特定的语法以使值更具可读性，而不是必须使用 vector[] ，其中每个单独的 u8 值都以数字形式指定。

目前有两种受支持的向量 u8 文字类型，字节字符串和十六进制字符串。

#### Byte Strings

Byte strings are quoted string literals prefixed by a `b`, e.g. `b"Hello!\n"`.

These are ASCII encoded strings that allow for escape sequences. Currently, the supported escape
sequences are
#### 字节串
字节字符串是以 a b 为前缀的带引号的字符串文字，例如b“你好！”。

这些是允许转义序列的 ASCII 编码字符串。目前，支持的转义序列是

| Escape Sequence | Description                                    |
| --------------- | ---------------------------------------------- |
| `\n`            | New line (or Line feed)                        |
| `\r`            | Carriage return                                |
| `\t`            | Tab                                            |
| `\\`            | Backslash                                      |
| `\0`            | Null                                           |
| `\"`            | Quote                                          |
| `\xHH`          | Hex escape, inserts the hex byte sequence `HH` |

#### Hex Strings

Hex strings are quoted string literals prefixed by a `x`, e.g. `x"48656C6C6F210A"`

Each byte pair, ranging from `00` to `FF`, is interpreted as hex encoded `u8` value. So each byte
pair corresponds to a single entry in the resulting `vector<u8>`
#### 十六进制字符串
十六进制字符串是以 x 为前缀的带引号的字符串文字，例如x'48656C6C6F210A'

每个字节对，范围从 00 到 FF，都被解释为十六进制编码的 u8 值。所以每个字节对对应于结果向量 u8 中的一个条目

#### Example String Literals

```move
script {
fun byte_and_hex_strings() {
    assert!(b"" == x"", 0);
    assert!(b"Hello!\n" == x"48656C6C6F210A", 1);
    assert!(b"\x48\x65\x6C\x6C\x6F\x21\x0A" == x"48656C6C6F210A", 2);
    assert!(
        b"\"Hello\tworld!\"\n \r \\Null=\0" ==
            x"2248656C6C6F09776F726C6421220A200D205C4E756C6C3D00",
        3
    );
}
}
```

## Operations

`vector` supports the following operations via the `std::vector` module in the Move standard
library:
## 操作符
vector 通过 Move 标准库中的 std::vector 模块支持以下操作：

| Function                                                   | Description                                                   | Aborts?                 |
| ---------------------------------------------------------- | ------------------------------------------------------------- | ----------------------- |
| `vector::empty<T>(): vector<T>`                            | Create an empty vector that can store values of type `T`      | Never                   |
| `vector::singleton<T>(t: T): vector<T>`                    | Create a vector of size 1 containing `t`                      | Never                   |
| `vector::push_back<T>(v: &mut vector<T>, t: T)`            | Add `t` to the end of `v`                                     | Never                   |
| `vector::pop_back<T>(v: &mut vector<T>): T`                | Remove and return the last element in `v`                     | If `v` is empty         |
| `vector::borrow<T>(v: &vector<T>, i: u64): &T`             | Return an immutable reference to the `T` at index `i`         | If `i` is not in bounds |
| `vector::borrow_mut<T>(v: &mut vector<T>, i: u64): &mut T` | Return an mutable reference to the `T` at index `i`           | If `i` is not in bounds |
| `vector::destroy_empty<T>(v: vector<T>)`                   | Delete `v`                                                    | If `v` is not empty     |
| `vector::append<T>(v1: &mut vector<T>, v2: vector<T>)`     | Add the elements in `v2` to the end of `v1`                   | If `i` is not in bounds |
| `vector::contains<T>(v: &vector<T>, e: &T): bool`          | Return true if `e` is in the vector `v`                       | Never                   |
| `vector::swap<T>(v: &mut vector<T>, i: u64, j: u64)` | Swaps the elements at the `i`th and `j`th indices in the vector `v`.| If `i` or `j` is out of bounds |
| `vector::reverse<T>(v: &mut vector<T>)`                    | Reverses the order of the elements in the vector `v` in place | Never                   |
| `vector::index_of<T>(v: &vector<T>, e: &T): bool` | Return `(true, i)` if `e` is in the vector `v` at index `i`. Otherwise, returns `(false, 0)`.| Never |
| `vector::remove<T>(v: &mut vector<T>, i: u64): T` | Remove the `i`th element of the vector `v`, shifting all subsequent elements. This is O(n) and preserves ordering of elements in the vector. | If `i` is out of bounds. |
| `vector::swap_remove<T>(v: &mut vector<T>, i: u64): T` | Swap the `i`th element of the vector `v` with the last element and then pop the vector, This is O(1), but does not preserve ordering of elements in the vector. | If `i` is out of bounds. |

More operations may be added overtime

更多操作可能会加班

## Example
## 例子

```move
use std::vector;

let v = vector::empty<u64>();
vector::push_back(&mut v, 5);
vector::push_back(&mut v, 6);

assert!(*vector::borrow(&v, 0) == 5, 42);
assert!(*vector::borrow(&v, 1) == 6, 42);
assert!(vector::pop_back(&mut v) == 6, 42);
assert!(vector::pop_back(&mut v) == 5, 42);
```

## Destroying and copying `vector`s

Some behaviors of `vector<T>` depend on the abilities of the element type, `T`. For example, vectors
containing elements that do not have `drop` cannot be implicitly discarded like `v` in the example
above--they must be explicitly destroyed with `vector::destroy_empty`.

Note that `vector::destroy_empty` will abort at runtime unless `vec` contains zero elements:
## 销毁和复制向量
向量 T 的某些行为取决于元素类型 T 的能力。例如，包含不具有 drop 的元素的向量不能像上例中的 v 那样被隐式丢弃——它们必须用 vector::destroy_empty 显式销毁。

请注意，除非 vec 包含零个元素，否则 vector::destroy_empty 将在运行时中止：

```move
fun destroy_any_vector<T>(vec: vector<T>) {
    vector::destroy_empty(vec) // deleting this line will cause a compiler error
}
```

But no error would occur for dropping a vector that contains elements with `drop`:

但是删除包含带有 drop 的元素的向量不会发生错误：

```move
fun destroy_droppable_vector<T: drop>(vec: vector<T>) {
    // valid!
    // nothing needs to be done explicitly to destroy the vector
}
```

Similarly, vectors cannot be copied unless the element type has `copy`. In other words, a
`vector<T>` has `copy` if and only if `T` has `copy`. However, even copyable vectors are never
implicitly copied:

同样，除非元素类型具有副本，否则无法复制向量。换句话说，一个向量 T 有副本当且仅当 T 有副本。然而，即使是可复制的向量也永远不会被隐式复制：

```move
let x = vector::singleton<u64>(10);
let y = copy x; // compiler error without the copy!
```

Copies of large vectors can be expensive, so the compiler requires explicit `copy`'s to make it
easier to see where they are happening.

For more details see the sections on [type abilities](./abilities.md) and [generics](./generics.md).

大向量的副本可能很昂贵，因此编译器需要显式副本以便更容易查看它们发生的位置。

有关更多详细信息，请参阅类型能力和泛型部分。

## Ownership

As mentioned [above](#destroying-and-copying-vectors), `vector` values can be copied only if the
elements can be copied. In that case, the copy must be explicit via a
[`copy`](./variables.md#move-and-copy) or a [dereference `*`](./references.md#reference-operators).
## 所有权
如上所述，只有可以复制元素，才能复制向量值。在这种情况下，副本必须通过副本或取消引用 * 显式。
