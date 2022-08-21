# 数组 (Vector)

`vector<T>` is the only primitive collection type provided by Move. A `vector<T>` is a homogenous
collection of `T`'s that can grow or shrink by pushing/popping values off the "end".

A `vector<T>` can be instantiated with any type `T`. For example, `vector<u64>`, `vector<address>`,
`vector<0x42::MyModule::MyResource>`, and `vector<vector<u8>>` are all valid vector types.

`vector<T>` 是Move提供的仅有的基本集合类型。`vector<T>` 是类型为 `T` 的同构集合，可以通过从"末端"推入/弹出值来增长或缩小。

`vector<T>` 可以用任何类型 `T` 实例化。 例如, `vector<u64>`, `vector<address>`, `vector<0x42::MyModuel::MyResource>`, 和 `vector<vector<u8>>` 都是有效的数组类型。

## 字面值 (Literals)

### 通用 `vector` 字面值 (General `vector` Literals)

Vectors of any type can be created with `vector` literals.

| Syntax                | Type                                                                          | Description                                |
| --------------------- | ----------------------------------------------------------------------------- | ------------------------------------------ |
| `vector[]`            | `vector[]: vector<T>` where `T` is any single, non-reference type             | An empty vector                            |
| `vector[e1, ..., en]` | `vector[e1, ..., en]: vector<T>` where `e_i: T` s.t. `0 < i <= n` and `n > 0` | A vector with `n` elements (of length `n`) |

任何类型的数组都可以通过`vector`字面值创建。

| 语法                | 类型                                                                          | 描述                                |
| --------------------- | ----------------------------------------------------------------------------- | ------------------------------------------ |
| `vector[]`            | `vector[]: vector<T>` 其中`T`是任何单一的非引用类型              | 一个空数组                            |
| `vector[e1, ..., en]` | `vector[e1, ..., en]: vector<T>` where `e_i: T` 满足 `0 < i <= n` and `n > 0` | 一个具有`n`个元素的数组(长度是`n`) |

In these cases, the type of the `vector` is inferred, either from the element type or from the
vector's usage. If the type cannot be inferred, or simply for added clarity, the type can be
specified explicitly:

在这些情况下，`vector` 的类型是从元素类型或从向量的使用上推断出来的。 如果无法推断类型或者只是为了更清楚起见，类型可以明确指定：

```move
vector<T>[]: vector<T>
vector<T>[e1, ..., en]: vector<T>
```

#### 数组字面值(Literals)举例

```move
(vector[]: vector<bool>);
(vector[0u8, 1u8, 2u8]: vector<u8>);
(vector<u128>[]: vector<u128>);
(vector<address>[@0x42, @0x100]: vector<address>);
```

### `vector<u8>` 字面值 (`vector<u8>` literals)

A common use-case for vectors in Move is to represent "byte arrays", which are represented with
`vector<u8>`. These values are often used for cryptographic purposes, such as a public key or a hash
result. These values are so common that specific syntax is provided to make the values more
readable, as opposed to having to use `vector[]` where each individual `u8` value is specified in
numeric form.

There are currently two supported types of `vector<u8>` literals, byte strings and hex strings.

Move中数组的一个常见用例是表示"字节数组", 用 `vector<u8>` 表示。这些值通常用于加密目的，例如公钥或哈希结果。这些值非常常见，以至于提供了特定的语法以使值更具可读性，而不是必须使用 `vector[]` ，其中每个单独的 `u8` 值都以数字形式指定。

当前支持两种类型的 `vector<u8>` 字面量，字节字符串和十六进制字符串。

#### 字节字符串 (Byte Strings)

Byte strings are quoted string literals prefixed by a `b`, e.g. `b"Hello!\n"`.

These are ASCII encoded strings that allow for escape sequences. Currently, the supported escape
sequences are

字节字符串是带引号的字符串字面量，以 `b` 为前缀，例如 `b"Hello!\n"`.

这些是允许转义序列的ASCII编码字符串。 目前，支持的转义序列如下：

| Escape Sequence | Description                                    |
| --------------- | ---------------------------------------------- |
| `\n`            | 换行                                            |
| `\r`            | 回车                                            |
| `\t`            | 制表符                                          |
| `\\`            | 反斜杠                                          |
| `\0`            | 零                                             |
| `\"`            | 引号                                            |
| `\xHH`          | 十六进制进制转义, 插入十六进制字节序列 `HH`                 |

#### 十六进制字符串 (Hex Strings)

Hex strings are quoted string literals prefixed by a `x`, e.g. `x"48656C6C6F210A"`

Each byte pair, ranging from `00` to `FF`, is interpreted as hex encoded `u8` value. So each byte
pair corresponds to a single entry in the resulting `vector<u8>`

十六进制字符串是带引号的字符串字面值(literals)，前缀为 `x`, 例如：`x"48656C6C6F210A"`

每个字节对，范围从 `00` 到 `FF` 被解析(interpreted)为16进制编码的 `u8` 值。所以每个字节对对应于结果 `vector<u8>` 的单个条目。

#### 字符串字面值(Literals)举例

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

## 操作 (Operations)

`vector` supports the following operations via the `std::vector` module in the Move standard
library:

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

`vector` 通过Move标准库里的 `std::vector` 模块支持以下操作：

| 函数                                                       | 描述                                                   | 中止条件                 |
| ---------------------------------------------------------- | ------------------------------------------------------------- | ----------------------- |
| `vector::empty<T>(): vector<T>`                            | 创建一个可以存储`T`类型值的空数组                    | 永不中止                   |
| `vector::singleton<T>(t: T): vector<T>`                    | 创建一个包含`t`的大小为1的数组                      | 永不中止                   |
| `vector::push_back<T>(v: &mut vector<T>, t: T)`            | 将`t`添加到`v`的尾部                                | 永不中止r                   |
| `vector::pop_back<T>(v: &mut vector<T>): T`                | 移除并返回`v`中的最后一个元素                        | 如果`v`是空数组          |
| `vector::borrow<T>(v: &vector<T>, i: u64): &T`             | 返回在索引`i`处对`T`的不可变引用                     | 如果`i` 越界|
| `vector::borrow_mut<T>(v: &mut vector<T>, i: u64): &mut T` | 返回在索引`i`处对`T`的可变引用                       | 如果`i`越界|
| `vector::destroy_empty<T>(v: vector<T>)`                   | 销毁 `v`数组                               | 如果`v` 不是空数组     |
| `vector::append<T>(v1: &mut vector<T>, v2: vector<T>)`     | 将`v2`中的元素添加到`v1`的末尾                       | 永不中止    |
| `vector::contains<T>(v: &vector<T>, e: &T): bool`          | 如果`e`在数组`v`里返回true                          | 永不中止     |
| `vector::swap<T>(v: &mut vector<T>, i: u64, j: u64)`       | 交换数组`v`中第`i`个和第`j`个索引处的元素             | 如果`i`或`j`越界|
| `vector::reverse<T>(v: &mut vector<T>)`                    | 就地反转数组`v`中元素的顺序                          | 永不中止     |
| `vector::index_of<T>(v: &vector<T>, e: &T): bool`          | 如果`e`在索引`i`处的数组中，则返回`(true, i)`否则返回`(false, 0)` | 永不中止 |
| `vector::remove<T>(v: &mut vector<T>, i: u64): T`          | 移除数组`v`中的第`i`个元素, 移动所有后续元素。这里是O(n)时间复杂度且保留了数组中元素的顺序。| 如果 `i` 越界 |
| `vector::swap_remove<T>(v: &mut vector<T>, i: u64): T`     | 将数组中的第`i`个元素与最后一个元素交换, 然后弹出。这里是O(1)时间复杂度，但是不保留数组中的元素顺序。| 如果`i`越界 |

随着时间的推移可能会增加更多操作。

## 示例

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

## 销毁和复制 `vector`

Some behaviors of `vector<T>` depend on the abilities of the element type, `T`. For example, vectors
containing elements that do not have `drop` cannot be implicitly discarded like `v` in the example
above--they must be explicitly destroyed with `vector::destroy_empty`.

Note that `vector::destroy_empty` will abort at runtime unless `vec` contains zero elements:

`vector<T>` 的某些行为取决于其元素类型`T`的能力(abilities)，例如：数组中包含没有 `drop` 能力的元素不能像前面列子中的 `v` 一样隐式丢弃--它们必须用 `vector::destroy_empty` 销毁。

请注意：除非数组 `vec` 包含零个元素(空数组)，否则 `vector::destroy_empty` 将在运行时中止。

```move
fun destroy_any_vector<T>(vec: vector<T>) {
    vector::destroy_empty(vec) // deleting this line will cause a compiler error
}
```

但是删除包含带有 `drop` 能力(abilities)元素的数组不会发生错误。

```move
fun destroy_droppable_vector<T: drop>(vec: vector<T>) {
    // valid!
    // nothing needs to be done explicitly to destroy the vector
}
```
Similarly, vectors cannot be copied unless the element type has `copy`. In other words, a
`vector<T>` has `copy` if and only if `T` has `copy`. However, even copyable vectors are never
implicitly copied:

同样，除非元素类型具有 `copy` 能力(abilities), 否则无法复制数组。换句话说，当且仅当 `T` 具有 `copy` 能力(abilities)时，vector<T>才具有 `copy` 能力(abilities)。然而，即使是可复制的数组也不会被隐式复制:

```move
let x = vector::singleton<u64>(10);
let y = copy x; // compiler error without the copy!
```

Copies of large vectors can be expensive, so the compiler requires explicit `copy`'s to make it
easier to see where they are happening.

For more details see the sections on [type abilities](./abilities.md) and [generics](./generics.md).

拷贝大的数组可能很昂贵，因此编译器需要显示的`copy`使它更容易看到他们发生在哪。

有关更多详细信息，请参阅[type abilities](./abilities.md)和[generics](./generics.md)

## 所有权(Ownership)

如上所述，数组值只有在元素值可以复制的时候才能复制。 在这种情况下，拷贝必须通过显式[`copy`](./variables.md#move-and-copy)或者解引用[dereference `*`](./references.md#引用运算符-reference-operators)。
