# Standard Library

The Move standard library exposes interfaces that implement the following functionality:
* [Basic operations on vectors](#vector).
* [Option types and operations on`Option` types](#option).
* [A common error encoding code interface for abort codes](#errors).
* [32-bit precision fixed-point numbers](#fixed_point32).

# 标准库
Move 标准库公开了实现以下功能的接口：

* 向量的基本操作。
* 选项类型和选项类型的操作。
* 中止代码的常见错误编码代码接口。
* 32 位精度定点数。

## vector

The `vector` module defines a number of operations over the primitive
[`vector`](./vector.md) type. The module is published under the
named address `Std` and consists of a number of native functions, as
well as functions defined in Move. The API for this module is as follows.

## 向量
vector 模块定义了对原始向量类型的许多操作。该模块在命名地址 Std 下发布，由许多本机函数以及 Move 中定义的函数组成。该模块的 API 如下。

### Functions
### 函数

---------------------------------------------------------------------------

Create an empty [`vector`](./vector.md).
The `Element` type can be both a `resource` or `copyable` type.

创建一个空向量。 Element 类型既可以是资源类型，也可以是可复制类型。

```move
    native public fun empty<Element>(): vector<Element>;
```

---------------------------------------------------------------------------

Create a vector of length `1` containing the passed in `element`.

创建一个包含传入元素的长度为 1 的向量。

```move
    public fun singleton<Element>(e: Element): vector<Element>;
```

---------------------------------------------------------------------------

Destroy (deallocate) the vector `v`. Will abort if `v` is non-empty.
*Note*: The emptiness restriction is due to the fact that `Element` can be a
resource type, and destruction of a non-empty vector would violate
[resource conservation](./structs-and-resources.md).

销毁（解除分配）向量 v。如果 v 不为空，将中止。注意：空性限制是由于 Element 可以是资源类型，销毁非空向量会违反资源守恒。

```move
    native public fun destroy_empty<Element>(v: vector<Element>);
```

---------------------------------------------------------------------------

Acquire an [immutable reference](./references.md) to the `i`th element of the vector `v`.  Will abort if
the index `i` is out of bounds for the vector `v`.

获取对向量 v 的第 i 个元素的不可变引用。如果索引 i 超出向量 v 的范围，将中止。

```move
    native public fun borrow<Element>(v: &vector<Element>, i: u64): &Element;
```

---------------------------------------------------------------------------

Acquire a [mutable reference](./references.md)
to the `i`th element of the vector `v`.  Will abort if
the index `i` is out of bounds for the vector `v`.

获取对向量 v 的第 i 个元素的可变引用。如果索引 i 超出向量 v 的范围，将中止。

```move
    native public fun borrow_mut<Element>(v: &mut vector<Element>, i: u64): &mut Element;
```

---------------------------------------------------------------------------

Empty and destroy the `other` vector, and push each of the elements in
the `other` vector onto the `lhs` vector in the same order as they occurred in `other`.

清空并销毁另一个向量，并将另一个向量中的每个元素以与它们在其他向量中出现的顺序相同的顺序推送到 lhs 向量上。

```move
    public fun append<Element>(lhs: &mut vector<Element>, other: vector<Element>);
```

---------------------------------------------------------------------------

Push an element `e` of type `Element` onto the end of the vector `v`. May
trigger a resizing of the underlying vector's memory.

将 Element 类型的元素 e 推到向量 v 的末尾。可能会触发底层向量内存的大小调整。

```move
    native public fun push_back<Element>(v: &mut vector<Element>, e: Element);
```

---------------------------------------------------------------------------

Pop an element from the end of the vector `v` in-place and return the owned
value. Will abort if `v` is empty.

从向量 v 的末尾就地弹出一个元素并返回拥有的值。如果 v 为空，将中止。

```move
    native public fun pop_back<Element>(v: &mut vector<Element>): Element;
```

---------------------------------------------------------------------------

Remove the element at index `i` in the vector `v` and return the owned value
that was previously stored at `i` in `v`. All elements occurring at indices
greater than `i` will be shifted down by 1. Will abort if `i` is out of bounds
for `v`.

删除向量 v 中索引 i 处的元素，并返回先前存储在 v 中 i 处的拥有值。所有出现在索引处大于 i 的元素将向下移动 1。如果 i 超出 v 的范围，将中止。

```move
    public fun remove<Element>(v: &mut vector<Element>, i: u64): Element;
```

---------------------------------------------------------------------------

Swap the `i`th element of the vector `v` with the last element and then pop
this element off of the back of the vector and return the owned value that
was previously stored at index `i`.
This operation is O(1), but does not preserve ordering of elements in the vector.
Aborts if the index `i` is out of bounds for the vector `v`.

将向量 v 的第 i 个元素与最后一个元素交换，然后将该元素从向量的背面弹出，并返回之前存储在索引 i 处的拥有值。此操作为 O(1)，但不保留向量中元素的顺序。如果索引 i 超出向量 v 的范围，则中止。

```move
    public fun swap_remove<Element>(v: &mut vector<Element>, i: u64): Element;
```

---------------------------------------------------------------------------

Swap the elements at the `i`'th and `j`'th indices in the vector `v`. Will
abort if either of `i` or `j` are out of bounds for `v`.

交换向量 v 中第 i 个和第 j 个索引处的元素。如果 i 或 j 中的任何一个超出 v 的范围，则将中止。

```move
    native public fun swap<Element>(v: &mut vector<Element>, i: u64, j: u64);
```

---------------------------------------------------------------------------

Reverse the order of the elements in the vector `v` in-place.

就地反转向量 v 中元素的顺序。

```move
    public fun reverse<Element>(v: &mut vector<Element>);
```

---------------------------------------------------------------------------

Return the index of the first occurrence of an element in `v` that is
equal to `e`. Returns `(true, index)` if such an element was found, and
`(false, 0)` otherwise.

返回 v 中等于 e 的元素第一次出现的索引。如果找到这样的元素，则返回 (true, index)，否则返回 (false, 0)。

```move
    public fun index_of<Element>(v: &vector<Element>, e: &Element): (bool, u64);
```

---------------------------------------------------------------------------

Return if an element equal to `e` exists in the vector `v`.

如果向量 v 中存在等于 e 的元素，则返回。

```move
    public fun contains<Element>(v: &vector<Element>, e: &Element): bool;
```

---------------------------------------------------------------------------

Return the length of a `vector`.

返回向量的长度。

```move
    native public fun length<Element>(v: &vector<Element>): u64;
```

---------------------------------------------------------------------------

Return whether the vector `v` is empty.

返回向量 v 是否为空。

```move
    public fun is_empty<Element>(v: &vector<Element>): bool;
```

---------------------------------------------------------------------------

## option

The `option` module defines a generic option type `Option<T>` that represents a
value of type `T` that may, or may not, be present. It is published under the named address `Std`.

The Move option type is internally represented as a singleton vector, and may
contain a value of `resource` or `copyable` kind.  If you are familiar with option
types in other languages, the Move `Option` behaves similarly to those with a
couple notable exceptions since the option can contain a value of kind `resource`.
Particularly, certain operations such as `get_with_default` and
`destroy_with_default` require that the element type `T` be of `copyable` kind.

The API for the `option` module is as as follows

## 选项
选项模块定义了一个通用选项类型 Option T，它代表一个类型 T 的值，该值可能存在，也可能不存在。它以命名地址 Std 发布。

Move 选项类型在内部表示为单例向量，并且可能包含资源或可复制种类的值。如果您熟悉其他语言中的选项类型，则移动选项的行为类似于那些具有几个值得注意的例外的选项，因为该选项可以包含 kind 资源的值。特别是，某些操作，如 get_with_default 和 destroy_with_default 要求元素类型 T 是可复制类型。

选件模块的 API 如下

### Types

Generic type abstraction of a value that may, or may not, be present. Can contain
a value of either `resource` or `copyable` kind.
### 类型
可能存在或不存在的值的通用类型抽象。可以包含资源或可复制类型的值。

```move
    struct Option<T>;
```

### Functions

Create an empty `Option` of that can contain a value of `Element` type.

### 功能
创建一个可以包含元素类型值的空选项。

```move
    public fun none<Element>(): Option<Element>;
```

---------------------------------------------------------------------------

Create a non-empty `Option` type containing a value `e` of type `Element`.

创建一个包含 Element 类型的值 e 的非空 Option 类型。

```move
    public fun some<Element>(e: T): Option<Element>;
```

---------------------------------------------------------------------------

Return an immutable reference to the value inside the option `opt_elem`
Will abort if `opt_elem` does not contain a value.

返回对选项 opt_elem 中值的不可变引用 如果 opt_elem 不包含值，将中止。

```move
    public fun borrow<Element>(opt_elem: &Option<Element>): &Element;
```

---------------------------------------------------------------------------

Return a reference to the value inside `opt_elem` if it contains one. If
`opt_elem` does not contain a value the passed in `default_ref` reference will be returned.
Does not abort.

如果它包含一个，则返回对 opt_elem 内的值的引用。如果 opt_elem 不包含值，则将返回传入的 default_ref 引用。不中止。

```move
    public fun borrow_with_default<Element>(opt_elem: &Option<Element>, default_ref: &Element): &Element;
```

---------------------------------------------------------------------------

Return a mutable reference to the value inside `opt_elem`. Will abort if
`opt_elem` does not contain a value.

返回对 opt_elem 中值的可变引用。如果 opt_elem 不包含值，将中止。

```move
    public fun borrow_mut<Element>(opt_elem: &mut Option<Element>): &mut Element;
```

---------------------------------------------------------------------------

Convert an option value that contains a value to one that is empty in-place by
removing and returning the value stored inside `opt_elem`.
Will abort if `opt_elem` does not contain a value.

通过删除并返回存储在 opt_elem 中的值，将包含值的选项值转换为就地为空的值。如果 opt_elem 不包含值，将中止。

```move
    public fun extract<Element>(opt_elem: &mut Option<Element>): Element;
```

---------------------------------------------------------------------------

Return the value contained inside the option `opt_elem` if it contains one.
Will return the passed in `default` value if `opt_elem` does not contain a
value. The `Element` type that the `Option` type is instantiated with must be
of `copyable` kind in order for this function to be callable.

如果它包含一个，则返回选项 opt_elem 中包含的值。如果 opt_elem 不包含值，将返回传入的默认值。用于实例化 Option 类型的 Element 类型必须是可复制类型，才能使此函数可调用。

```move
    public fun get_with_default<Element: copyable>(opt_elem: &Option<Element>, default: Element): Element;
```

---------------------------------------------------------------------------

Convert an empty option `opt_elem` to an option value that contains the value `e`.
Will abort if `opt_elem` already contains a value.

将空选项 opt_elem 转换为包含值 e 的选项值。如果 opt_elem 已经包含一个值，将中止。

```move
    public fun fill<Element>(opt_elem: &mut Option<Element>, e: Element);
```

---------------------------------------------------------------------------

Swap the value currently contained in `opt_elem` with `new_elem` and return the
previously contained value. Will abort if `opt_elem` does not contain a value.

将 opt_elem 中当前包含的值交换为 new_elem 并返回先前包含的值。如果 opt_elem 不包含值，将中止。

```move
    public fun swap<Element>(opt_elem: &mut Option<Element>, e: Element): Element;
```

---------------------------------------------------------------------------

Return true if `opt_elem` contains a value equal to the value of `e_ref`.
Otherwise, `false` will be returned.

如果 opt_elem 包含的值等于 e_ref 的值，则返回 true。否则，将返回 false。

```move
    public fun contains<Element>(opt_elem: &Option<Element>, e_ref: &Element): bool;
```

---------------------------------------------------------------------------

Return `true` if `opt_elem` does not contain a value.

如果 opt_elem 不包含值，则返回 true。

```move
    public fun is_none<Element>(opt_elem: &Option<Element>): bool;
```

---------------------------------------------------------------------------

Return `true` if `opt_elem` contains a value.

如果 opt_elem 包含一个值，则返回 true。

```move
    public fun is_some<Element>(opt_elem: &Option<Element>): bool;
```

---------------------------------------------------------------------------

Unpack `opt_elem` and return the value that it contained.
Will abort if `opt_elem` does not contain a value.

解包 opt_elem 并返回它包含的值。如果 opt_elem 不包含值，将中止。

```move
    public fun destroy_some<Element>(opt_elem: Option<Element>): Element;
```

---------------------------------------------------------------------------

Destroys the `opt_elem` value passed in. If `opt_elem` contained a value it
will be returned otherwise, the passed in `default` value will be returned.

销毁传入的 opt_elem 值。如果 opt_elem 包含值，则返回，否则返回传入的默认值。

```move
    public fun destroy_with_default<Element: copyable>(opt_elem: Option<Element>, default: Element): Element;
```

---------------------------------------------------------------------------

Destroys the `opt_elem` value passed in, `opt_elem` must be empty and not
contain a value. Will abort if `opt_elem` contains a value.

销毁传入的 opt_elem 值，opt_elem 必须为空且不包含值。如果 opt_elem 包含一个值，将中止。

```move
    public fun destroy_none<Element>(opt_elem: Option<Element>);
```

## errors

Recall that each abort code in Move is represented as an unsigned 64-bit integer. The `errors` module defines a common interface that can be used to "tag" each of these abort codes so that they can represent both the error **category** along with an error **reason**.

Error categories are declared as constants in the `errors` module and are globally unique with respect to this module. Error reasons on the other hand are module-specific error codes, and can provide greater detail (perhaps, even a particular _reason_) about the specific error condition. This representation of a category and reason for each error code is done by dividing the abort code into two sections.

The lower 8 bits of the abort code hold the *error category*. The remaining 56 bits of the abort code hold the *error reason*.
The reason should be a unique number relative to the module which raised the error and can be used to obtain more information about the error at hand. It should mostly be used for diagnostic purposes as error reasons may change over time if the module is updated.
## 错误
回想一下，Move 中的每个中止代码都表示为一个无符号的 64 位整数。 errors 模块定义了一个通用接口，可用于“标记”每个中止代码，以便它们可以表示错误类别和错误原因。

错误类别在错误模块中被声明为常量，并且相对于该模块是全局唯一的。另一方面，错误原因是特定于模块的错误代码，可以提供有关特定错误条件的更多详细信息（甚至可能是特定原因）。每个错误代码的类别和原因的这种表示是通过将中止代码分为两部分来完成的。

中止代码的低 8 位保存错误类别。中止代码的剩余 56 位保存错误原因。原因应该是相对于引发错误的模块的唯一编号，并且可用于获取有关手头错误的更多信息。它应该主要用于诊断目的，因为如果更新模块，错误原因可能会随着时间而改变。

| Category | Reason |
|----------|--------|
| 8 bits   | 56 bits|

Since error categories are globally stable, these present the most stable API and should in general be what is used by clients to determine the messages they may present to users (whereas the reason is useful for diagnostic purposes). There are public functions in the `errors` module for creating an abort code of each error category with a specific `reason` number (represented as a `u64`).

由于错误类别是全局稳定的，因此它们提供了最稳定的 API，通常应该是客户端用来确定它们可能呈现给用户的消息的内容（而原因对于诊断目的很有用）。错误模块中有公共函数，用于为每个错误类别创建一个带有特定原因号的中止代码（表示为 u64）。

### Constants

The system is in a state where the performed operation is not allowed.

### 常数
系统处于不允许执行的操作的状态。

```move
    const INVALID_STATE: u8 = 1;
```

---------------------------------------------------------------------------
A specific account address was required to perform an operation, but a different address from what was expected was encounterd.

执行操作需要特定的帐户地址，但遇到了与预期不同的地址。

```move
    const REQUIRES_ADDRESS: u8 = 2;
```

---------------------------------------------------------------------------
An account did not have the expected  role for this operation. Useful for Role Based Access Control (RBAC) error conditions.

帐户没有此操作的预期角色。对于基于角色的访问控制 (RBAC) 错误情况很有用。

```move
    const REQUIRES_ROLE: u8 = 3;
```

---------------------------------------------------------------------------
An account did not not have a required capability. Useful for RBAC error conditions.

帐户没有所需的功能。对于 RBAC 错误情况很有用。

```move
    const REQUIRES_CAPABILITY: u8 = 4;
```

---------------------------------------------------------------------------
A resource was expected, but did not exist under an address.

应有资源，但地址下不存在。

```move
    const NOT_PUBLISHED: u8 = 5;
```

---------------------------------------------------------------------------
Attempted to publish a resource under an address where one was already published.

尝试在已发布资源的地址下发布资源。

```move
    const ALREADY_PUBLISHED: u8 = 6;
```

---------------------------------------------------------------------------
An argument provided for an operation was invalid.

为操作提供的参数无效。

```move
    const INVALID_ARGUMENT: u8 = 7;
```

---------------------------------------------------------------------------
A limit on a value was exceeded.

超出了某个值的限制。

```move
    const LIMIT_EXCEEDED: u8 = 8;
```

---------------------------------------------------------------------------
An internal error (bug) has occurred.

发生内部错误（错误）。

```move
    const INTERNAL: u8 = 10;
```

---------------------------------------------------------------------------
A custom error category for extension points.

扩展点的自定义错误类别。

```move
    const CUSTOM: u8 = 255;
```

---------------------------------------------------------------------------

### Functions

 Should be used in the case where invalid (global) state is encountered. Constructs an abort code with specified `reason` and category `INVALID_STATE`. Will abort if `reason` does not fit in 56 bits.

### 函数

应该在遇到无效（全局）状态的情况下使用。构造具有指定原因和类别 INVALID_STATE 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun invalid_state(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if an account's address does not match a specific address. Constructs an abort code with specified `reason` and category `REQUIRES_ADDRESS`. Will abort if `reason` does not fit in 56 bits.

如果帐户的地址与特定地址不匹配，则应使用。构造具有指定原因和类别 REQUIRES_ADDRESS 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun requires_address(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if a role did not match a required role when using RBAC. Constructs an abort code with specified `reason` and category `REQUIRES_ROLE`. Will abort if `reason` does not fit in 56 bits.

如果在使用 RBAC 时角色与所需角色不匹配，则应使用该角色。构造具有指定原因和类别 REQUIRES_ROLE 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun requires_role(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if an account did not have a required capability when using RBAC. Constructs an abort code with specified `reason` and category `REQUIRES_CAPABILITY`. Should be Will abort if `reason` does not fit in 56 bits.

如果帐户在使用 RBAC 时没有所需的功能，则应使用。构造具有指定原因和类别 REQUIRES_CAPABILITY 的中止代码。如果原因不适合 56 位，则应该是将中止。

```move
    public fun requires_capability(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if a resource did not exist where one was expected. Constructs an abort code with specified `reason` and category `NOT_PUBLISHED`. Will abort if `reason` does not fit in 56 bits.

如果资源在预期的地方不存在，则应使用该资源。构造具有指定原因和类别 NOT_PUBLISHED 的中止代码。如果原因不适合 56 位，将中止。


```move
    public fun not_published(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if a resource already existed where one was about to be published. Constructs an abort code with specified `reason` and category `ALREADY_PUBLISHED`. Will abort if `reason` does not fit in 56 bits.

如果资源已经存在且即将发布，则应使用该资源。构造一个具有指定原因和类别 ALREADY_PUBLISHED 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun already_published(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if an invalid argument was passed to a function/operation. Constructs an abort code with specified `reason` and category `INVALID_ARGUMENT`. Will abort if `reason` does not fit in 56 bits.

如果将无效参数传递给函数/操作，则应使用。构造具有指定原因和类别 INVALID_ARGUMENT 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun invalid_argument(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if a limit on a specific value is reached, e.g., subtracting 1 from a value of 0. Constructs an abort code with specified `reason` and category `LIMIT_EXCEEDED`. Will abort if `reason` does not fit in 56 bits.

如果达到特定值的限制，则应使用，例如，从 0 中减去 1。构造具有指定原因和类别 LIMIT_EXCEEDED 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun limit_exceeded(reason: u64): u64;
```

---------------------------------------------------------------------------
Should be used if an internal error or bug was encountered. Constructs an abort code with specified `reason` and category `INTERNAL`. Will abort if `reason` does not fit in 56 bits.

如果遇到内部错误或错误，应使用。构造具有指定原因和类别 INTERNAL 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun internal(reason: u64): u64;
```

---------------------------------------------------------------------------
Used for extension points, should be not used under most circumstances. Constructs an abort code with specified `reason` and category `CUSTOM`. Will abort if `reason` does not fit in 56 bits.

用于扩展点，在大多数情况下不应该使用。构造具有指定原因和类别 CUSTOM 的中止代码。如果原因不适合 56 位，将中止。

```move
    public fun custom(reason: u64): u64;
```

---------------------------------------------------------------------------

## fixed_point32


The `fixed_point32` module defines a fixed-point numeric type with 32 integer bits and 32 fractional bits. Internally, this is represented as a `u64` integer wrapped in a struct to make a unique `fixed_point32` type. Since the numeric representation is a binary one, some decimal values may not be exactly representable, but it provides more than 9 decimal digits of precision both before and after the decimal point (18 digits total). For comparison, double precision floating-point has less than 16 decimal digits of precision, so you should be careful about using floating-point to convert these values to decimal.

## 固定点32
fixed_point32 模块定义了一个具有 32 个整数位和 32 个小数位的定点数值类型。在内部，这表示为一个包裹在结构中的 u64 整数，以形成唯一的 fixed_point32 类型。由于数字表示是二进制的，因此某些十进制值可能无法精确表示，但它在小数点前后都提供了超过 9 位的精度（总共 18 位）。作为比较，双精度浮点的精度小于 16 位小数，因此在使用浮点将这些值转换为十进制时应小心。

### Types

Represents a fixed-point numeric number with 32 fractional bits.

### 类型

表示具有 32 个小数位的定点数值。

```move
    struct FixedPoint32;
```

### Functions

Multiply a u64 integer by a fixed-point number, truncating any fractional part of the product. This will abort if the product overflows.

### 函数

将 u64 整数乘以定点数，截断乘积的任何小数部分。如果产品溢出，这将中止。
```move
    public fun multiply_u64(val: u64, multiplier: FixedPoint32): u64;
```

---------------------------------------------------------------------------
Divide a u64 integer by a fixed-point number, truncating any fractional part of the quotient. This will abort if the divisor is zero or if the quotient overflows.

将 u64 整数除以定点数，截断商的任何小数部分。如果除数为零或商溢出，这将中止。

```move
    public fun divide_u64(val: u64, divisor: FixedPoint32): u64;
```

---------------------------------------------------------------------------
Create a fixed-point value from a rational number specified by its numerator and denominator. Calling this function should be preferred for using `fixed_point32::create_from_raw_value` which is also available. This will abort if the denominator is zero. It will also abort if the numerator is nonzero and the ratio is not in the range $2^{-32}\ldots2^{32}-1$. When specifying decimal fractions, be careful about rounding errors: if you round to display $N$ digits after the decimal point, you can use a denominator of $10^N$ to avoid numbers where the very small imprecision in the binary representation could change the rounding, e.g., 0.0125 will round down to 0.012 instead of up to 0.013.

根据分子和分母指定的有理数创建定点值。使用也可用的 fixed_point32::create_from_raw_value 应该首选调用此函数。如果分母为零，这将中止。如果分子不为零并且比率不在 $2
{-32}ldots2
{32}-1$ 范围内，它也会中止。指定小数时，请注意舍入错误：如果四舍五入以显示小数点后的 $N$ 个数字，则可以使用分母 $10
N$ 来避免二进制表示中非常小的不精确性可能会改变四舍五入，例如，0.0125 将向下舍入为 0.012，而不是向上舍入为 0.013。

```move
    public fun create_from_rational(numerator: u64, denominator: u64): FixedPoint32;
```

---------------------------------------------------------------------------
Create a fixedpoint value from a raw `u64` value.

从原始 u64 值创建定点值。

```move
    public fun create_from_raw_value(value: u64): FixedPoint32;
```

---------------------------------------------------------------------------
Returns `true` if the decimal value of `num` is equal to zero.

如果 num 的十进制值等于 0，则返回 true。

```move
    public fun is_zero(num: FixedPoint32): bool;
```

---------------------------------------------------------------------------
Accessor for the raw `u64` value. Other less common operations, such as adding or subtracting `FixedPoint32` values, can be done using the raw values directly.

原始 u64 值的访问器。其他不太常见的操作，例如添加或减去 FixedPoint32 值，可以直接使用原始值完成。

```move
    public fun get_raw_value(num: FixedPoint32): u64;
```

---------------------------------------------------------------------------
