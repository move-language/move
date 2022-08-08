# Integers

Move supports three unsigned integer types: `u8`, `u64`, and `u128`. Values of these types range from 0 to a maximum that depends on the size of the type.
# 整数
Move 支持三种无符号整数类型：u8、u64 和 u128。这些类型的值范围从 0 到最大值，具体取决于类型的大小。

| Type                             | Value Range              |
| -------------------------------- | ------------------------ |
| Unsigned 8-bit integer, `u8`     | 0 to 2<sup>8</sup> - 1   |
| Unsigned 64-bit integer, `u64`   | 0 to 2<sup>64</sup> - 1  |
| Unsigned 128-bit integer, `u128` | 0 to 2<sup>128</sup> - 1 |

## Literals

Literal values for these types are specified either as a sequence of digits (e.g.,`112`) or as hex literals, e.g., `0xFF`. The type of the literal can optionally be added as a suffix, e.g., `112u8`. If the type is not specified, the compiler will try to infer the type from the context where the literal is used. If the type cannot be inferred, it is assumed to be `u64`.

If a literal is too large for its specified (or inferred) size range, an error is reported.
## 字面量
这些类型的文字值指定为数字序列（例如，112）或十六进制文字，例如，0xFF。可以选择将文字的类型添加为后缀，例如 112u8。如果未指定类型，编译器将尝试从使用文字的上下文推断类型。如果无法推断类型，则假定为 u64。

如果文字对于其指定的（或推断的）大小范围来说太大，则会报告错误。

### Examples
### 例子

```move
// literals with explicit annotations;
let explicit_u8 = 1u8;
let explicit_u64 = 2u64;
let explicit_u128 = 3u128;

// literals with simple inference
let simple_u8: u8 = 1;
let simple_u64: u64 = 2;
let simple_u128: u128 = 3;

// literals with more complex inference
let complex_u8 = 1; // inferred: u8
// right hand argument to shift must be u8
let _unused = 10 << complex_u8;

let x: u8 = 0;
let complex_u8 = 2; // inferred: u8
// arguments to `+` must have the same type
let _unused = x + complex_u8;

let complex_u128 = 3; // inferred: u128
// inferred from function argument type
function_that_takes_u128(complex_u128);

// literals can be written in hex
let hex_u8: u8 = 0x1;
let hex_u64: u64 = 0xCAFE;
let hex_u128: u128 = 0xDEADBEEF;
```

## Operations

### Arithmetic

Each of these types supports the same set of checked arithmetic operations. For all of these operations, both arguments (the left and right side operands) *must* be of the same type. If you need to operate over values of different types, you will need to first perform a [cast](#casting). Similarly, if you expect the result of the operation to be too large for the integer type, perform a [cast](#casting) to a larger size before performing the operation.

All arithmetic operations abort instead of behaving in a way that mathematical integers would not (e.g., overflow, underflow, divide-by-zero).
## 运营
### 算术
这些类型中的每一种都支持相同的检查算术运算集。对于所有这些操作，两个参数（左侧和右侧操作数）必须是同一类型。如果您需要对不同类型的值进行操作，则需要首先执行强制转换。同样，如果您预计运算结果对于整数类型来说太大，请在执行运算之前执行转换为更大的大小。

所有算术运算都会中止，而不是以数学整数不会的方式表现（例如，上溢、下溢、被零除）。

| Syntax | Operation | Aborts If
|--------|-----------|-------------------------------------
| `+` |addition | Result is too large for the integer type
| `-` | subtraction | Result is less than zero
| `*` | multiplication | Result is too large for the integer type
| `%` | modular division | The divisor is `0`
| `/` | truncating division | The divisor is `0`


### Bitwise

The integer types support the following bitwise operations that treat each number as a series of individual bits, either 0 or 1, instead of as numerical integer values.

Bitwise operations do not abort.
### 按位
整数类型支持以下按位运算，将每个数字视为一系列单独的位，0 或 1，而不是数字整数值。

按位运算不会中止。

| Syntax | Operation  | Description
|--------|------------|------------
| `&`    | bitwise and| Performs a boolean and for each bit pairwise
| `|`   | bitwise or | Performs a boolean or for each bit pairwise
| `^`    | bitwise xor| Performs a boolean exclusive or for each bit pairwise

### Bit Shifts

Similar to the bitwise operations, each integer type supports bit shifts. But unlike the other operations, the righthand side operand (how many bits to shift by) must *always* be a `u8` and need not match the left side operand (the number you are shifting).

Bit shifts can abort if the number of bits to shift by is greater than or equal to `8`, `64`, or `128` for `u8`, `u64`, and `u128` respectively.
### 位移
与按位运算类似，每种整数类型都支持位移。但与其他操作不同，右侧操作数（要移位多少位）必须始终是 u8 并且不需要匹配左侧操作数（您要移位的数字）。

如果要移位的位数分别大于或等于 u8、u64 和 u128 的 8、64 或 128，则移位可以中止。

| Syntax | Operation  | Aborts if
|--------|------------|----------
|`<<`    | shift left | Number of bits to shift by is greater than the size of the integer type
|`>>`    | shift right| Number of bits to shift by is greater than the size of the integer type

### Comparisons

Integer types are the *only* types in Move that can use the comparison operators. Both arguments need to be of the same type. If you need to compare integers of different types, you will need to [cast](#casting) one of them first.

Comparison operations do not abort.
### 比较
整数类型是 Move 中唯一可以使用比较运算符的类型。两个参数必须是同一类型。如果您需要比较不同类型的整数，则需要先转换其中一个。

比较操作不会中止。

| Syntax | Operation
|--------|-----------
| `<`    | less than
| `>`    | greater than
| `<=`   | less than or equal to
| `>=`   | greater than or equal to

### Equality

Like all types with [`drop`](./abilities.md) in Move, all integer types support the ["equal"](./equality.md) and ["not equal"](./equality.md)  operations. Both arguments need to be of the same type. If you need to compare integers of different types, you will need to [cast](#casting) one of them first.

Equality operations do not abort.
### 平等
与 Move 中的所有类型一样，所有整数类型都支持“等于”和“不等于”操作。两个参数必须是同一类型。如果您需要比较不同类型的整数，则需要先转换其中一个。

平等操作不会中止。

| Syntax | Operation
|--------|----------
| `==`   | equal
| `!=`   | not equal

For more details see the section on [equality](./equality.md)

有关更多详细信息，请参阅平等部分

## Casting

Integer types of one size can be cast to integer types of another size. Integers are the only types in Move that support casting.

Casts *do not* truncate. Casting will abort if the result is too large for the specified type
## 铸件
一种大小的整数类型可以转换为另一种大小的整数类型。整数是 Move 中唯一支持强制转换的类型。

强制转换不会截断。如果结果对于指定类型来说太大，则转换将中止

| Syntax     | Operation                                                                       | Aborts if
|------------|---------------------------------------------------------------------------------|---------------------------------------
| `(e as T)`| Cast integer expression `e` into an integer type `T` | `e` is too large to represent as a `T`

Here, the type of `e` must be `u8`, `u64`, or `u128` and `T` must be `u8`, `u64`, or `u128`.

For example:

这里，e 的类型必须是 u8、u64 或 u128，T 必须是 u8、u64 或 u128。

例如：

- `(x as u8)`
- `(2u8 as u64)`
- `(1 + 3 as u128)`

## Ownership

As with the other scalar values built-in to the language, integer values are implicitly copyable, meaning they can be copied without an explicit instruction such as [`copy`](./variables.md#move-and-copy).
## 所有权
与语言内置的其他标量值一样，整数值是隐式可复制的，这意味着它们可以在没有显式指令（如复制）的情况下复制。
