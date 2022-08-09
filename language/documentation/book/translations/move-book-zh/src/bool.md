# Bool

`bool` is Move's primitive type for boolean `true` and `false` values.
# 布尔
bool 是 Move 的布尔真假值的原始类型。

## Literals

Literals for `bool` are either `true` or `false`.
## 字面量
bool 的文字为真或假。

## Operations

### Logical

`bool` supports three logical operations:
## 操作符
### 逻辑的
bool 支持三种逻辑运算：

| Syntax                    | Description                  | Equivalent Expression                                               |
| ------------------------- | ---------------------------- | ------------------------------------------------------------------- |
| `&&`                      | short-circuiting logical and | `p && q` is equivalent to `if (p) q else false`                     |
| <code>&vert;&vert;</code> | short-circuiting logical or  | <code>p &vert;&vert; q</code> is equivalent to `if (p) true else q` |
| `!`                       | logical negation             | `!p` is equivalent to `if (p) false else true`                      |

### Control Flow

`bool` values are used in several of Move's control-flow constructs:
### 控制流
布尔值用于 Move 的多个控制流结构中：

- [`if (bool) { ... }`](./conditionals.md)
- [`while (bool) { .. }`](./loops.md)
- [`assert!(bool, u64)`](./abort-and-assert.md)

## Ownership

As with the other scalar values built-in to the language, boolean values are implicitly copyable,
meaning they can be copied without an explicit instruction such as
[`copy`](./variables.md#move-and-copy).
## 所有权
与语言内置的其他标量值一样，布尔值是隐式可复制的，这意味着它们可以在没有显式指令（如复制）的情况下复制。
