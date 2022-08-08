# Abort and Assert

[`return`](./functions.md) and `abort` are two control flow constructs that end execution, one for
the current function and one for the entire transaction.

More information on [`return` can be found in the linked section](./functions.md)
# 中止和断言
return 和 abort 是结束执行的两种控制流结构，一种用于当前函数，一种用于整个事务。

有关退货的更多信息，请参见链接部分

## `abort`

`abort` is an expression that takes one argument: an **abort code** of type `u64`. For example:
## 中止
abort 是一个带有一个参数的表达式：u64 类型的中止代码。例如：

```move
abort 42
```

The `abort` expression halts execution the current function and reverts all changes made to global
state by the current transaction. There is no mechanism for "catching" or otherwise handling an
`abort`.

Luckily, in Move transactions are all or nothing, meaning any changes to global storage are made all
at once only if the transaction succeeds. Because of this transactional commitment of changes, after
an abort there is no need to worry about backing out changes. While this approach is lacking in
flexibility, it is incredibly simple and predictable.

Similar to [`return`](./functions.md), `abort` is useful for exiting control flow when some
condition cannot be met.

In this example, the function will pop two items off of the vector, but will abort early if the
vector does not have two items

abort 表达式停止执行当前函数并恢复当前事务对全局状态所做的所有更改。没有“捕获”或以其他方式处理中止的机制。

幸运的是，在 Move 中，事务是全有或全无，这意味着只有在事务成功时才会对全局存储进行任何更改。由于更改的这种事务性承诺，在中止之后无需担心撤销更改。虽然这种方法缺乏灵活性，但它非常简单且可预测。

与 return 类似，abort 对于在某些条件无法满足时退出控制流很有用。

在此示例中，该函数将从向量中弹出两个项目，但如果向量没有两个项目，该函数将提前中止

```move=
use std::vector;
fun pop_twice<T>(v: &mut vector<T>): (T, T) {
    if (vector::length(v) < 2) abort 42;

    (vector::pop_back(v), vector::pop_back(v))
}
```

This is even more useful deep inside a control-flow construct. For example, this function checks
that all numbers in the vector are less than the specified `bound`. And aborts otherwise

这在控制流结构的深处甚至更有用。例如，此函数检查向量中的所有数字是否小于指定的界限。否则中止

```move=
use std::vector;
fun check_vec(v: &vector<u64>, bound: u64) {
    let i = 0;
    let n = vector::length(v);
    while (i < n) {
        let cur = *vector::borrow(v, i);
        if (cur > bound) abort 42;
        i = i + 1;
    }
}
```

### `assert`

`assert` is a builtin, macro-like operation provided by the Move compiler. It takes two arguments, a
condition of type `bool` and a code of type `u64`

### 断言
assert 是 Move 编译器提供的内置的类似宏的操作。它有两个参数，一个 bool 类型的条件和一个 u64 类型的代码

```move
assert!(condition: bool, code: u64)
```

Since the operation is a macro, it must be invoked with the `!`. This is to convey that the
arguments to `assert` are call-by-expression. In other words, `assert` is not a normal function and
does not exist at the bytecode level. It is replaced inside the compiler with

由于该操作是一个宏，因此必须使用 ! 调用它。这是为了传达断言的参数是按表达式调用的。换句话说，assert 不是一个普通的函数，在字节码级别是不存在的。它在编译器内部被替换为

```move
if (condition) () else abort code
```

`assert` is more commonly used than just `abort` by itself. The `abort` examples above can be
rewritten using `assert`

assert 比 abort 本身更常用。上面的中止示例可以使用 assert 重写

```move=
use std::vector;
fun pop_twice<T>(v: &mut vector<T>): (T, T) {
    assert!(vector::length(v) >= 2, 42); // Now uses 'assert'

    (vector::pop_back(v), vector::pop_back(v))
}
```

and

```move=
use std::vector;
fun check_vec(v: &vector<u64>, bound: u64) {
    let i = 0;
    let n = vector::length(v);
    while (i < n) {
        let cur = *vector::borrow(v, i);
        assert!(cur <= bound, 42); // Now uses 'assert'
        i = i + 1;
    }
}
```

Note that because the operation is replaced with this `if-else`, the argument for the `code` is not
always evaluated. For example:

请注意，由于该操作被替换为 if-else，因此并不总是评估代码的参数。例如：

```move
assert!(true, 1 / 0)
```

Will not result in an arithmetic error, it is equivalent to

不会导致算术错误，相当于

```move
if (true) () else (1 / 0)
```

So the arithmetic expression is never evaluated!

所以算术表达式永远不会被评估！

### Abort codes in the Move VM

When using `abort`, it is important to understand how the `u64` code will be used by the VM.

Normally, after successful execution, the Move VM produces a change-set for the changes made to
global storage (added/removed resources, updates to existing resources, etc).

If an `abort` is reached, the VM will instead indicate an error. Included in that error will be two
pieces of information:

- The module that produced the abort (address and name)
- The abort code.

For example
### Move VM 中的中止代码
使用 abort 时，了解 VM 将如何使用 u64 代码非常重要。

通常，在成功执行后，Move VM 会为对全局存储所做的更改（添加/删除资源、更新现有资源等）生成一个更改集。

如果达到中止，VM 将改为指示错误。该错误中包含两条信息：

产生中止的模块（地址和名称）
中止代码。
例如

```move=
address 0x2 {
module example {
    public fun aborts() {
        abort 42
    }
}
}

script {
    fun always_aborts() {
        0x2::example::aborts()
    }
}
```

If a transaction, such as the script `always_aborts` above, calls `0x2::example::aborts`, the VM
would produce an error that indicated the module `0x2::example` and the code `42`.

This can be useful for having multiple aborts being grouped together inside a module.

In this example, the module has two separate error codes used in multiple functions

如果事务（例如上面的脚本 always_aborts）调用 0x2::example::aborts，VM 将产生一个错误，指示模块 0x2::example 和代码 42。

这对于在一个模块内将多个中止组合在一起很有用。

在此示例中，模块有两个单独的错误代码，用于多个功能

```move=
address 0x42 {
module example {

    use std::vector;

    const EMPTY_VECTOR: u64 = 0;
    const INDEX_OUT_OF_BOUNDS: u64 = 1;

    // move i to j, move j to k, move k to i
    public fun rotate_three<T>(v: &mut vector<T>, i: u64, j: u64, k: u64) {
        let n = vector::length(v);
        assert!(n > 0, EMPTY_VECTOR);
        assert!(i < n, INDEX_OUT_OF_BOUNDS);
        assert!(j < n, INDEX_OUT_OF_BOUNDS);
        assert!(k < n, INDEX_OUT_OF_BOUNDS);

        vector::swap(v, i, k);
        vector::swap(v, j, k);
    }

    public fun remove_twice<T>(v: &mut vector<T>, i: u64, j: u64): (T, T) {
        let n = vector::length(v);
        assert!(n > 0, EMPTY_VECTOR);
        assert!(i < n, INDEX_OUT_OF_BOUNDS);
        assert!(j < n, INDEX_OUT_OF_BOUNDS);
        assert!(i > j, INDEX_OUT_OF_BOUNDS);

        (vector::remove<T>(v, i), vector::remove<T>(v, j))
    }
}
}
```

## The type of `abort`

The `abort i` expression can have any type! This is because both constructs break from the normal
control flow, so they never need to evaluate to the value of that type.

The following are not useful, but they will type check
## 中止类型
abort i 表达式可以有任何类型！这是因为这两种构造都脱离了正常的控制流，因此它们永远不需要评估该类型的值。

以下没有用，但它们会键入检查

```move
let y: address = abort 0;
```

This behavior can be helpful in situations where you have a branching instruction that produces a
value on some branches, but not all. For example:

在您有一个分支指令在某些分支上产生值的情况下，这种行为可能会有所帮助，但不是全部。例如：

```move
let b =
    if (x == 0) false
    else if (x == 1) true
    else abort 42;
//       ^^^^^^^^ `abort 42` has type `bool`
```
