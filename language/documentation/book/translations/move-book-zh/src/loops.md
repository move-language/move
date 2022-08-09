# While and Loop

Move offers two constructs for looping: `while` and `loop`.
# While 和循环
Move 提供了两种循环结构：while 和 loop。

## `while` loops

The `while` construct repeats the body (an expression of type unit) until the condition (an expression of type `bool`) evaluates to `false`.

Here is an example of simple `while` loop that computes the sum of the numbers from `1` to `n`:
## while 循环
while 构造重复主体（单元类型的表达式），直到条件（布尔类型的表达式）评估为假。

下面是一个简单的 while 循环示例，它计算从 1 到 n 的数字之和：

```move
fun sum(n: u64): u64 {
    let sum = 0;
    let i = 1;
    while (i <= n) {
        sum = sum + i;
        i = i + 1
    };

    sum
}
```

Infinite loops are allowed:
允许无限循环：

```move=
fun foo() {
    while (true) { }
}
```

### `break`

The `break` expression can be used to exit a loop before the condition evaluates to `false`. For example, this loop uses `break` to find the smallest factor of `n` that's greater than 1:

### `break`
break 表达式可用于在条件计算为假之前退出循环。例如，此循环使用 break 来查找 n 中大于 1 的最小因子：

```move
fun smallest_factor(n: u64): u64 {
    // assuming the input is not 0 or 1
    let i = 2;
    while (i <= n) {
        if (n % i == 0) break;
        i = i + 1
    };

    i
}
```

The `break` expression cannot be used outside of a loop.

break 表达式不能在循环外使用。

### `continue`

The `continue` expression skips the rest of the loop and continues to the next iteration. This loop uses `continue` to compute the sum of `1, 2, ..., n`, except when the number is divisible by 10:

### `continue`
continue 表达式跳过循环的其余部分并继续下一次迭代。此循环使用 continue 来计算 1、2、...、n 的总和，除非该数字能被 10 整除：

```move
fun sum_intermediate(n: u64): u64 {
    let sum = 0;
    let i = 0;
    while (i < n) {
        i = i + 1;
        if (i % 10 == 0) continue;
        sum = sum + i;
    };

    sum
}
```

The `continue` expression cannot be used outside of a loop.
continue 表达式不能在循环外使用。

### The type of `break` and `continue`

`break` and `continue`, much like `return` and `abort`, can have any type. The following examples illustrate where this flexible typing can be helpful:

### 中断和继续的类型
break 和 continue 就像 return 和 abort 一样，可以有任何类型。以下示例说明了这种灵活的类型在哪些方面会有所帮助：

```move
fun pop_smallest_while_not_equal(
    v1: vector<u64>,
    v2: vector<u64>,
): vector<u64> {
    let result = vector::empty();
    while (!vector::is_empty(&v1) && !vector::is_empty(&v2)) {
        let u1 = *vector::borrow(&v1, vector::length(&v1) - 1);
        let u2 = *vector::borrow(&v2, vector::length(&v2) - 1);
        let popped =
            if (u1 < u2) vector::pop_back(&mut v1)
            else if (u2 < u1) vector::pop_back(&mut v2)
            else break; // Here, `break` has type `u64`
        vector::push_back(&mut result, popped);
    };

    result
}
```

```move
fun pick(
    indexes: vector<u64>,
    v1: &vector<address>,
    v2: &vector<address>
): vector<address> {
    let len1 = vector::length(v1);
    let len2 = vector::length(v2);
    let result = vector::empty();
    while (!vector::is_empty(&indexes)) {
        let index = vector::pop_back(&mut indexes);
        let chosen_vector =
            if (index < len1) v1
            else if (index < len2) v2
            else continue; // Here, `continue` has type `&vector<address>`
        vector::push_back(&mut result, *vector::borrow(chosen_vector, index))
    };

    result
}
```

## The `loop` expression

The `loop` expression repeats the loop body (an expression with type `()`) until it hits a `break`

Without a `break`, the loop will continue forever
## 循环表达式
循环表达式重复循环体（类型为 () 的表达式），直到遇到中断

没有中断，循环将永远继续

```move
fun foo() {
    let i = 0;
    loop { i = i + 1 }
}
```

Here is an example that uses `loop` to write the `sum` function:

这是一个使用循环编写求和函数的示例：

```move
fun sum(n: u64): u64 {
    let sum = 0;
    let i = 0;
    loop {
        i = i + 1;
        if (i > n) break;
        sum = sum + i
    };

    sum
}
```

As you might expect, `continue` can also be used inside a `loop`. Here is `sum_intermediate` from above rewritten using `loop` instead of `while`

如您所料， continue 也可以在循环内使用。这是上面使用循环而不是 while 重写的 sum_intermediate

```move
fun sum_intermediate(n: u64): u64 {
    let sum = 0;
    let i = 0;
    loop {
        i = i + 1;
        if (i % 10 == 0) continue;
        if (i > n) break;
        sum = sum + i
    };

    sum
}
```

## The type of `while` and `loop`

Move loops are typed expressions. A `while` expression always has type `()`.
## while 和循环的类型
移动循环是类型化的表达式。 while 表达式始终具有 () 类型。

```move
let () = while (i < 10) { i = i + 1 };
```

If a `loop` contains a `break`, the expression has type unit `()`

如果循环包含中断，则表达式的类型为 unit ()

```move
(loop { if (i < 10) i = i + 1 else break }: ());
let () = loop { if (i < 10) i = i + 1 else break };
```

If `loop` does not have a `break`, `loop` can have any type much like `return`, `abort`, `break`, and `continue`.

如果循环包含中断，则表达式的类型为 unit ()

```move
(loop (): u64);
(loop (): address);
(loop (): &vector<vector<u8>>);
```
