# Conditionals

An `if` expression specifies that some code should only be evaluated if a certain condition is true. For example:
# 条件句
if 表达式指定仅当某个条件为真时才应评估某些代码。例如：

```move
if (x > 5) x = x - 5
```

The condition must be an expression of type `bool`.

An `if` expression can optionally include an `else` clause to specify another expression to evaluate when the condition is false.

条件必须是布尔类型的表达式。

if 表达式可以选择包含 else 子句，以指定另一个表达式在条件为假时进行评估。

```move
if (y <= 10) y = y + 1 else y = 10
```

Either the "true" branch or the "false" branch will be evaluated, but not both. Either branch can be a single expression or an expression block.

The conditional expressions may produce values so that the `if` expression has a result.
将评估“真”分支或“假”分支，但不会同时评估两者。任何一个分支都可以是单个表达式或表达式块。

条件表达式可以产生值，以便 if 表达式有结果。

```move
let z = if (x < 100) x else 100;
```

The expressions in the true and false branches must have compatible types. For example:

true 和 false 分支中的表达式必须具有兼容的类型。例如：

```move=
// x and y must be u64 integers
let maximum: u64 = if (x > y) x else y;

// ERROR! branches different types
let z = if (maximum < 10) 10u8 else 100u64;

// ERROR! branches different types, as default false-branch is () not u64
if (maximum >= 10) maximum;
```

If the `else` clause is not specified, the false branch defaults to the unit value. The following are equivalent:

如果没有指定 else 子句，则 false 分支默认为单位值。以下是等价的：

```move
if (condition) true_branch // implied default: else ()
if (condition) true_branch else ()
```

Commonly, [`if` expressions](./conditionals.md) are used in conjunction with expression blocks.

通常，if 表达式与表达式块一起使用。

```move
let maximum = if (x > y) x else y;
if (maximum < 10) {
    x = x + 10;
    y = y + 10;
} else if (x >= 10 && y >= 10) {
    x = x - 10;
    y = y - 10;
}
```

## Grammar for Conditionals

> *if-expression* → **if (** *expression* **)** *expression* *else-clause*<sub>*opt*</sub>
> *else-clause* → **else** *expression*

## 条件语法
if 表达式 → if ( 表达式 ) 表达式 else-clauseopt else-clause → else 表达式
