# Local Variables and Scope

Local variables in Move are lexically (statically) scoped. New variables are introduced with the
keyword `let`, which will shadow any previous local with the same name. Locals are mutable and can
be updated both directly and via a mutable reference.
# 局部变量和范围
Move 中的局部变量是词法（静态）范围的。使用关键字 let 引入了新变量，这将隐藏任何以前的同名本地变量。局部变量是可变的，可以直接更新，也可以通过可变引用更新。

## Declaring Local Variables

### `let` bindings

Move programs use `let` to bind variable names to values:
## 声明局部变量
### `let` 绑定
移动程序使用 let 将变量名绑定到值：

```move
let x = 1;
let y = x + x:
```

`let` can also be used without binding a value to the local.

let 也可以在不将值绑定到本地的情况下使用。

```move
let x;
```

The local can then be assigned a value later.

然后可以稍后为本地分配一个值。

```move
let x;
if (cond) {
  x = 1
} else {
  x = 0
}
```

This can be very helpful when trying to extract a value from a loop when a default value cannot be
provided.

当无法提供默认值时，这在尝试从循环中提取值时非常有用。

```move
let x;
let cond = true;
let i = 0;
loop {
    (x, cond) = foo(i);
    if (!cond) break;
    i = i + 1;
}
```

### Variables must be assigned before use

Move's type system prevents a local variable from being used before it has been assigned.
### 变量必须在使用前赋值
Move 的类型系统防止在分配之前使用局部变量。

```move
let x;
x + x // ERROR!
```

```move
let x;
if (cond) x = 0;
x + x // ERROR!
```

```move
let x;
while (cond) x = 0;
x + x // ERROR!
```

### Valid variable names

Variable names can contain underscores `_`, letters `a` to `z`, letters `A` to `Z`, and digits `0`
to `9`. Variable names must start with either an underscore `_` or a letter `a` through `z`. They
_cannot_ start with uppercase letters.
### 有效的变量名
变量名称可以包含下划线 `_`、字母 a 到 z、字母 A 到 Z 以及数字 0 到 9。变量名称必须以下划线 `_` 或字母 a 到 z 开头。它们不能以大写字母开头。

```move
// all valid
let x = e;
let _x = e;
let _A = e;
let x0 = e;
let xA = e;
let foobar_123 = e;

// all invalid
let X = e; // ERROR!
let Foo = e; // ERROR!
```

### Type annotations

The type of a local variable can almost always be inferred by Move's type system. However, Move
allows explicit type annotations that can be useful for readability, clarity, or debuggability. The
syntax for adding a type annotation is:
### 类型注释
局部变量的类型几乎总是可以通过 Move 的类型系统推断出来。但是，Move 允许显式类型注释，这对可读性、清晰性或可调试性很有用。添加类型注释的语法是：

```move
let x: T = e; // "Variable x of type T is initialized to expression e"
```

Some examples of explicit type annotations:

显式类型注释的一些示例：

```move=
address 0x42 {
module example {

    struct S { f: u64, g: u64 }

    fun annotated() {
        let u: u8 = 0;
        let b: vector<u8> = b"hello";
        let a: address = @0x0;
        let (x, y): (&u64, &mut u64) = (&0, &mut 1);
        let S { f, g: f2 }: S = S { f: 0, g: 1 };
    }
}
}
```

Note that the type annotations must always be to the right of the pattern:

请注意，类型注释必须始终位于模式的右侧：

```move
let (x: &u64, y: &mut u64) = (&0, &mut 1); // ERROR! should be let (x, y): ... =
```

### When annotations are necessary

In some cases, a local type annotation is required if the type system cannot infer the type. This
commonly occurs when the type argument for a generic type cannot be inferred. For example:
### 需要注释时
在某些情况下，如果类型系统无法推断类型，则需要本地类型注释。当无法推断泛型类型的类型参数时，通常会发生这种情况。例如：

```move
let _v1 = vector::empty(); // ERROR!
//        ^^^^^^^^^^^^^^^ Could not infer this type. Try adding an annotation
let v2: vector<u64> = vector::empty(); // no error
```

In a rarer case, the type system might not be able to infer a type for divergent code (where all the
following code is unreachable). Both `return` and [`abort`](./abort-and-assert.md) are expressions
and can have any type. A [`loop`](./loops.md) has type `()` if it has a `break`, but if there is no
break out of the `loop`, it could have any type. If these types cannot be inferred, a type
annotation is required. For example, this code:

在极少数情况下，类型系统可能无法推断不同代码的类型（以下所有代码都无法访问）。 return 和 abort 都是表达式，可以有任何类型。如果循环有中断，则其类型为 ()，但如果循环没有中断，则它可以具有任何类型。如果无法推断出这些类型，则需要类型注释。例如，这段代码：

```move
let a: u8 = return ();
let b: bool = abort 0;
let c: signer = loop ();

let x = return (); // ERROR!
//  ^ Could not infer this type. Try adding an annotation
let y = abort 0; // ERROR!
//  ^ Could not infer this type. Try adding an annotation
let z = loop (); // ERROR!
//  ^ Could not infer this type. Try adding an annotation
```

Adding type annotations to this code will expose other errors about dead code or unused local
variables, but the example is still helpful for understanding this problem.

在这段代码中添加类型注释会暴露其他关于死代码或未使用的局部变量的错误，但该示例仍然有助于理解这个问题。

### Multiple declarations with tuples

`let` can introduce more than one local at a time using tuples. The locals declared inside the
parenthesis are initialized to the corresponding values from the tuple.
### 带有元组的多个声明
let 可以使用元组一次引入多个本地。括号内声明的局部变量被初始化为元组中的相应值。

```move
let () = ();
let (x0, x1) = (0, 1);
let (y0, y1, y2) = (0, 1, 2);
let (z0, z1, z2, z3) = (0, 1, 2, 3);
```

The type of the expression must match the arity of the tuple pattern exactly.

表达式的类型必须与元组模式的数量完全匹配。

```move
let (x, y) = (0, 1, 2); // ERROR!
let (x, y, z, q) = (0, 1, 2); // ERROR!
```

You cannot declare more than one local with the same name in a single `let`.
您不能在一个 let 中声明多个具有相同名称的本地。

```move
let (x, x) = 0; // ERROR!
```

### Multiple declarations with structs

`let` can also introduce more than one local at a time when destructuring (or matching against) a
struct. In this form, the `let` creates a set of local variables that are initialized to the values
of the fields from a struct. The syntax looks like this:
### 带有结构的多个声明
let 还可以在解构（或匹配）结构时一次引入多个本地。在这种形式中，let 创建了一组局部变量，这些变量被初始化为结构中字段的值。语法如下所示：

```move
struct T { f1: u64, f2: u64 }
```

```move
let T { f1: local1, f2: local2 } = T { f1: 1, f2: 2 };
// local1: u64
// local2: u64
```

Here is a more complicated example:

这是一个更复杂的例子：

```move
address 0x42 {
module example {
    struct X { f: u64 }
    struct Y { x1: X, x2: X }

    fun new_x(): X {
        X { f: 1 }
    }

    fun example() {
        let Y { x1: X { f }, x2 } = Y { x1: new_x(), x2: new_x() };
        assert!(f + x2.f == 2, 42);

        let Y { x1: X { f: f1 }, x2: X { f: f2 } } = Y { x1: new_x(), x2: new_x() };
        assert!(f1 + f2 == 2, 42);
    }
}
}
```

Fields of structs can serve double duty, identifying the field to bind _and_ the name of the
variable. This is sometimes referred to as punning.
结构的字段可以起到双重作用，识别要绑定的字段和变量的名称。这有时被称为双关语。

```move
let X { f } = e;
```

is equivalent to:

相当于：

```move
let X { f: f } = e;
```

As shown with tuples, you cannot declare more than one local with the same name in a single `let`.

如元组所示，您不能在单个 let 中声明多个具有相同名称的本地。

```move
let Y { x1: x, x2: x } = e; // ERROR!
```

### Destructuring against references

In the examples above for structs, the bound value in the let was moved, destroying the struct value
and binding its fields.
### 针对引用进行解构
在上面的结构示例中，let 中的绑定值被移动，破坏了结构值并绑定了它的字段。

```move
struct T { f1: u64, f2: u64 }
```

```move
let T { f1: local1, f2: local2 } = T { f1: 1, f2: 2 };
// local1: u64
// local2: u64
```

In this scenario the struct value `T { f1: 1, f2: 2 }` no longer exists after the `let`.

If you wish instead to not move and destroy the struct value, you can borrow each of its fields. For
example:
在这种情况下，结构值 T { f1: 1, f2: 2 } 在 let 之后不再存在。

如果您希望不移动和破坏结构值，则可以借用其每个字段。例如：

```move
let t = T { f1: 1, f2: 2 };
let T { f1: local1, f2: local2 } = &t;
// local1: &u64
// local2: &u64
```

And similarly with mutable references:
与可变引用类似：

```move
let t = T { f1: 1, f2: 2 };
let T { f1: local1, f2: local2 } = &mut t;
// local1: &mut u64
// local2: &mut u64
```

This behavior can also work with nested structs.
此行为也适用于嵌套结构。

```move
address 0x42 {
module example {
    struct X { f: u64 }
    struct Y { x1: X, x2: X }

    fun new_x(): X {
        X { f: 1 }
    }

    fun example() {
        let y = Y { x1: new_x(), x2: new_x() };

        let Y { x1: X { f }, x2 } = &y;
        assert!(*f + x2.f == 2, 42);

        let Y { x1: X { f: f1 }, x2: X { f: f2 } } = &mut y;
        *f1 = *f1 + 1;
        *f2 = *f2 + 1;
        assert!(*f1 + *f2 == 4, 42);
    }
}
}
```

### Ignoring Values

In `let` bindings, it is often helpful to ignore some values. Local variables that start with `_`
will be ignored and not introduce a new variable
### 忽略值
在 let 绑定中，忽略某些值通常很有帮助。以 _ 开头的局部变量将被忽略，不会引入新变量

```move
fun three(): (u64, u64, u64) {
    (0, 1, 2)
}
```

```move
let (x1, _, z1) = three();
let (x2, _y, z2) = three();
assert!(x1 + z1 == x2 + z2)
```

This can be necessary at times as the compiler will error on unused local variables
这有时是必要的，因为编译器会在未使用的局部变量上出错

```move
let (x1, y, z1) = three(); // ERROR!
//       ^ unused local 'y'
```

### General `let` grammar

All of the different structures in `let` can be combined! With that we arrive at this general
grammar for `let` statements:
### 一般 `let` 语法
let 中所有不同的结构都可以组合！这样，我们就得出了 let 语句的一般语法：

> _let-binding_ → **let** _pattern-or-list_ _type-annotation_<sub>_opt_</sub>
> _initializer_<sub>_opt_</sub> > _pattern-or-list_ → _pattern_ | **(** _pattern-list_ **)** >
> _pattern-list_ → _pattern_ **,**<sub>_opt_</sub> | _pattern_ **,** _pattern-list_ >
> _type-annotation_ → **:** _type_ _initializer_ → **=** _expression_

The general term for the item that introduces the bindings is a _pattern_. The pattern serves to
both destructure data (possibly recursively) and introduce the bindings. The pattern grammar is as
follows:

引入绑定的项目的通用术语是模式。该模式用于解构数据（可能是递归的）并引入绑定。模式语法如下：

> _pattern_ → _local-variable_ | _struct-type_ **{** _field-binding-list_ **}** >
> _field-binding-list_ → _field-binding_ **,**<sub>_opt_</sub> | _field-binding_ **,**
> _field-binding-list_ > _field-binding_ → _field_ | _field_ **:** _pattern_

A few concrete examples with this grammar applied:

应用此语法的一些具体示例：

```move
    let (x, y): (u64, u64) = (0, 1);
//       ^                           local-variable
//       ^                           pattern
//          ^                        local-variable
//          ^                        pattern
//          ^                        pattern-list
//       ^^^^                        pattern-list
//      ^^^^^^                       pattern-or-list
//            ^^^^^^^^^^^^           type-annotation
//                         ^^^^^^^^  initializer
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ let-binding

    let Foo { f, g: x } = Foo { f: 0, g: 1 };
//      ^^^                                    struct-type
//            ^                                field
//            ^                                field-binding
//               ^                             field
//                  ^                          local-variable
//                  ^                          pattern
//               ^^^^                          field-binding
//            ^^^^^^^                          field-binding-list
//      ^^^^^^^^^^^^^^^                        pattern
//      ^^^^^^^^^^^^^^^                        pattern-or-list
//                      ^^^^^^^^^^^^^^^^^^^^   initializer
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ let-binding
```

## Mutations

### Assignments

After the local is introduced (either by `let` or as a function parameter), the local can be
modified via an assignment:
## 突变
### 作业
在引入局部后（通过 let 或作为函数参数），可以通过赋值来修改局部：

```move
x = e
```

Unlike `let` bindings, assignments are expressions. In some languages, assignments return the value
that was assigned, but in Move, the type of any assignment is always `()`.

与 let 绑定不同，赋值是表达式。在某些语言中，赋值返回被赋值的值，但在 Move 中，任何赋值的类型始终是 ()。

```move
(x = e: ())
```

Practically, assignments being expressions means that they can be used without adding a new
expression block with braces (`{`...`}`).
实际上，赋值是表达式意味着它们可以在不添加带有大括号 ({...}) 的新表达式块的情况下使用。

```move
let x = 0;
if (cond) x = 1 else x = 2;
```

The assignment uses the same pattern syntax scheme as `let` bindings:

赋值使用与 let 绑定相同的模式语法方案：

```move=
address 0x42 {
module example {
    struct X { f: u64 }

    fun new_x(): X {
        X { f: 1 }
    }

    // This example will complain about unused variables and assignments.
    fun example() {
       let (x, _, z) = (0, 1, 3);
       let (x, y, f, g);

       (X { f }, X { f: x }) = (new_x(), new_x());
       assert!(f + x == 2, 42);

       (x, y, z, f, _, g) = (0, 0, 0, 0, 0, 0);
    }
}
}
```

Note that a local variable can only have one type, so the type of the local cannot change between
assignments.
注意一个局部变量只能有一种类型，所以局部变量的类型不能在赋值之间改变。

```move
let x;
x = 0;
x = false; // ERROR!
```

### Mutating through a reference

In addition to directly modifying a local with assignment, a local can be modified via a mutable
reference `&mut`.
### 通过引用进行变异
除了通过赋值直接修改局部外，还可以通过可变引用 &mut 修改局部。

```move
let x = 0;
let r = &mut x;
*r = 1;
assert!(x == 1, 42)
}
```

This is particularly useful if either:

(1) You want to modify different variables depending on some condition.

这在以下情况下特别有用：

(1) 您想根据某些条件修改不同的变量。

```move
let x = 0;
let y = 1;
let r = if (cond) &mut x else &mut y;
*r = *r + 1;
```

(2) You want another function to modify your local value.

(2) 你想要另一个函数来修改你的本地值。

```move
let x = 0;
modify_ref(&mut x);
```

This sort of modification is how you modify structs and vectors!

这种修改就是你修改结构和向量的方式！

```move
let v = vector::empty();
vector::push_back(&mut v, 100);
assert!(*vector::borrow(&v, 0) == 100, 42)
```

For more details, see [Move references](./references.md).

有关更多详细信息，请参阅移动引用。

## Scopes

Any local declared with `let` is available for any subsequent expression, _within that scope_.
Scopes are declared with expression blocks, `{`...`}`.

Locals cannot be used outside of the declared scope.
## 范围
使用 let 声明的任何本地表达式都可用于该范围内的任何后续表达式。范围用表达式块声明，{...}。

局部变量不能在声明的范围之外使用。

```move
let x = 0;
{
    let y = 1;
};
x + y // ERROR!
//  ^ unbound local 'y'
```

But, locals from an outer scope _can_ be used in a nested scope.

但是，来自外部作用域的本地变量可以在嵌套作用域中使用。

```move
{
    let x = 0;
    {
        let y = x + 1; // valid
    }
}
```

Locals can be mutated in any scope where they are accessible. That mutation survives with the local,
regardless of the scope that performed the mutation.

局部变量可以在可以访问的任何范围内进行变异。无论执行突变的范围如何，该突变都会在本地生存。

```move
let x = 0;
x = x + 1;
assert!(x == 1, 42);
{
    x = x + 1;
    assert!(x == 2, 42);
};
assert!(x == 2, 42);
```

### Expression Blocks

An expression block is a series of statements separated by semicolons (`;`). The resulting value of
an expression block is the value of the last expression in the block.
### 表达式块
表达式块是由分号 (;) 分隔的一系列语句。表达式块的结果值是块中最后一个表达式的值。

```move
{ let x = 1; let y = 1; x + y }
```

In this example, the result of the block is `x + y`.

A statement can be either a `let` declaration or an expression. Remember that assignments (`x = e`)
are expressions of type `()`.

在此示例中，块的结果是 x + y。

语句可以是 let 声明或表达式。请记住，赋值 (x = e) 是 () 类型的表达式。

```move
{ let x; let y = 1; x = 1; x + y }
```

Function calls are another common expression of type `()`. Function calls that modify data are
commonly used as statements.

函数调用是类型 () 的另一种常见表达方式。修改数据的函数调用通常用作语句。

```move
{ let v = vector::empty(); vector::push_back(&mut v, 1); v }
```

This is not just limited to `()` types---any expression can be used as a statement in a sequence!

这不仅限于 () 类型——任何表达式都可以用作序列中的语句！

```move
{
    let x = 0;
    x + 1; // value is discarded
    x + 2; // value is discarded
    b"hello"; // value is discarded
}
```

But! If the expression contains a resource (a value without the `drop` [ability](./abilities.md)),
you will get an error. This is because Move's type system guarantees that any value that is dropped
has the `drop` [ability](./abilities.md). (Ownership must be transferred or the value must be
explicitly destroyed within its declaring module.)

但！如果表达式包含资源（没有丢弃能力的值），您将收到错误消息。这是因为 Move 的类型系统保证任何被删除的值都具有删除能力。 （必须转移所有权，或者必须在其声明模块中显式销毁该值。）

```move
{
    let x = 0;
    Coin { value: x }; // ERROR!
//  ^^^^^^^^^^^^^^^^^ unused value without the `drop` ability
    x
}
```

If a final expression is not present in a block---that is, if there is a trailing semicolon `;`,
there is an implicit unit `()` value. Similarly, if the expression block is empty, there is an
implicit unit `()` value.

如果块中不存在最终表达式——也就是说，如果有一个尾随分号;，则有一个隐含的 unit () 值。同样，如果表达式块为空，则存在隐含的 unit() 值。

```move
// Both are equivalent
// 两者是等价的
{ x = x + 1; 1 / x; }
{ x = x + 1; 1 / x; () }
```

```move
// Both are equivalent
{ }
{ () }
```

An expression block is itself an expression and can be used anyplace an expression is used. (Note:
The body of a function is also an expression block, but the function body cannot be replaced by
another expression.)
表达式块本身就是一个表达式，可以在任何使用表达式的地方使用。 （注意：函数体也是一个表达式块，但函数体不能被另一个表达式代替。）

```move
let my_vector: vector<vector<u8>> = {
    let v = vector::empty();
    vector::push_back(&mut v, b"hello");
    vector::push_back(&mut v, b"goodbye");
    v
};
```

(The type annotation is not needed in this example and only added for clarity.)
（此示例中不需要类型注释，只是为了清楚起见而添加。）

### Shadowing

If a `let` introduces a local variable with a name already in scope, that previous variable can no
longer be accessed for the rest of this scope. This is called _shadowing_.

### 隐藏
如果一个 let 引入了一个名称已经在作用域中的局部变量，则该作用域的其余部分将无法再访问先前的变量。这称为隐藏。

```move
let x = 0;
assert!(x == 0, 42);

let x = 1; // x is shadowed
assert!(x == 1, 42);
```

When a local is shadowed, it does not need to retain the same type as before.

当局部被遮蔽时，它不需要保留与以前相同的类型。

```move
let x = 0;
assert!(x == 0, 42);

let x = b"hello"; // x is shadowed
assert!(x == b"hello", 42);
```

After a local is shadowed, the value stored in the local still exists, but will no longer be
accessible. This is important to keep in mind with values of types without the
[`drop` ability](./abilities.md), as ownership of the value must be transferred by the end of the
function.

在本地被遮蔽后，存储在本地的值仍然存在，但将不再可访问。对于没有删除能力的类型的值，请记住这一点很重要，因为值的所有权必须在函数结束时转移。

```move
address 0x42 {
    module example {
        struct Coin has store { value: u64 }

        fun unused_resource(): Coin {
            let x = Coin { value: 0 }; // ERROR!
//              ^ This local still contains a value without the `drop` ability
            x.value = 1;
            let x = Coin { value: 10 };
            x
//          ^ Invalid return
        }
    }
}
```

When a local is shadowed inside a scope, the shadowing only remains for that scope. The shadowing is
gone once that scope ends.
当本地在范围内被遮蔽时，该遮蔽仅保留在该范围内。一旦该范围结束，阴影就消失了。

```move
let x = 0;
{
    let x = 1;
    assert!(x == 1, 42);
};
assert!(x == 0, 42);
```

Remember, locals can change type when they are shadowed.

请记住，本地人在被遮蔽时可以更改类型。

```move
let x = 0;
{
    let x = b"hello";
    assert!(x = b"hello", 42);
};
assert!(x == 0, 42);
```

## Move and Copy

All local variables in Move can be used in two ways, either by `move` or `copy`. If one or the other
is not specified, the Move compiler is able to infer whether a `copy` or a `move` should be used.
This means that in all of the examples above, a `move` or a `copy` would be inserted by the
compiler. A local variable cannot be used without the use of `move` or `copy`.

`copy` will likely feel the most familiar coming from other programming languages, as it creates a
new copy of the value inside of the variable to use in that expression. With `copy`, the local
variable can be used more than once.
## 移动和复制
Move 中的所有局部变量都可以通过两种方式使用，通过移动或复制。如果未指定其中之一，则 Move 编译器能够推断应该使用副本还是移动。这意味着在上述所有示例中，编译器将插入移动或复制。如果不使用移动或复制，就不能使用局部变量。

复制可能会让人感觉最熟悉来自其他编程语言，因为它会在变量内部创建一个新的值副本以在该表达式中使用。使用复制，可以多次使用局部变量。

```move
let x = 0;
let y = copy x + 1;
let z = copy x + 2;
```

Any value with the `copy` [ability](./abilities.md) can be copied in this way.

`move` takes the value out of the local variable _without_ copying the data. After a `move` occurs,
the local variable is unavailable.

任何具有复制能力的值都可以通过这种方式复制。

move 从局部变量中取出值而不复制数据。移动发生后，局部变量不可用。

```move
let x = 1;
let y = move x + 1;
//      ------ Local was moved here
let z = move x + 2; // Error!
//      ^^^^^^ Invalid usage of local 'x'
y + z
```

### Safety

Move's type system will prevent a value from being used after it is moved. This is the same safety
check described in [`let` declaration](#let-bindings) that prevents local variables from being used
before it is assigned a value.
### 安全
Move 的类型系统会阻止一个值在移动后被使用。这与 let 声明中描述的安全检查相同，可防止在为其赋值之前使用局部变量。

<!-- For more information, see TODO future section on ownership and move semantics. -->

### Inference

As mentioned above, the Move compiler will infer a `copy` or `move` if one is not indicated. The
algorithm for doing so is quite simple:

- Any scalar value with the `copy` [ability](./abilities.md) is given a `copy`.
- Any reference (both mutable `&mut` and immutable `&`) is given a `copy`.
  - Except under special circumstances where it is made a `move` for predictable borrow checker
    errors.
- Any other value is given a `move`.
  - This means that even though other values might be have the `copy` [ability](./abilities.md), it
    must be done _explicitly_ by the programmer.
  - This is to prevent accidental copies of large data structures.

For example:
### 推理
如上所述，如果未指明，Move 编译器将推断出副本或移动。这样做的算法非常简单：

- 任何具有复制能力的标量值都会被赋予一个副本。
- 任何引用（可变的 &mut 和不可变的 &）都会给出一个副本。
  - 除非在特殊情况下会因可预测的借用检查器错误而采取行动。
- 任何其他值都会被移动。
  - 这意味着即使其他值可能具有复制能力，也必须由程序员明确完成。
  - 这是为了防止意外复制大型数据结构。
例如：

```move
let s = b"hello";
let foo = Foo { f: 0 };
let coin = Coin { value: 0 };

let s2 = s; // move
let foo2 = foo; // move
let coin2 = coin; // move

let x = 0;
let b = false;
let addr = @0x42;
let x_ref = &x;
let coin_ref = &mut coin2;

let x2 = x; // copy
let b2 = b; // copy
let addr2 = @0x42; // copy
let x_ref2 = x_ref; // copy
let coin_ref2 = coin_ref; // copy
```
