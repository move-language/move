# 局部变量和作用域(Local Variables and Scopes)

Local variables in Move are lexically (statically) scoped. New variables are introduced with the
keyword `let`, which will shadow any previous local with the same name. Locals are mutable and can
be updated both directly and via a mutable reference.

在Move语言中，局部变量的解析依赖于词法作用域(lexically scoped)或静态作用域(statically scoped)。新变量是通过关键字 `let` 引入的，它将遮蔽任何之前的局部同名变量。局部变量是可变的，可以直接更新，也可以通过可变引用更新。

## 声明局部变量 (Declaring Local Variables)

### `let` bindings (`let` 绑定)

Move programs use `let` to bind variable names to values:

Move语言程序使用 `let` 来给变量名赋值：

```move
let x = 1;
let y = x + x:
```

`let` can also be used without binding a value to the local.

`let` 使用时也可以不绑定任何数值。

```move
let x;
```

The local can then be assigned a value later.

这些局部变量可以被稍后赋值。

```move
let x;
if (cond) {
  x = 1
} else {
  x = 0
}
```

This can be very helpful when trying to extract a value from a loop when a default value cannot be provided.

当无法提供默认值时，且尝试从循环中提取值时非常有用。

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

### 变量必须在使用前赋值 (Variables must be assigned before use)

Move's type system prevents a local variable from being used before it has been assigned.

Move的类型系统防止在赋值前使用局部变量。

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

### 有效的变量名 (Valid variable names)


Variable names can contain underscores `_`, letters `a` to `z`, letters `A` to `Z`, and digits `0`
to `9`. Variable names must start with either an underscore `_` or a letter `a` through `z`. They
_cannot_ start with uppercase letters.

变量名可以包含下划线 `_`，小写字母 `a` 到 `z` ，大写字母 `A` 到 `Z`， 和数字 `0` 到 `9` 。变量名必须以下划线`_`或者以小写字母`a`到`z`开头。它们 _不可以_ 用大写字母开头。

```move
// 正确写法
let x = e;
let _x = e;
let _A = e;
let x0 = e;
let xA = e;
let foobar_123 = e;

// 非正确写法
let X = e; // ERROR!
let Foo = e; // ERROR!
```

### 类型标注 (Type annotations)

The type of a local variable can almost always be inferred by Move's type system. However, Move
allows explicit type annotations that can be useful for readability, clarity, or debuggability. The
syntax for adding a type annotation is:

局部变量的类型几乎总是可以通过 Move 的类型系统推断出来。 但是，Move 允许显式标注类型，这对可读性、清晰性或可调试性很有用。 添加类型标注的语法如下：

```move
let x: T = e; // "变量 x 的类型 T 被定义为表达式 e"
```

Some examples of explicit type annotations:
一些显式标注类型的例子：

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

值得注意的是，类型标注必须总是位于变量右边的模式：

```move
let (x: &u64, y: &mut u64) = (&0, &mut 1); // 错误! 正确写法是 let (x, y): ... =
```

### 何时需要(类型)标注 (When annotations are necessary)

In some cases, a local type annotation is required if the type system cannot infer the type. This
commonly occurs when the type argument for a generic type cannot be inferred. For example:

在某些情况下，如果类型系统无法推断类型，则需要局部类型标注。这常常发生于无法推断某个泛型(generic type)的类型参数时。比如：

```move
let _v1 = vector::empty(); // 错误!
//        ^^^^^^^^^^^^^^^ 无法推断它的类型。 请加上注解
let v2: vector<u64> = vector::empty(); // 正确
```

In a rarer case, the type system might not be able to infer a type for divergent code (where all the
following code is unreachable). Both `return` and [`abort`](./abort-and-assert.md) are expressions
and can have any type. A [`loop`](./loops.md) has type `()` if it has a `break`, but if there is no
break out of the `loop`, it could have any type. If these types cannot be inferred, a type
annotation is required. For example, this code:

在极少数情况下，Move的类型系统并不能推断出一段发散式代码(divergent code)的类型(后面所有代码无法访问)。在Move语言中，`return` 和 [`abort`](./abort-and-assert.md)都属于表达式，它们可以返回任何类型。如果一段 [`loop`](./loops.md) 有 `break` 语句，那么它的返回类型是`()`, 然而如果它不包含`break`语句，它的返回类型可以是任何类型。如果这些类型无法推断，类型标注是必须的。比如：

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

在这段代码中添加类型标注会暴露其他关于死代码或未使用的局部变量的错误，无论如何这示例仍然有助于理解这个问题。


### 元组式的多个(变量)声明 (Multiple declarations with tuples)

`let` can introduce more than one local at a time using tuples. The locals declared inside the
parenthesis are initialized to the corresponding values from the tuple.

`let` 可以使用元组一次引入多个局部变量。在括号里面声明的局部变量会被初始化为元组中的对应值。

```move
let () = ();
let (x0, x1) = (0, 1);
let (y0, y1, y2) = (0, 1, 2);
let (z0, z1, z2, z3) = (0, 1, 2, 3);
```

The type of the expression must match the arity of the tuple pattern exactly.

表达式的类型必须与元组模式的数量完全匹配。

```move
let (x, y) = (0, 1, 2); // 错误!
let (x, y, z, q) = (0, 1, 2); // 错误!
```

You cannot declare more than one local with the same name in a single `let`.

您不能在单个 `let` 中声明多个具有相同名称的局部变量。

```move
let (x, x) = 0; // 错误!
```

### 结构体式的多个(变量)声明(Multiple declarations with structs)

`let` can also introduce more than one local at a time when destructuring (or matching against) a
struct. In this form, the `let` creates a set of local variables that are initialized to the values
of the fields from a struct. The syntax looks like this:

`let` 也可以在解构(或匹配)结构时一次引入多个局部变量。在这种形式中，`let` 创建了一组局部变量，这些变量被初始化为结构中的字段的值。语法如下所示：

```move
struct T { f1: u64, f2: u64 }
```

```move
let T { f1: local1, f2: local2 } = T { f1: 1, f2: 2 };
// local1: u64
// local2: u64
```

Here is a more complicated example:

这是一个更复杂的示例：

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

结构体的字段可以起到双重作用：识别要绑定的字段 _和_ 命名变量。这有时被称为双关语。

```move
let X { f } = e;
```

is equivalent to:

等价于：

```move
let X { f: f } = e;
```

As shown with tuples, you cannot declare more than one local with the same name in a single `let`.

如元组所示，您不能在单个 `let` 中声明多个具有相同名称的局部变量。

```move
let Y { x1: x, x2: x } = e; // 错误!(两个x同名了，注者注)
```

### 针对引用进行解构(Destructuring against references)

In the examples above for structs, the bound value in the let was moved, destroying the struct value
and binding its fields.

在上面的结构体示例中，`let` 中绑定的值被移动了，这销毁了结构体的值并同时绑定了它的字段(到变量)。

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

在这种场景下结构体的值 `T { f1: 1, f2: 2 }` 会在 `let` 后消失.

如果您希望不移动和销毁结构体的值，则可以借用其中的每个字段。比如说：

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

此行为也适用于嵌套结构体。

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

### 忽略值(Ignoring Values)

In `let` bindings, it is often helpful to ignore some values. Local variables that start with `_`
will be ignored and not introduce a new variable

在 `let` 绑定中，忽略某些值通常很有帮助。以 `_` 开头的局部变量将被忽略并且不会引入新变量。

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

This can be necessary at times as the compiler will error on unused local variables。

这有时是必要的，因为编译器会在检测到未使用的局部变量时报错。

```move
let (x1, y, z1) = three(); // 错误!
//       ^ 未被使用的局部变量 'y'
```

### 通用的 `let` 语法 (General `let` grammar)

All of the different structures in `let` can be combined! With that we arrive at this general
grammar for `let` statements:

`let` 中的所有不同结构都可以组合！有了这个，我们撰写了`let`语句的通用语法：

> _let-binding_ → **let** _pattern-or-list_ _type-annotation_<sub>_opt_</sub>
> _initializer_<sub>_opt_</sub> > _pattern-or-list_ → _pattern_ | **(** _pattern-list_ **)** >
> _pattern-list_ → _pattern_ **,**<sub>_opt_</sub> | _pattern_ **,** _pattern-list_ >
> _type-annotation_ → **:** _type_ _initializer_ → **=** _expression_

The general term for the item that introduces the bindings is a _pattern_. The pattern serves to
both destructure data (possibly recursively) and introduce the bindings. The pattern grammar is as
follows:

引入绑定(binding)的项(item)的通用术语是 _模式_。这种模式(pattern)用于解构数据(可能递归)并引入绑定。模式语法如下：

> _pattern_ → _local-variable_ | _struct-type_ **{** _field-binding-list_ **}** >
> _field-binding-list_ → _field-binding_ **,**<sub>_opt_</sub> | _field-binding_ **,**
> _field-binding-list_ > _field-binding_ → _field_ | _field_ **:** _pattern_

A few concrete examples with this grammar applied:

应用此语法的一些具体示例：

```move
    let (x, y): (u64, u64) = (0, 1);
//       ^                           局部变量
//       ^                           模式
//          ^                        局部变量
//          ^                        模式
//          ^                        模式列表
//       ^^^^                        模式列表
//      ^^^^^^                       模式或列表
//            ^^^^^^^^^^^^           类型注解
//                         ^^^^^^^^  初始化的值
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ let-绑定

    let Foo { f, g: x } = Foo { f: 0, g: 1 };
//      ^^^                                    结构类型
//            ^                                字段
//            ^                                字段绑定
//               ^                             字段
//                  ^                          局部变量
//                  ^                          模式
//               ^^^^                          字段绑定
//            ^^^^^^^                          字段绑定列表
//      ^^^^^^^^^^^^^^^                        模式
//      ^^^^^^^^^^^^^^^                        模式或列表
//                      ^^^^^^^^^^^^^^^^^^^^   初始化的值
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ let-绑定
```

## 变更(mutations)

### 赋值(Assignments)

After the local is introduced (either by `let` or as a function parameter), the local can be
modified via an assignment:

在引入一个局部变量后(使用`let`或是用作一个函数参数(function parameter))，局部(变量)可以通过赋值进行修改：

```move
x = e
```

Unlike `let` bindings, assignments are expressions. In some languages, assignments return the value
that was assigned, but in Move, the type of any assignment is always `()`.

不同于 `let` 的绑定，赋值属于表达式。在一些编程语言中，赋值表达式会返回被赋予的值，但是在move语言中，任何赋值返回的类型永远都是`()`。

```move
(x = e: ())
```

Practically, assignments being expressions means that they can be used without adding a new
expression block with braces (`{`...`}`).

实际应用中，赋值属于表达式意味着使用它们时不用添加额外表达块(expression block)的括号。(`{`...`}`)

```move
let x = 0;
if (cond) x = 1 else x = 2;
```

The assignment uses the same pattern syntax scheme as `let` bindings:

赋值使用与 `let` 绑定相同的模式语法方案：：

```move=
address 0x42 {
    module example {
        struct X { f: u64 }

        fun new_x(): X {
            X { f: 1 }
        }

        // 以下的例子会因为未使用的变量和赋值报错。
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

值得注意的是一个局部变量只能有一种类型，所以局部变量不能在赋值之间(多次赋值)改变类型。

```move
let x;
x = 0;
x = false; // 错误!
```

### 通过引用进行变更 (Mutating through a reference)

In addition to directly modifying a local with assignment, a local can be modified via a mutable
reference `&mut`.

除了通过赋值直接修改局部变量外，还可以通过可变引用 `&mut` 的方式修改局部变量。

```move
let x = 0;
let r = &mut x;
*r = 1;
assert!(x == 1, 42)
}
```

This is particularly useful if either:

(1) You want to modify different variables depending on some condition.

这在以下情况下特别有用:

(1) 您想根据某些条件修改不同的变量。

```move
let x = 0;
let y = 1;
let r = if (cond) &mut x else &mut y;
*r = *r + 1;
```

(2) You want another function to modify your local value.

(2) 您想要另一个函数来修改您的局部(变量)值。

```move
let x = 0;
modify_ref(&mut x);
```

This sort of modification is how you modify structs and vectors!

这种修改就是你更改结构体和数组的方式！

```move
let v = vector::empty();
vector::push_back(&mut v, 100);
assert!(*vector::borrow(&v, 0) == 100, 42)
```


For more details, see [Move references](./references.md).

关于更多细节可以参考 [Move references](./references.md).

## 作用域 (Scopes)

Any local declared with `let` is available for any subsequent expression, _within that scope_.
Scopes are declared with expression blocks, `{`...`}`.

Locals cannot be used outside of the declared scope.

使用 `let` 声明的任何局部变量都可用于任何后续表达式，_在该范围内_。作用域用表达式块(expression blocks)声明，`{`...`}`。

局部变量不能在声明的作用域之外使用。

```move
let x = 0;
{
    let y = 1;
};
x + y // 错误!
//  ^ 未绑定的局部变量“y”
```
But, locals from an outer scope _can_ be used in a nested scope.

但是，来自作用域外部的部变量 _可以_ 在嵌套作用域中使用。

```move
{
    let x = 0;
    {
        let y = x + 1; // 合规范的
    }
}
```

Locals can be mutated in any scope where they are accessible. That mutation survives with the local,
regardless of the scope that performed the mutation.

局部变量可以在允许访问的任何作用域内进行变更(mutation)。与进行变更的作用域无关，这种变更会跟随局部变量的生命周期。


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

### 表达式块(Expression Blocks)

An expression block is a series of statements separated by semicolons (`;`). The resulting value of
an expression block is the value of the last expression in the block.

表达式块是由分号 (`;`) 分隔的一系列语句。结果值为表达式块是块中最后一个表达式的值。

```move
{ let x = 1; let y = 1; x + y }
```

In this example, the result of the block is `x + y`.

A statement can be either a `let` declaration or an expression. Remember that assignments (`x = e`)
are expressions of type `()`.

在此示例中, 此区块的结果是 `x + y`.

语句可以是 `let` 声明或表达式。请记住赋值(`x = e`)是 `()` 类型的表达式。

```move
{ let x; let y = 1; x = 1; x + y }
```

Function calls are another common expression of type `()`. Function calls that modify data are
commonly used as statements.

函数调用是 `()` 类型的另一种常见表达方式。修改数据的函数调用通常被用作语句表达(`statements`)。

```move
{ let v = vector::empty(); vector::push_back(&mut v, 1); v }
```

This is not just limited to `()` types---any expression can be used as a statement in a sequence!

这不仅限于 `()` 类型——任何表达式都可以用作序列中的语句！

```move
{
    let x = 0;
    x + 1; // 值会被丢弃
    x + 2; // 值会被丢弃
    b"hello"; // 值会被丢弃
}
```

But! If the expression contains a resource (a value without the `drop` [ability](./abilities.md)),
you will get an error. This is because Move's type system guarantees that any value that is dropped
has the `drop` [ability](./abilities.md). (Ownership must be transferred or the value must be
explicitly destroyed within its declaring module.)

但是！如果表达式包含一个没有 `drop` [能力](./abilities.md) 的值的资源，程序会返回错误。这是因为 Move 的类型系统保证任何被丢弃的值有 `drop` [能力](./abilities.md)。 (所有权必须被转让或一个值必须在其声明模块中被显式销毁。)

```move
{
    let x = 0;
    Coin { value: x }; // ERROR!
//  ^^^^^^^^^^^^^^^^^ 没有 `drop` 能力的未使用值
    x
}
```

If a final expression is not present in a block---that is, if there is a trailing semicolon `;`,
there is an implicit unit `()` value. Similarly, if the expression block is empty, there is an
implicit unit `()` value.

如果块中不存在最终表达式---也就是说，如果有一个尾随分号`;`，有一个隐含的 unit `()` 值。同样，如果表达式块为空，则存在隐式 unit `()` 值。

```move
// 两者是相同的
{ x = x + 1; 1 / x; }
{ x = x + 1; 1 / x; () }
```

```move
// 两者是相同的
{ }
{ () }
```

An expression block is itself an expression and can be used anyplace an expression is used. (Note:
The body of a function is also an expression block, but the function body cannot be replaced by
another expression.)

表达式块本身就是一个表达式，可以在任何使用表达式的地方使用。 (注意：函数体也是表达式块，但函数体不能被另一个表达式替换。)

```move
let my_vector: vector<vector<u8>> = {
    let v = vector::empty();
    vector::push_back(&mut v, b"hello");
    vector::push_back(&mut v, b"goodbye");
    v
};
```

(The type annotation is not needed in this example and only added for clarity.)

(此示例中不需要类型标注，只是为了清晰而添加。)

### 遮蔽(shadowing)

If a `let` introduces a local variable with a name already in scope, that previous variable can no
longer be accessed for the rest of this scope. This is called _shadowing_.

如果一个 `let` 引入了一个已经在作用域内的同名局部变量，那么之前的变量不能继续在此作用域的其余部分访问。这称为 _遮蔽_ ( _shadowing_ )。

```move
let x = 0;
assert!(x == 0, 42);

let x = 1; // x被遮蔽了
assert!(x == 1, 42);
```

When a local is shadowed, it does not need to retain the same type as before.

当局部变量被遮蔽时，它不需要保留与以前相同的类型。

```move
let x = 0;
assert!(x == 0, 42);

let x = b"hello"; // x被遮蔽了
assert!(x == b"hello", 42);
```

After a local is shadowed, the value stored in the local still exists, but will no longer be
accessible. This is important to keep in mind with values of types without the
[`drop` ability](./abilities.md), as ownership of the value must be transferred by the end of the
function.

一个局部变量被遮蔽后，存储在局部变量的值仍然存在，但将变得不再可访问。对于没有[`drop` 能力](./abilities.md)的类型的值，请记住这一点很重要，因为值的所有权必须在函数结束时转移。

```move
address 0x42 {
    module example {
        struct Coin has store { value: u64 }

        fun unused_resource(): Coin {
            let x = Coin { value: 0 }; // 错误!
//              ^ 这个局部变量仍然包含一个没有 `drop` 能力的值
            x.value = 1;
            let x = Coin { value: 10 };
            x
//          ^ 不合规范的返回
        }
    }
}
```

When a local is shadowed inside a scope, the shadowing only remains for that scope. The shadowing is
gone once that scope ends.

当局部变量在作用域内被遮蔽时，该遮蔽作用仅保留在该作用域内。一旦该作用域结束，遮蔽就会自动消失。

```move
let x = 0;
{
    let x = 1;
    assert!(x == 1, 42);
};
assert!(x == 0, 42);
```

Remember, locals can change type when they are shadowed.

注意，局部变量在被遮蔽时可以更改类型。

```move
let x = 0;
{
    let x = b"hello";
    assert!(x = b"hello", 42);
};
assert!(x == 0, 42);
```

## 移动和复制(Move and Copy)

All local variables in Move can be used in two ways, either by `move` or `copy`. If one or the other
is not specified, the Move compiler is able to infer whether a `copy` or a `move` should be used.
This means that in all of the examples above, a `move` or a `copy` would be inserted by the
compiler. A local variable cannot be used without the use of `move` or `copy`.

`copy` will likely feel the most familiar coming from other programming languages, as it creates a
new copy of the value inside of the variable to use in that expression. With `copy`, the local
variable can be used more than once.

Move 中的所有局部变量都可以通过两种方式使用：通过 `move` 或 `copy`。如果其中一个未被指定时，Move 编译器能够推断应该使用 `copy` 或 `move`。这意味着在上述所有示例中，`move` 或 `copy` 将被编译器插入。不使用 `move` 或 `copy` 就不能使用局部变量。

`copy` 对来自其他编程语言(的开发者)来说可能会觉得最熟悉，因为它创建了一个要在该表达式中使用的变量内部值的新副本。使用 `copy`，本地变量可以多次使用。

```move
let x = 0;
let y = copy x + 1;
let z = copy x + 2;
```

Any value with the `copy` [ability](./abilities.md) can be copied in this way.

`move` takes the value out of the local variable _without_ copying the data. After a `move` occurs,
the local variable is unavailable.

任何带有 `copy` [能力](./abilities.md) 的值都可以通过这种方式复制。

`move` 从局部变量中取走值 _而不是_ 复制数据。发生`移动`后，局部变量会不可用。

```move
let x = 1;
let y = move x + 1;
//      ------ 局部变量被移动到这里了
let z = move x + 2; // 错误!
//      ^^^^^^ 不合规范的'x'使用方式
y + z
```

### 安全性(Safety)

Move's type system will prevent a value from being used after it is moved. This is the same safety
check described in [`let` declaration](#let-bindings) that prevents local variables from being used
before it is assigned a value.

Move 的类型系统会防止一个值在移动后被使用。这和 [`let` 声明](#let-bindings) 中描述的防止在局部变量在赋值之前被使用是一样的安全检查。

<!-- For more information, see TODO future section on ownership and move semantics. -->

<!-- 如了解更多信息, 参阅未来所有权和移动语义的部分 (TODO )。 -->

### 推断(Inference)

As mentioned above, the Move compiler will infer a `copy` or `move` if one is not indicated. The
algorithm for doing so is quite simple:

- Any scalar value with the `copy` [ability](./abilities.md) is given a `copy`.
- Any reference (both mutable `&mut` and immutable `&`) is given a `copy`.
  - Except under special circumstances where it is made a `move` for predictable borrow checker errors.
- Any other value is given a `move`.
  - This means that even though other values might be have the `copy` [ability](./abilities.md), it must be done _explicitly_ by the programmer.
  - This is to prevent accidental copies of large data structures.

如上所述，如果未指明，Move 编译器将推断出“复制”或“移动”。它的算法非常简单：

- 任何带有 `copy` [能力](./abilities.md) 的标量值都被赋予了 `copy`。
- 任何引用(可变 `&mut`和不可变 `&`)都被赋予一个`copy`。
  - 除非在预估借用检查器出错的特殊情况下，会进行 `move` 操作.
- 任何其他值都被赋予 `Move`。
  - 这意味着即使其他值可能具有 `copy` [能力](./abilities.md)，它必须由程序员 _显式_ 声明。
  - 这是为了防止意外复制很大的数据结构。

例如：

```move
let s = b"hello";
let foo = Foo { f: 0 };
let coin = Coin { value: 0 };

let s2 = s; // 移动
let foo2 = foo; // 移动
let coin2 = coin; // 移动

let x = 0;
let b = false;
let addr = @0x42;
let x_ref = &x;
let coin_ref = &mut coin2;

let x2 = x; // 复制
let b2 = b; // 复制
let addr2 = @0x42; // 复制
let x_ref2 = x_ref; // 复制
let coin_ref2 = coin_ref; // 复制
```
