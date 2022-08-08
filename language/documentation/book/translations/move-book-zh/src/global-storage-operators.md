# Global Storage - Operators

Move programs can create, delete, and update [resources](./structs-and-resources.md) in global storage using the following five instructions:
# 全球存储 - 操作符
移动程序可以使用以下五个指令在全局存储中创建、删除和更新资源：


| Operation                              | Description                                                     | Aborts?                                 |
---------------------------------------- |---------------------------------------------------------------- |---------------------------------------- |
|`move_to<T>(&signer,T)`                 | Publish `T` under `signer.address`                              | If `signer.address` already holds a `T` |
|`move_from<T>(address): T`              | Remove `T` from `address` and return it                         | If `address` does not hold a `T`        |
|`borrow_global_mut<T>(address): &mut T` | Return a mutable reference to the `T` stored under `address`    | If `address` does not hold a `T`        |
|`borrow_global<T>(address): &T`         | Return an immutable reference to the `T` stored under `address` | If `address` does not hold a `T`        |
|`exists<T>(address): bool`              | Return `true` if a `T` is stored under `address`                |  Never                                  |


Each of these instructions is parameterized by a type `T` with the [`key` ability](./abilities.md). However, each type `T` *must be declared in the current module*. This ensures that a resource can only be manipulated via the API exposed by its defining module. The instructions also take either an [`address`](./address.md) or [`&signer`](./signer.md) representing the account address where the resource of type `T` is stored.

这些指令中的每一个都由具有关键能力的类型 T 参数化。但是，每个类型 T 都必须在当前模块中声明。这确保了资源只能通过其定义模块公开的 API 进行操作。这些指令还采用地址或 &signer 表示存储类型 T 资源的帐户地址。

## References to resources

References to global resources returned by `borrow_global` or `borrow_global_mut` mostly behave like references to local storage: they can be extended, read, and written using ordinary [reference operators](./references.md) and passed as arguments to other function. However, there is one important difference between local and global references: **a function cannot return a reference that points into global storage**. For example, these two functions will each fail to compile:

## 对资源的引用
borrow_global 或 borrow_global_mut 返回的对全局资源的引用主要表现为对本地存储的引用：它们可以使用普通的引用运算符进行扩展、读取和写入，并作为参数传递给其他函数。但是，本地引用和全局引用之间有一个重要区别：函数不能返回指向全局存储的引用。例如，这两个函数都将无法编译：

```move
struct R has key { f: u64 }
// will not compile
fun ret_direct_resource_ref_bad(a: address): &R {
    borrow_global<R>(a) // error!
}
// also will not compile
fun ret_resource_field_ref_bad(a: address): &u64 {
    &borrow_global<R>(a).f // error!
}
```

Move must enforce this restriction to guarantee absence of dangling references to global storage. [This](#reference-safety-for-global-resources) section contains much more detail for the interested reader.

Move 必须强制执行此限制以保证不存在对全局存储的悬空引用。本节为感兴趣的读者提供了更多详细信息。

## Global storage operators with generics

Global storage operations can be applied to generic resources with both instantiated and uninstantiated generic type parameters:

## 具有泛型的全局存储运算符
全局存储操作可以应用于具有实例化和未实例化的泛型类型参数的泛型资源：

```move
struct Container<T> has key { t: T }

// Publish a Container storing a type T of the caller's choosing
fun publish_generic_container<T>(account: &signer, t: T) {
    move_to<Container<T>>(account, Container { t })
}

/// Publish a container storing a u64
fun publish_instantiated_generic_container(account: &signer, t: u64) {
    move_to<Container<u64>>(account, Container { t })
}
```

The ability to index into global storage via a type parameter chosen at runtime is a powerful Move feature known as *storage polymorphism*. For more on the design patterns enabled by this feature, see [Move generics](./generics.md).

通过在运行时选择的类型参数对全局存储进行索引的能力是一种强大的移动功能，称为存储多态性。有关此功能启用的设计模式的更多信息，请参阅移动泛型。

## Example: `Counter`

The simple `Counter` module below exercises each of the five global storage operators. The API exposed by this module allows:

- Anyone to publish a `Counter` resource under their account
- Anyone to check if a `Counter` exists under any address
- Anyone to read or increment the value of a `Counter` resource under any address
- An account that stores a `Counter` resource to reset it to zero
- An account that stores a `Counter` resource to remove and delete it

## 示例：计数器
下面的简单 Counter 模块练习了五个全局存储运算符中的每一个。该模块公开的 API 允许：

- 任何人都可以在其帐户下发布 Counter 资源
- 任何人都可以检查任何地址下是否存在计数器
- 任何人都可以读取或增加任何地址下的 Counter 资源的值
- 存储计数器资源以将其重置为零的帐户
- 存储 Counter 资源以移除和删除它的帐户

```move
address 0x42 {
module counter {
    use std::signer;

    /// Resource that wraps an integer counter
    struct Counter has key { i: u64 }

    /// Publish a `Counter` resource with value `i` under the given `account`
    public fun publish(account: &signer, i: u64) {
      // "Pack" (create) a Counter resource. This is a privileged operation that
      // can only be done inside the module that declares the `Counter` resource
      move_to(account, Counter { i })
    }

    /// Read the value in the `Counter` resource stored at `addr`
    public fun get_count(addr: address): u64 acquires Counter {
        borrow_global<Counter>(addr).i
    }

    /// Increment the value of `addr`'s `Counter` resource
    public fun increment(addr: address) acquires Counter {
        let c_ref = &mut borrow_global_mut<Counter>(addr).i;
        *c_ref = *c_ref + 1
    }

    /// Reset the value of `account`'s `Counter` to 0
    public fun reset(account: &signer) acquires Counter {
        let c_ref = &mut borrow_global_mut<Counter>(signer::address_of(account)).i;
        *c_ref = 0
    }

    /// Delete the `Counter` resource under `account` and return its value
    public fun delete(account: &signer): u64 acquires Counter {
        // remove the Counter resource
        let c = move_from<Counter>(signer::address_of(account));
        // "Unpack" the `Counter` resource into its fields. This is a
        // privileged operation that can only be done inside the module
        // that declares the `Counter` resource
        let Counter { i } = c;
        i
    }

    /// Return `true` if `addr` contains a `Counter` resource
    public fun exists(addr: address): bool {
        exists<Counter>(addr)
    }
}
}
```

## Annotating functions with `acquires`

In the `counter` example, you might have noticed that the `get_count`, `increment`, `reset`, and `delete` functions are annotated with `acquires Counter`. A Move function `m::f` must be annotated with `acquires T` if and only if:

- The body of `m::f` contains a `move_from<T>`, `borrow_global_mut<T>`, or `borrow_global<T>` instruction, or
- The body of `m::f` invokes a function `m::g` declared in the same module that is annotated with `acquires`

For example, the following function inside `Counter` would need an `acquires` annotation:

## 使用获取注释函数
在 counter 示例中，您可能已经注意到 get_count、increment、reset 和 delete 函数都使用 acquire Counter 进行注释。移动函数 m::f 必须用获取 T 注释当且仅当：

- m::f 的主体包含 move_from T 、 borrow_global_mut T 或 borrow_global T 指令，或
- m::f 的主体调用在同一个模块中声明的函数 m::g
例如，Counter 中的以下函数需要一个获取注解：

```move
// Needs `acquires` because `increment` is annotated with `acquires`
fun call_increment(addr: address): u64 acquires Counter {
    counter::increment(addr)
}
```

However, the same function *outside* `Counter` would not need an annotation:

但是，Counter 之外的相同函数不需要注释：

```move
address 0x43 {
module m {
   use 0x42::counter;

   // Ok. Only need annotation when resource acquired by callee is declared
   // in the same module
   fun call_increment(addr: address): u64 {
       counter::increment(addr)
   }
}
}
```

If a function touches multiple resources, it needs multiple `acquires`:

如果一个函数涉及多个资源，它需要多次获取：

```move=
address 0x42 {
module two_resources {
    struct R1 has key { f: u64 }
    struct R2 has key { g: u64 }

    fun double_acquires(a: address): u64 acquires R1, R2 {
        borrow_global<R1>(a).f + borrow_global<R2>.g
    }
}
}
```

The `acquires` annotation does not take generic type parameters into account:

获取注解不考虑泛型类型参数：

```move=
address 0x42 {
module m {
    struct R<T> has key { t: T }

    // `acquires R`, not `acquires R<T>`
    fun acquire_generic_resource<T: store>(a: addr) acquires R {
        let _ = borrow_global<R<T>>(a);
    }

    // `acquires R`, not `acquires R<u64>
    fun acquire_instantiated_generic_resource(a: addr) acquires R {
        let _ = borrow_global<R<u64>>(a);
    }
}
}
```

Finally: redundant `acquires` are not allowed. Adding this function inside `Counter` will result in a compilation error:

最后：不允许冗余获取。在 Counter 中添加这个函数会导致编译错误：

```move
// This code will not compile because the body of the function does not use a global
// storage instruction or invoke a function with `acquires`
fun redundant_acquires_bad() acquires Counter {}
```

For more information on `acquires`, see [Move functions](./functions.md).

有关获取的更多信息，请参阅移动函数。

## Reference Safety For Global Resources

Move prohibits returning global references and requires the `acquires` annotation to prevent dangling references. This allows Move to live up to its promise of static reference safety (i.e., no dangling references, no `null` or `nil` dereferences) for all [reference](./references.md) types.

This example illustrates how the Move type system uses `acquires` to prevent a dangling reference:

## 全局资源的引用安全
Move 禁止返回全局引用，并要求获取注解以防止悬空引用。这允许 Move 兑现其对所有引用类型的静态引用安全的承诺（即，没有悬空引用，没有 null 或 nil 取消引用）。

此示例说明了 Move 类型系统如何使用获取来防止悬空引用：

```move=
address 0x42 {
module dangling {
    struct T has key { f: u64 }

    fun borrow_then_remove_bad(a: address) acquires T {
        let t_ref: &mut T = borrow_global_mut<T>(a);
        let t = remove_t(a); // type system complains here
        // t_ref now dangling!
        let uh_oh = *&t_ref.f
    }

    fun remove_t(a: address): T acquires T {
        move_from<T>(a)
    }

}
}
```

In this code, line 6 acquires a reference to the `T` stored at address `a` in global storage. The callee `remove_t` then removes the value, which makes `t_ref` a dangling reference.

Fortunately, this cannot happen because the type system will reject this program. The `acquires` annotation on `remove_t` lets the type system know that line 7 is dangerous, without having to recheck or introspect the body of `remove_t` separately!

The restriction on returning global references prevents a similar, but even more insidious problem:

在此代码中，第 6 行获取对存储在全局存储中地址 a 处的 T 的引用。被调用者 remove_t 然后删除该值，这使 t_ref 成为悬空引用。

幸运的是，这不可能发生，因为类型系统会拒绝这个程序。 remove_t 上的 acquires 注释让类型系统知道第 7 行是危险的，而无需单独重新检查或反省 remove_t 的主体！

对返回全局引用的限制防止了类似但更隐蔽的问题：

```move=
address 0x42 {
module m1 {
    struct T has key {}

    public fun ret_t_ref(a: address): &T acquires T {
        borrow_global<T>(a) // error! type system complains here
    }

    public fun remove_t(a: address) acquires T {
        let T {} = move_from<T>(a);
    }
}

module m2 {
    fun borrow_then_remove_bad(a: address) {
        let t_ref = m1::ret_t_ref(a);
        let t = m1::remove_t(a); // t_ref now dangling!
    }
}
}
```

Line 16 acquires a reference to a global resource `m1::T`, then line 17 removes that same resource, which makes `t_ref` dangle. In this case, `acquires` annotations do not help us because the `borrow_then_remove_bad` function is outside of the `m1` module that declares `T` (recall that `acquires` annotations can only be used for resources declared in the current module). Instead, the type system avoids this problem by preventing the return of a global reference at line 6.

Fancier type systems that would allow returning global references without sacrificing reference safety are possible, and we may consider them in future iterations of Move. We chose the current design because it strikes a good balance between expressivity, annotation burden, and type system complexity.

第 16 行获取对全局资源 m1::T 的引用，然后第 17 行删除相同的资源，这使得 t_ref 悬空。在这种情况下，获取注解对我们没有帮助，因为 borrow_then_remove_bad 函数位于声明 T 的 m1 模块之外（回想一下，获取注解只能用于在当前模块中声明的资源）。相反，类型系统通过阻止在第 6 行返回全局引用来避免这个问题。

在不牺牲引用安全的情况下允许返回全局引用的更高级的类型系统是可能的，我们可能会在 Move 的未来迭代中考虑它们。我们选择了当前的设计，因为它在表现力、注释负担和类型系统复杂性之间取得了很好的平衡。
