# 签名者（Signer）

`signer` is a built-in Move resource type. A `signer` is a [capability](https://en.wikipedia.org/wiki/Object-capability_model) that allows the holder to act on behalf of a particular `address`.
You can think of the native implementation as being:

`签名者（signer）`是 Move 内置的资源类型。`签名者（signer）`是一种允许持有者代表特定`地址（address）`行使权力的[能力（capability）](https://en.wikipedia.org/wiki/Object-capability_model)。你可以将原生实现（native implementation）视为：

```move
struct signer has drop { a: address }
```

A `signer` is somewhat similar to a Unix [UID](https://en.wikipedia.org/wiki/User_identifier) in
that it represents a user authenticated by code _outside_ of Move (e.g., by checking a cryptographic
signature or password).

`signer` 有点像 Unix [UID](https://en.wikipedia.org/wiki/User_identifier)，因为它表示一个通过 Move *之外*的代码（例如，通过检查加密签名或密码）进行身份验证的用户。

## 与 `address` 的比较（Comparison to `address`）

A Move program can create any `address` value without special permission using address literals:

Move 程序可以使用地址字面量（literal）创建任何`地址（address）`值，而无需特殊许可：

```move
let a1 = @0x1;
let a2 = @0x2;
// ... 等等，所有其他可能的地址
```

However, `signer` values are special because they cannot be created via literals or
instructions--only by the Move VM. Before the VM runs a script with parameters of type `signer`, it
will automatically create `signer` values and pass them into the script:

但是，`signer` 值是特殊的，因为它们不能通过字面量或者指令创建 —— 只能通过 Move 虚拟机（VM）创建。在虚拟机运行带有 `signer` 类型参数的脚本之前，它会自动创建 `signer` 值并将它们传递给脚本：

```move
script {
    use std::signer;
    fun main(s: signer) {
        assert!(signer::address_of(&s) == @0x42, 0);
    }
}
```

This script will abort with code `0` if the script is sent from any address other than `0x42`.

A transaction script can have an arbitrary number of `signer`s as long as the signers are a prefix
to any other arguments. In other words, all of the signer arguments must come first:

如果脚本是从 `0x42` 以外的任何地址发送的，则此脚本将中止并返回代码 `0`。

交易脚本可以有任意数量的 `signer`，只要 `signer` 参数排在其他参数前面。换句话说，所有 `signer` 参数都必须放在第一位。

```move
script {
    use std::signer;
    fun main(s1: signer, s2: signer, x: u64, y: u8) {
        // ...
    }
}
```

This is useful for implementing _multi-signer scripts_ that atomically act with the authority of
multiple parties. For example, an extension of the script above could perform an atomic currency
swap between `s1` and `s2`.

这对于实现具有多方权限原子行为的*多重签名脚本（multi-signer scripts）*很有用。例如，上述脚本的扩展可以在 `s1` 和 `s2` 之间执行原子货币交换。

## `signer` 操作符（`signer` Operators）

The `std::signer` standard library module provides two utility functions over `signer` values:

`std::signer` 标准库模块为 `signer` 提供了两个实用函数：

| 函数                                        | 描述                                                          |
| ------------------------------------------- | ------------------------------------------------------------- |
| `signer::address_of(&signer): address`      | 返回由 `&signer` 包装的地址值。                               |
| `signer::borrow_address(&signer): &address` | 返回由 `&signer` 包装的地址的引用。                           |

In addition, the `move_to<T>(&signer, T)` [global storage operator](./global-storage-operators.md)
requires a `&signer` argument to publish a resource `T` under `signer.address`'s account. This
ensures that only an authenticated user can elect to publish a resource under their `address`.

此外，`move_to<T>(&signer, T)` [全局存储](./global-storage-operators.md)操作符需要一个 `&signer` 参数在 `signer.address` 的帐户下发布资源 `T`。这确保了只有经过身份验证的用户才能在其地址下发布资源。

## 所有权（Ownership）

Unlike simple scalar values, `signer` values are not copyable, meaning they cannot be copied (from
any operation whether it be through an explicit [`copy`](./variables.md#move-and-copy) instruction
or through a [dereference `*`](./references.md#reference-operators)).

与简单的标量值不同，`signer` 值是不可复制的，这意味着他们不能被复制（通过任何操作，无论是通过显式 [`copy`](./variables.md#移动和复制move-and-copy)指令还是通过[解引用（dereference）`*`](./references.md#reference-operators)）。
