# Signer

`signer` is a built-in Move resource type. A `signer` is a
[capability](https://en.wikipedia.org/wiki/Object-capability_model) that allows the holder to act on
behalf of a particular `address`. You can think of the native implementation as being:

```move
struct signer has drop { a: address }
```

A `signer` is somewhat similar to a Unix [UID](https://en.wikipedia.org/wiki/User_identifier) in
that it represents a user authenticated by code _outside_ of Move (e.g., by checking a cryptographic
signature or password).

## Comparison to `address`

A Move program can create any `address` value without special permission using address literals:

```move
let a1 = @0x1;
let a2 = @0x2;
// ... and so on for every other possible address
```

However, `signer` values are special because they cannot be created via literals or
instructions--only by the Move VM. Before the VM runs a script with parameters of type `signer`, it
will automatically create `signer` values and pass them into the script:

```move
script {
    use std::signer;
    fun main(s: signer) {
        assert!(signer::address_of(&s) == @0x42, 0);
    }
}
```

This script will abort with code `0` if the script is sent from any address other than `0x42`.

A transaction script can have an arbitrary number of `signer`s as long as the `signer`s are a prefix
to any other arguments. In other words, all of the `signer` arguments must come first:

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

## `signer` Operators

The `std::signer` standard library module provides two utility functions over `signer` values:

| Function                                    | Description                                                    |
| ------------------------------------------- | -------------------------------------------------------------  |
| `signer::address_of(&signer): address`      | Return the `address` wrapped by this `&signer`.                |
| `signer::borrow_address(&signer): &address` | Return a reference to the `address` wrapped by this `&signer`. |

In addition, the `move_to<T>(&signer, T)` [global storage operator](./global-storage-operators.md)
requires a `&signer` argument to publish a resource `T` under `signer.address`'s account. This
ensures that only an authenticated user can elect to publish a resource under their `address`.

## Ownership

Unlike simple scalar values, `signer` values are not copyable, meaning they cannot be copied (from
any operation whether it be through an explicit [`copy`](./variables.md#move-and-copy) instruction
or through a [dereference `*`](./references.md#reading-and-writing-through-references).
