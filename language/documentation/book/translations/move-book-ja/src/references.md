# References

Move has two types of references: immutable `&` and mutable `&mut`. Immutable references are read
only, and cannot modify the underlying value (or any of its fields). Mutable references allow for
modifications via a write through that reference. Move's type system enforces an ownership
discipline that prevents reference errors.

For more details on the rules of references, see [Structs and Resources](./structs-and-resources.md)

## Reference Operators

Move provides operators for creating and extending references as well as converting a mutable
reference to an immutable one. Here and elsewhere, we use the notation `e: T` for "expression `e`
has type `T`".

| Syntax      | Type                                                  | Description                                                    |
| ----------- | ----------------------------------------------------- | -------------------------------------------------------------- |
| `&e`        | `&T` where `e: T` and `T` is a non-reference type     | Create an immutable reference to `e`                           |
| `&mut e`    | `&mut T` where `e: T` and `T` is a non-reference type | Create a mutable reference to `e`.                             |
| `&e.f`      | `&T` where `e.f: T`                                   | Create an immutable reference to field `f` of struct `e`.      |
| `&mut e.f`  | `&mut T` where `e.f: T`                               | Create a mutable reference to field `f` of struct`e`.          |
| `freeze(e)` | `&T` where `e: &mut T`                                | Convert the mutable reference `e` into an immutable reference. |

The `&e.f` and `&mut e.f` operators can be used both to create a new reference into a struct or to
extend an existing reference:

```move
let s = S { f: 10 };
let f_ref1: &u64 = &s.f; // works
let s_ref: &S = &s;
let f_ref2: &u64 = &s_ref.f // also works
```

A reference expression with multiple fields works as long as both structs are in the same module:

```move
struct A { b: B }
struct B { c : u64 }
fun f(a: &A): &u64 {
  &a.b.c
}
```

Finally, note that references to references are not allowed:

```move
let x = 7;
let y: &u64 = &x;
let z: &&u64 = &y; // will not compile
```

## Reading and Writing Through References

Both mutable and immutable references can be read to produce a copy of the referenced value.

Only mutable references can be written. A write `*x = v` discards the value previously stored in `x`
and updates it with `v`.

Both operations use the C-like `*` syntax. However, note that a read is an expression, whereas a
write is a mutation that must occur on the left hand side of an equals.

| Syntax     | Type                                | Description                         |
| ---------- | ----------------------------------- | ----------------------------------- |
| `*e`       | `T` where `e` is `&T` or `&mut T`   | Read the value pointed to by `e`    |
| `*e1 = e2` | `()` where `e1: &mut T` and `e2: T` | Update the value in `e1` with `e2`. |

In order for a reference to be read, the underlying type must have the
[`copy` ability](./abilities.md) as reading the reference creates a new copy of the value. This rule
prevents the copying of resource values:

```move=
fun copy_resource_via_ref_bad(c: Coin) {
    let c_ref = &c;
    let counterfeit: Coin = *c_ref; // not allowed!
    pay(c);
    pay(counterfeit);
}
```

Dually: in order for a reference to be written to, the underlying type must have the
[`drop` ability](./abilities.md) as writing to the reference will discard (or "drop") the old value.
This rule prevents the destruction of resource values:

```move=
fun destroy_resource_via_ref_bad(ten_coins: Coin, c: Coin) {
    let ref = &mut ten_coins;
    *ref = c; // not allowed--would destroy 10 coins!
}
```

## `freeze` inference

A mutable reference can be used in a context where an immutable reference is expected:

```move
let x = 7;
let y: &u64 = &mut x;
```

This works because the under the hood, the compiler inserts `freeze` instructions where they are
needed. Here are a few more examples of `freeze` inference in action:

```move=
fun takes_immut_returns_immut(x: &u64): &u64 { x }

// freeze inference on return value
fun takes_mut_returns_immut(x: &mut u64): &u64 { x }

fun expression_examples() {
    let x = 0;
    let y = 0;
    takes_immut_returns_immut(&x); // no inference
    takes_immut_returns_immut(&mut x); // inferred freeze(&mut x)
    takes_mut_returns_immut(&mut x); // no inference

    assert!(&x == &mut y, 42); // inferred freeze(&mut y)
}

fun assignment_examples() {
    let x = 0;
    let y = 0;
    let imm_ref: &u64 = &x;

    imm_ref = &x; // no inference
    imm_ref = &mut y; // inferred freeze(&mut y)
}
```

### Subtyping

With this `freeze` inference, the Move type checker can view `&mut T` as a subtype of `&T`. As shown
above, this means that anywhere for any expression where a `&T` value is used, a `&mut T` value can
also be used. This terminology is used in error messages to concisely indicate that a `&mut T` was
needed where a `&T` was supplied. For example

```move=
address 0x42 {
module example {
    fun read_and_assign(store: &mut u64, new_value: &u64) {
        *store = *new_value
    }

    fun subtype_examples() {
        let x: &u64 = &0;
        let y: &mut u64 = &mut 1;

        x = &mut 1; // valid
        y = &2; // invalid!

        read_and_assign(y, x); // valid
        read_and_assign(x, y); // invalid!
    }
}
}
```

will yield the following error messages

```text
error:

    ┌── example.move:12:9 ───
    │
 12 │         y = &2; // invalid!
    │         ^ Invalid assignment to local 'y'
    ·
 12 │         y = &2; // invalid!
    │             -- The type: '&{integer}'
    ·
  9 │         let y: &mut u64 = &mut 1;
    │                -------- Is not a subtype of: '&mut u64'
    │

error:

    ┌── example.move:15:9 ───
    │
 15 │         read_and_assign(x, y); // invalid!
    │         ^^^^^^^^^^^^^^^^^^^^^ Invalid call of '0x42::example::read_and_assign'. Invalid argument for parameter 'store'
    ·
  8 │         let x: &u64 = &0;
    │                ---- The type: '&u64'
    ·
  3 │     fun read_and_assign(store: &mut u64, new_value: &u64) {
    │                                -------- Is not a subtype of: '&mut u64'
    │
```

The only other types currently that has subtyping are [tuples](./tuples.md)

## Ownership

Both mutable and immutable references can always be copied and extended _even if there are existing
copies or extensions of the same reference_:

```move
fun reference_copies(s: &mut S) {
  let s_copy1 = s; // ok
  let s_extension = &mut s.f; // also ok
  let s_copy2 = s; // still ok
  ...
}
```

This might be surprising for programmers familiar with Rust's ownership system, which would reject
the code above. Move's type system is more permissive in its treatment of
[copies](./variables.md#move-and-copy), but equally strict in ensuring unique ownership of mutable
references before writes.

### References Cannot Be Stored

References and tuples are the _only_ types that cannot be stored as a field value of structs, which
also means that they cannot exist in global storage. All references created during program execution
will be destroyed when a Move program terminates; they are entirely ephemeral. This invariant is
also true for values of types without the `store` [ability](./abilities.md), but note that
references and tuples go a step further by never being allowed in structs in the first place.

This is another difference between Move and Rust, which allows references to be stored inside of
structs.

Currently, Move cannot support this because references cannot be
[serialized](https://en.wikipedia.org/wiki/Serialization), but _every Move value must be
serializable_. This requirement comes from Move's
[persistent global storage](./global-storage-structure.md), which needs to serialize values to
persist them across program executions. Structs can be written to global storage, and thus they must
be serializable.

One could imagine a fancier, more expressive, type system that would allow references to be stored
in structs _and_ ban those structs from existing in global storage. We could perhaps allow
references inside of structs that do not have the `store` [ability](./abilities.md), but that would
not completely solve the problem: Move has a fairly complex system for tracking static reference
safety, and this aspect of the type system would also have to be extended to support storing
references inside of structs. In short, Move's type system (particularly the aspects around
reference safety) would have to expand to support stored references. But it is something we are
keeping an eye on as the language evolves.
