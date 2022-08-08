# Friends

The `friend` syntax is used to declare modules that are trusted by the current module.
A trusted module is allowed to call any function defined in the current module that have the `public(friend)` visibility.
For details on function visibilities, please refer to the *Visibility* section in [Functions](./functions.md).

# 友元

友元语法用于声明当前模块信任的模块。允许受信任的模块调用当前模块中定义的任何具有公共（朋友）可见性的函数。有关函数可见性的详细信息，请参阅函数中的可见性部分。

## Friend declaration

A module can declare other modules as friends via friend declaration statements, in the format of

- `friend <address::name>` — friend declaration using fully qualified module name like the example below, or
## 友元声明
一个模块可以通过友元声明语句将其他模块声明为友元，格式为

- `friend <address::name>` — 使用完全限定模块名称的朋友声明，如下例所示，或

  ```move
  address 0x42 {
  module a {
      friend 0x42::b;
  }
  }
  ```

- `friend <module-name-alias>` — friend declaration using a module name alias, where the module alias is introduced via the `use` statement.

- `friend <module-name-alias>`——使用模块名称别名的朋友声明，其中模块别名是通过 use 语句引入的。

  ```move
  address 0x42 {
  module a {
      use 0x42::b;
      friend b;
  }
  }
  ```

A module may have multiple friend declarations, and the union of all the friend modules forms the friend list.
In the example below, both `0x42::B` and `0x42::C` are considered as friends of `0x42::A`.

一个模块可能有多个好友声明，所有好友模块的并集形成好友列表。在下面的示例中，0x42::B 和 0x42::C 都被视为 0x42::A 的朋友。

```move
address 0x42 {
module a {
    friend 0x42::b;
    friend 0x42::c;
}
}
```

Unlike `use` statements, `friend` can only be declared in the module scope and not in the expression block scope.
`friend` declarations may be located anywhere a top-level construct (e.g., `use`, `function`, `struct`, etc.) is allowed.
However, for readability, it is advised to place friend declarations near the beginning of the module definition.

Note that the concept of friendship does not apply to Move scripts:
- A Move script cannot declare `friend` modules as doing so is considered meaningless: there is no mechanism to call the function defined in a script.
- A Move module cannot declare `friend` scripts as well because scripts are ephemeral code snippets that are never published to global storage.

与 use 语句不同，friend 只能在模块范围内声明，而不能在表达式块范围内声明。友元声明可以位于允许顶级构造（例如，使用、函数、结构等）的任何地方。但是，为了可读性，建议将友元声明放在模块定义的开头附近。

请注意，友谊的概念不适用于 Move 脚本：

- Move 脚本不能声明友元模块，因为这样做被认为是没有意义的：没有调用脚本中定义的函数的机制。
- Move 模块也不能声明友元脚本，因为脚本是临时代码片段，从未发布到全局存储。

### Friend declaration rules
Friend declarations are subject to the following rules:

- A module cannot declare itself as a friend.

### 好友声明规则
朋友声明须遵守以下规则：

- 模块不能将自己声明为友元。

  ```move=
  address 0x42 {
  module m { friend Self; // ERROR! }
  //                ^^^^ Cannot declare the module itself as a friend
  }

  address 0x43 {
  module m { friend 0x43::M; // ERROR! }
  //                ^^^^^^^ Cannot declare the module itself as a friend
  }
  ```

- Friend modules must be known by the compiler
- 编译器必须知道友元模块

  ```move=
  address 0x42 {
  module m { friend 0x42::nonexistent; // ERROR! }
  //                ^^^^^^^^^^^^^^^^^ Unbound module '0x42::nonexistent'
  }
  ```

- Friend modules must be within the same account address. (Note: this is not a technical requirement but rather a policy decision which *may* be relaxed later.)
- 好友模块必须在同一个账户地址内。 （注：这不是技术要求，而是以后可能放宽的政策决定。）

  ```move=
  address 0x42 {
  module m {}
  }

  address 0x43 {
  module n { friend 0x42::m; // ERROR! }
  //                ^^^^^^^ Cannot declare modules out of the current address as a friend
  }
  ```

- Friends relationships cannot create cyclic module dependencies.

  Cycles are not allowed in the friend relationships, e.g., the relation `0x2::a` friends `0x2::b` friends `0x2::c` friends `0x2::a` is not allowed.
More generally, declaring a friend module adds a dependency upon the current module to the friend module (because the purpose is for the friend to call functions in the current module).
If that friend module is already used, either directly or transitively, a cycle of dependencies would be created.
- 朋友关系不能创建循环模块依赖关系。

朋友关系中不允许循环，例如，关系 0x2::a 朋友 0x2::b 朋友 0x2::c 朋友 0x2::a 是不允许的。更一般地，声明一个友元模块会将对当前模块的依赖添加到友元模块（因为目的是让友元调用当前模块中的函数）。如果该友元模块已被直接或传递地使用，则将创建一个依赖循环。

  ```move=
  address 0x2 {
  module a {
      use 0x2::c;
      friend 0x2::b;

      public fun a() {
          c::c()
      }
  }

  module b {
      friend 0x2::c; // ERROR!
  //         ^^^^^^ This friend relationship creates a dependency cycle: '0x2::b' is a friend of '0x2::a' uses '0x2::c' is a friend of '0x2::b'
  }

  module c {
      public fun c() {}
  }
  }
  ```

- The friend list for a module cannot contain duplicates.
- 模块的好友列表不能包含重复项。

  ```move=
  address 0x42 {
  module a {}

  module m {
      use 0x42::a as aliased_a;
      friend 0x42::A;
      friend aliased_a; // ERROR!
  //         ^^^^^^^^^ Duplicate friend declaration '0x42::a'. Friend declarations in a module must be unique
  }
  }
  ```
