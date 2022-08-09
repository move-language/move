# Introduction

Welcome to Move, a next generation language for secure, sandboxed, and formally verified programming. Its first use case is for the Diem blockchain, where Move provides the foundation for its implementation. Move allows developers to write programs that flexibly manage and transfer assets, while providing the security and protections against attacks on those assets. However, Move has been developed with use cases in mind outside a blockchain context as well.

Move takes its cue from [Rust](https://www.rust-lang.org/) by using resource types with move (hence the name) semantics as an explicit representation of digital assets, such as currency.

# 介绍

欢迎使用 Move，这是一种用于安全、沙盒和形式化验证的下一代编程语言。
它的第一个用例是 Diem 区块链，Move 为其实现提供了基础。
Move 允许开发人员编写灵活地管理和转移资产的程序，同时提供安全保护，防止那些对资产攻击的行为。
不仅如此，Move 也可用于区块链之外的开发场景。

Move 的诞生从 [Rust](https://www.rust-lang.org/) 中的*所有权（ownership）*机制汲取了灵感，通过使用具有*移动（move）*语义的资源类型作为数字资产（例如货币）的显示表示，Move 也因此而得名。

## Who is Move for?

Move was designed and created as a secure, verified, yet flexible programming language. The first use of Move is for the implementation of the Diem blockchain. That said, the language is still evolving. Move has the potential to be a language for other blockchains, and even non-blockchain use cases as well.

Given custom Move modules will not be supported at the [launch](https://diem.com/white-paper/#whats-next) of the Diem Payment Network (DPN), we are targeting an early Move Developer persona.

The early Move Developer is one with some programming experience, who wants to begin understanding the core programming language and see examples of its usage.

## Move 是为谁而准备的？

Move 被设计和创建为一种安全、经过验证且灵活的编程语言。
Move 的第一个用途是实现 Diem 区块链。也就是说，语言仍在不断发展。
Move 有可能成为其他区块链甚至非区块链用例的语言。

鉴于在 Diem 支付网络 (DPN) 启动时将不支持自定义 Move 模块，我们的目标是早期的 Move 开发人员。

早期的 Move 开发人员是具有一定编程经验的人，他们希望开始了解核心编程语言并查看其使用示例。

### Hobbyists

Understanding that the capability to create custom modules on the Diem Payment Network will not be available at launch, the hobbyist Move Developer is interested in learning the intricacies of the language. She will understand the basic syntax, the standard libraries available, and write example code that can be executed using the Move CLI. The Move Developer may even want to dig into understanding how the Move Virtual Machine executes the code she writes.

### 爱好者

了解在 Diem 支付网络上创建自定义模块的功能在发布时将不可用，爱好 Move 的开发人员有兴趣学习该语言的复杂性。
她将了解基本语法、可用的标准库，并编写可以使用 Move CLI 执行的示例代码。
Move 开发人员甚至可能想深入了解 Move 虚拟机如何执行她编写的代码。

### Core Contributor

Beyond a hobbyist wanting to stay ahead of the curve for the core programming language is someone who may want to [contribute](https://diem.com/en-US/cla-sign/) directly to Move. Whether this includes submitting language improvements or even, in the future, adding core modules available on the Diem Payment Network, the core contributor will understand Move at a deep level.

### 核心贡献者

除了想要在核心编程语言方面保持领先的业余爱好者之外，还有可能想要直接为 Move 做出贡献的人。
无论这包括提交语言改进，还是将来添加 Diem 支付网络上可用的核心模块，核心贡献者都将深入了解 Move。

### Who Move is currently not targeting

Currently, Move is not targeting developers who wish to create custom modules and contracts for use on the Diem Payment Network. We are also not targeting novice developers who expect a completely polished developer experience even in testing the language.

### Move 目前不适用于哪些人

目前，Move 不针对希望创建自定义模块和合约以在 Diem 支付网络上使用的开发人员。
我们也不针对那些期望在语言测试阶段就获得完美开发体验的新手开发者。

## Where Do I Start?

Begin with understanding [modules and scripts](./modules-and-scripts.md) and then work through the [Move Tutorial](./creating-coins.md).

## 我该从哪里开始呢？

从了解[模块和脚本](./modules-and-scripts.md)开始，然后完成[Move 教程](./creating-coins.md)。
