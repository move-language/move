# はじめに(Introduction)

Welcome to Move, a next generation language for secure, sandboxed, and formally verified programming. Its first use case is for the Diem blockchain, where Move provides the foundation for its implementation. Move allows developers to write programs that flexibly manage and transfer assets, while providing the security and protections against attacks on those assets. However, Move has been developed with use cases in mind outside a blockchain context as well.

セキュアでサンドボックス化され、形式的に検証されたプログラミングのための次世代言語、Moveへようこそ。Moveは、その最初のユースケースであるDiemの実装基盤を提供しています。Moveを使えば、開発者は資産に対する攻撃に対するセキュリティとプロテクションを実現しながらそれを柔軟に管理・移転するプログラムを書くことができます。しかし、Moveはブロックチェーン以外のユースケースも想定して開発されています。

Move takes its cue from [Rust](https://www.rust-lang.org/) by using resource types with move (hence the name) semantics as an explicit representation of digital assets, such as currency.

Moveは[Rust](https://www.rust-lang.org/)からヒントを得て、通貨などのデジタル資産を明示的に表現するためのmove（ゆえに、この名前）セマンティクスを持つリソースタイプを利用しています。

## Moveは誰のためのもの? (Who is Move for?)

Move was designed and created as a secure, verified, yet flexible programming language. The first use of Move is for the implementation of the Diem blockchain. That said, the language is still evolving. Move has the potential to be a language for other blockchains, and even non-blockchain use cases as well.

Moveは安全かつ検証済みで、かつ柔軟なプログラミング言語として設計され、作成されました。その最初の用途はDiemブロックチェーンの実装です。とはいえ、この言語はいまだ進化を続けています。Moveは他のブロックチェーン、さらにブロックチェーン以外のユースケースにも対応する言語になる可能性を持っています。

Given custom Move modules will not be supported at the [launch](https://diem.com/white-paper/#whats-next) of the Diem Payment Network (DPN), we are targeting an early Move Developer persona.

Diem Payment Network(DPN)の[ローンチ](https://diem.com/white-paper/#whats-next)ではカスタムMove Moduleはサポートされないため、初期のMove開発者というペルソナをターゲットとしています。

The early Move Developer is one with some programming experience, who wants to begin understanding the core programming language and see examples of its usage.

初期のMove開発者とは、プログラミングの経験がありコアとなるプログラミング言語を理解し始めそしてそれの利用例を見てみたいと思っている人々です。

### Hobbyists

Understanding that the capability to create custom modules on the Diem Payment Network will not be available at launch, the hobbyist Move Developer is interested in learning the intricacies of the language. She will understand the basic syntax, the standard libraries available, and write example code that can be executed using the Move CLI. The Move Developer may even want to dig into understanding how the Move Virtual Machine executes the code she writes.

### Core Contributor

Beyond a hobbyist wanting to stay ahead of the curve for the core programming language is someone who may want to [contribute](https://diem.com/en-US/cla-sign/) directly to Move. Whether this includes submitting language improvements or even, in the future, adding core modules available on the Diem Payment Network, the core contributor will understand Move at a deep level.

### Who Move is currently not targeting

Currently, Move is not targeting developers who wish to create custom modules and contracts for use on the Diem Payment Network. We are also not targeting novice developers who expect a completely polished developer experience even in testing the language.

## Where Do I Start?

Begin with understanding [modules and scripts](./modules-and-scripts.md) and then work through the [Move Tutorial](./creating-coins.md).
