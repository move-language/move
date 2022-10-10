# Move Book 中文版

区块链技术的发展经历了两个阶段，比特币（BTC）开启了*区块链1.0时代*，以太坊（ETH）开启了*区块链2.0时代*。
以太坊的出现为区块链带来了*智能合约*这一关键技术，让区块链不只停留在记账这一单的目的，而是带来更多的应用拓展性。
遗憾的是，智能合约如同一把双刃剑，在带来众多丰富功能拓展的同时，也容易让智能合约开发者无意间引入不安全的代码，让链的资产受到威胁。

编写简单、安全、易部署的智能合约应该是*区块链3.0时代*应该关注的重点，**面向资源编程**的 [Move 语言](https://github.com/move-language/move)，无疑给这个问题提供了一个很好的解决方案。

本书是 [Move Book](https://move-language.github.io/move/) 的中文版。

## 快速开始

本书使用 [mdBook](https://rust-lang.github.io/mdBook/) 构建。

1. 使用 Cargo 安装 mdBook：

```shell
cargo install mdbook
cargo install mdbook-i18n
```

2. 下载

```shell
git clone https://github.com/move-language/move.git
```

3. 构建并预览

```shell
cd language/documentation/book
mdbook serve --open
```


# 参与贡献



## 如何参与

《Move Book 中文版》翻译工作还在持续进行中，如果你愿意贡献你的一份力量，欢迎提交 [pr](https://github.com/move-language/move) 或 issue。

在贡献之前请阅读[官方的贡献指南](https://github.com/move-language/move/blob/main/CONTRIBUTING.md)和《Move Book 中文版》的贡献指南(如下)。


<details>
<summary>《Move Book 中文版》的贡献指南</summary>

本翻译项目由 [*Move 中文社区（MoveCC）*](https://github.com/move-cc)[发起](https://github.com/move-language/move/issues/353)，并与 [MoveDAO 社区](https://github.com/move-dao)共同初步完善。
目前工作仍在进行中！

欢迎所有对 Move 感兴趣的朋友一起加入到《Move Book 中文版》的翻译工作中。

感谢您有兴趣为 **《Move Book 中文版》** 做出贡献！有很多方法可以做出贡献，我们感谢所有这些方式。

### 提交 PR 的 Commits 格式

```text
[move-book-zh] 关于这个 PR 的描述信息
```

### 文档规范

请参考：[中文技术文档的写作规范](https://github.com/ruanyf/document-style-guide)

#### 断句

本书使用 Markdown 作为源文件，使用 [mdBook](https://github.com/rust-lang/mdBook) 作为渲染引擎。

由于中英文有所区别，换行后渲染引擎会自动追加一个空格。为了优化视觉体验，中文一个段落内不必换行，保持中文的段内容为一个物理行。

英文是由空格分隔的文本，所以不存在上述问题。

#### 中英文混排规范

文中出现中英文混排时，中文与英文之间需要添加一个空格。如果英文单词结尾，此时单词与标点符号之间不添加空格。

#### 数字规范

数字之间需要添加一个空格，如果数字带有英文单位，那么数字与英文单位之间不能添加空格。如果数字后带有中文单位，需要添加一个空格。

```text
正确：如果一个 1s 的帧被划分为 10 个时隙，每个时隙为 100ms。

错误：如果一个1s的帧被划分为10个时隙，每个时隙为100ms。

错误：如果一个 1 s 的帧被划分为10个时隙，每个时隙为 100 ms。
```

#### 逗号问题

英文中，没有顿号这种标点符号，逗号常用分隔并列的句子成分或结构。

英文中，像 `and` 并列词的前面通常会有一个 `,`，但在中文里表示对象之间的并列关系时，`和`的前面不能带逗号。

```text
War, famine, and flood are terrible.
战争, 饥荒和洪水都是很可怕的。
```

#### 冒号和逗号的使用场景规范

冒号通常用在“问、答、说、指出、宣布、证明、表明、例如”一类动词后面，表示提起下文。
如果在较短的提示句子中，需要将 `：` 改为 `，`；如果提示内容比较多，则使用 `：` 来提起下文：

示例1，提起的内容短少：

```text
十六进制字符串是以 x 为前缀的带引号的字符串文字，例如x'48656C6C6F210A'
```

示例2，提起的内容多：

```text
在这些情况下，vector 的类型是从元素类型或从动态数组的使用上推断出来的。如果无法推断类型或者只是为了更清楚地表示，则可以显式指定类型：

vector<T>[]: vector<T>
vector<T>[e1, ..., en]: vector<T>
```

</details>



## 贡献人员 ✨

感谢以下小伙伴为中文版做出贡献 ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/Kusou1"><img src="https://avatars.githubusercontent.com/u/57334674?v=4?s=100" width="100px;" alt=""/><br /><sub><b>zhang</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=Kusou1" title="Code">💻</a> <a href="https://github.com/move-dao/move-book-zh/commits?author=Kusou1" title="Documentation">📖</a> <a href="#translation-Kusou1" title="Translation">🌍</a> <a href="#infra-Kusou1" title="Infrastructure (Hosting, Build-Tools, etc)">🚇</a></td>
    <td align="center"><a href="https://github.com/ruy1su"><img src="https://avatars.githubusercontent.com/u/9391802?v=4?s=100" width="100px;" alt=""/><br /><sub><b>ruyisu</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=ruy1su" title="Code">💻</a> <a href="#translation-ruy1su" title="Translation">🌍</a> <a href="https://github.com/move-dao/move-book-zh/commits?author=ruy1su" title="Documentation">📖</a> <a href="https://github.com/move-dao/move-book-zh/pulls?q=is%3Apr+reviewed-by%3Aruy1su" title="Reviewed Pull Requests">🚇</a></td>
    <td align="center"><a href="https://github.com/lshoo"><img src="https://avatars.githubusercontent.com/u/670440?v=4?s=100" width="100px;" alt=""/><br /><sub><b>lshoo</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=lshoo" title="Code">💻</a> <a href="#translation-lshoo" title="Translation">🌍</a> <a href="https://github.com/move-dao/move-book-zh/commits?author=lshoo" title="Documentation">📖</a> <a href="#ideas-lshoo" title="Ideas, Planning, & Feedback">🤔</a> <a href="https://github.com/move-dao/move-book-zh/pulls?q=is%3Apr+reviewed-by%3Alshoo" title="Reviewed Pull Requests">👀</a></td>
    <td align="center"><a href="https://github.com/Container-00"><img src="https://avatars.githubusercontent.com/u/61052480?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Container</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=Container-00" title="Code">💻</a> <a href="#translation-Container-00" title="Translation">🌍</a> <a href="https://github.com/move-dao/move-book-zh/commits?author=Container-00" title="Documentation">📖</a> <a href="https://github.com/move-dao/move-book-zh/pulls?q=is%3Apr+reviewed-by%3AContainer-00" title="Reviewed Pull Requests">👀</a></td>
    <td align="center"><a href="https://github.com/nosalt99"><img src="https://avatars.githubusercontent.com/u/22558493?v=4?s=100" width="100px;" alt=""/><br /><sub><b>nosalt</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=nosalt99" title="Code">💻</a> <a href="#translation-nosalt99" title="Translation">🌍</a> <a href="https://github.com/move-dao/move-book-zh/commits?author=nosalt99" title="Documentation">📖</a> <a href="#infra-nosalt99" title="Infrastructure (Hosting, Build-Tools, etc)">🚇</a></td>
    <td align="center"><a href="https://github.com/geometryolife"><img src="https://avatars.githubusercontent.com/u/54882546?v=4?s=100" width="100px;" alt=""/><br /><sub><b>geometryolife</b></sub></a><br /><a href="https://github.com/move-language/move/commits?author=geometryolife" title="Code">💻</a> <a href="#translation-geometryolife" title="Translation">🌍</a> <a href="#talk-geometryolife" title="Talks">📢</a></td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/666thi"><img src="https://avatars.githubusercontent.com/u/109965699?v=4?s=100" width="100px;" alt=""/><br /><sub><b>666thi</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=666thi" title="Code">💻</a> <a href="#translation-666thi" title="Translation">🌍</a> <a href="#talk-666thi" title="Talks">📢</a></td>
    <td align="center"><a href="https://github.com/MagicGordon"><img src="https://avatars.githubusercontent.com/u/19465870?v=4?s=100" width="100px;" alt=""/><br /><sub><b>MagicGordon</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=MagicGordon" title="Code">💻</a> <a href="#translation-MagicGordon" title="Translation">🌍</a> <a href="#talk-MagicGordon" title="Talks">📢</a></td>
    <td align="center"><a href="https://github.com/xixifusi1984"><img src="https://avatars.githubusercontent.com/u/39210551?v=4?s=100" width="100px;" alt=""/><br /><sub><b>xixifusi1984</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=xixifusi1984" title="Code">💻</a> <a href="#translation-xixifusi1984" title="Translation">🌍</a> <a href="#talk-xixifusi1984" title="Talks">📢</a></td>
    <td align="center"><a href="https://github.com/yvvw"><img src="https://avatars.githubusercontent.com/u/15168529?v=4?s=100" width="100px;" alt=""/><br /><sub><b>yvvw</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=yvvw" title="Code">💻</a> <a href="#translation-yvvw" title="Translation">🌍</a> <a href="#talk-yvvw" title="Talks">📢</a></td>
    <td align="center"><a href="https://github.com/xiaochuan891102"><img src="https://avatars.githubusercontent.com/u/109952533?v=4?s=100" width="100px;" alt=""/><br /><sub><b>xiaochuan891102</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=xiaochuan891102" title="Code">💻</a> <a href="#translation-xiaochuan891102" title="Translation">🌍</a> <a href="#talk-xiaochuan891102" title="Talks">📢</a></td>
    <td align="center"><a href="https://github.com/stephenLee"><img src="https://avatars.githubusercontent.com/u/1144508?v=4?s=100" width="100px;" alt=""/><br /><sub><b>stephenLee</b></sub></a><br /><a href="https://github.com/move-dao/move-book-zh/commits?author=stephenLee" title="Code">💻</a> <a href="#translation-stephenLee" title="Translation">🌍</a> <a href="#talk-stephenLee" title="Talks">📢</a></td>
  </tr>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->



<details>
<summary>各章节译者</summary>

|    | 章节                       | 译者                                           | 校对                                                                                |
|----|----------------------------|------------------------------------------------|-------------------------------------------------------------------------------------|
| 0  | Intoduction                | Tom                                            | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 1  | Modules and Scripts        | [@Kusou1](https://github.com/kusou1)           | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 2  | Move Tutorial              | loadi、[@leego](https://github.com/leego)      | [@Joe Chen](https://github.com/geometryolife)                                       |
| 3  | Integers                   | Tom                                            | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 4  | Bool                       | Tom                                            | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 5  | Address                    | ([@stephenLee](https://github.com/stephenLee)) | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 6  | Vector                     | ([@stephenLee](https://github.com/stephenLee)) | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 7  | Signer                     | ([@stephenLee](https://github.com/stephenLee)) | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 8  | References                 | container                                      | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 9  | Tuples and Unit            | container                                      | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 10 | Local Variables and Scopes | @ruyisu                                        | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 11 | Equality                   | @ruyisu                                        | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 12 | Abort and Assert           | @ruyisu                                        | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 13 | Conditionals               | [@Kusou1](https://github.com/kusou1)           | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 14 | While and Loop             | [@Kusou1](https://github.com/kusou1)           | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 15 | Functions                  | @nosalt99                                      | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 16 | Structs and Resource       | @nosalt99                                      | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 17 | Constants                  | @nosalt99                                      | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 18 | Generics                   | 小川                                           | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 19 | Type Abilities             | 小川                                           | [@lshoo](https://github.com/lshoo)、[@Joe Chen](https://github.com/geometryolife)   |
| 20 | Uses and Aliases           | 小川                                           | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 21 | Friends                    | @xiaochuan891102                               | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 22 | Packages                   | @xiaochuan891102                               | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 23 | Unit Test                  | [@yvvw](https://github.com/yvvw)               | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 24 | Global Storage Structure   | [@yvvw](https://github.com/yvvw)               | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 25 | Global Storage Operators   | [@yvvw](https://github.com/yvvw)               | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 26 | Standard Library           | @MagicGordon                                   | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |
| 27 | Coding Conventions         | @MagicGordon                                   | [@ruyisu](https://github.com/ruy1su)、[@Joe Chen](https://github.com/geometryolife) |

</details>
