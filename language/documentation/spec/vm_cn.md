# MoveVM Specification

**Move VM** 的实例化其实就是初始化一个 *Loader* 实例，而初始化的 *Loader* 实际上就是一小组空白的 `Tables`（其背后是通过 `Mutex` 保护的少量几个 `HashMap` 和 `Vec`）。因此，**VM** 的初始化相当便宜。*Loader* 本质上就是一个 `Code Cacher`，其拥有 **VM** 等长的生命周期。当 `functions` 和 `scripts` 执行的时候，代码首先会在 *runtime* 中得以加载。一旦加载之后，`modules` 和 `scripts` 就会做好立即执行的准备，且可以以加载的形式重新使用。加载代码是昂贵的，且 **VM** 执行急迫（`Eager`）加载。当执行开始后，就不会再加载了。也就是说，所有（可能通过任意控制流）的代码都会在加载阶段就缓存好以备执行。也许更重要的是，该 `Eager`模型确保了在运行时不会有链接错误，并且给定的调用是不会因为代码路径的不同而出现加载或链接错误的。该调用的一致性在执行之前就得以保证。很明显，运行时错误是仍然可能且可“预期的”。

上述模型非常适合典型的区块链需求：

* 仅仅使用有限的几个在创世时就发布的函数，就足以验证。一旦加载，代码就总是可以从缓存中取出并立即可用；
* 执行是在给定数据视图的上下文中进行的，该数据视图稳定且不可变。由于这些代码也是稳定的，因此优化上述加载的过程是很重要的。此外，交易的结构也是相当类似的，且代码的重用也可以显著的提升性能和稳定性；

**Move VM**有一个数据缓存的内部实现以减轻客户端的重任（数据缓存一致性）。该抽象位于 *Session* 之后，`Session`是和 *Runtime* 交流的唯一方式。

*Session* 的目标是创建和管理一组对 **VM** 中的调用的数据缓存。它还旨在以适合适配器的格式返回副作用。*Session* 将调用转发到 *Runtime* ，*Runtime* 是 **VM** 逻辑和实现所存在和启动的地方。

### Code Cache

当首次加载 *Module* 时，**VM** 会在数据存储中查询该 *Module*。其二进制数据被 *Loader* 反序列化，验证，加载，缓存。一旦加载后，该 *Module* 在该 **VM** 实例的生命周期内便永远不会再请求。在该系统中，代码是不可变资源。

`加载` 过程可以被概括为以下步骤：

1. 从数据存储中取得以序列化形式表示的二进制 *Module*（`Vec<u8>`），这里可能需要网络访问；
2. 对该二进制数据进行反序列化和验证；
3. 加载该 *Module* 的依赖项 （对每一个依赖项重复步骤1~4）；
4. 将这些依赖项链接至该 *Module*（会转换成适合 *Runtime* 的表示形式）并由 *Loader* 进行缓存。

因此，对于已加载 *Module* 的引用，是不会执行任何的网络获取，验证，或者转换成 *Runtime* 中的结构等操作（比如 `linking`）。

在一个典型的客户端中，代码缓存的一致性可能会因执行一个系统级交易这样的硬性升级而被破坏，因此它需要适配器在重新启动之前停止处理交易。其它客户端也可能拥有不同的 `代码模型` （比如不同的版本形式）。

总之，持有 **Move VM** 实例的客户端必须要留意代码缓存的行为，并提供与已加载代码兼容的数据视图（`DataStore`）。此外，当特定的条件可能影响代码缓存的一致性时，客户端会负责释放并实例化一个新的 **VM**。

### Module Publishing

客户端可以在系统中通过如下调用来发布 *Module*：

```rust
pub fn publish_module(
	&mut self,
	module: Vec<u8>,
	sender: AccountAddress,
	gas_status: &mut impl GasMeter,
) -> VMResult<()>;
```

上述参数中的 `module` 是[序列化的格式](#Binary-Format)，且 **VM** 执行下列步骤：

* 反序列化该 `module`：如果该 `module` 没有反序列化，将会返回一个具有合适 `StatusCode` 的错误；

* 核验该 `module` 的地址和该 `sender` 的地址相同：该核验过程会验证发布者就是最终[持有该 `module` 的账户](#References-to-Data-and-Code)。如果不匹配，则返回 `StatusCode::MODULE_ADDRESS_DOES_NOT_MATCH_SENDER` 错误；

* 核验该 `module` 还没发布过：**Move** 中的代码是不可变的，尝试覆写一个存在的 *Module* 会导致一个 `StatusCode::DUPLICATE_MODULE_NAME` 的错误；

* 验证 *Loading*：**VM** 会对 *Module* 进行[验证](#Verification)以证明其正确性。然而，不管是 `Module` 还是其任意依赖，事实上都是保存在缓存中的。**VM** 会确保在找到引用时可以加载 `module`。当 `module` 加载失败时，会返回一个具有合适 `StatusCode` 的错误；

* 发布：**VM** 会将该 `module` 序列化后的字节，以合适的[键](#References-to-Data-and-Code)写入到存储中。在该步骤之后，该 `module` 的任意引用都是可用的了。

### Script Execution

**VM** 是允许执行 [**Script**](#Binary-Format)的。所谓的 *Script* 就是一个定义在一个 `script`块中的 Move 函数，它执行对已发布到链上的框架的调用，来完成一笔逻辑交易。	*Script* 是既不能被存储，也不能被其它 *Script* 或 *Module*调用的。

```rust
pub fn execute_script(
	&mut self,
	script: Vec<u8>,
	ty_args: Vec<TypeTag>,
	args: Vec<Vec<u8>>,
	senders: Vec<AccountAddress>,
	gas_status: &mut impl GasMeter,
) -> VMResult<()>;
```

上述参数中的 `script` 是以[序列化的格式](#Binary-Format)指定的。如果 `script` 是泛型的，则 `ty_args` 这个动态数组则会含有 `TypeTag`这个参数类型值。签名该 `Script` 的所有的账户地址指定在 `sender` 动态数组中。其它额外的参数由 `args` 这个动态数组提供，其中每一个参数都是的基于BCS序列化的字节数组。**VM** 执行下列步骤：

* 加载 `Script` 和 `主函数`：

	- 计算二进制 `script` 值的 `sha3_256` 哈希值；
	- 该哈希值用来访问 `Script` 缓存以查看是否被加载（即其识别符）；
	- 如果 `Script` 不在缓存中，就会执行 [`Loading`](#Loading)；如果加载失败，则会停止执行并返回一个具有合适 `StatusCode` 的错误；
	- Script的主函数将于类型参数实例进行[核验](#Verification)，如果有错误，将停止执行并返回错误；

* 构建参数列表：第一个参数便是 **VM** 为 `senders` 参数中的账户地址所创建的 `Signer` 值。`args`数组中的其它参数都会与一个类型许可的白名单比对，如果是在名单内则添加到该 `Script` 的参数中，否则就会返回 `StatusCode::TYPE_MISMATCH` 错误。

* 执行 `Script`：**VM** 调用 *Interpreter* 来[执行该 `Script`](#Interpreter)。执行期间出现任意错误都会中止并返回。该 **VM** 会返回是否执行成功或失败。


### Script Function Execution

*Script Function* （**VM**在V2+版本中） 类似于 `Script`，所不同的是，	Move字节码来自于链上 `Module` 中的 Move函数（具有 `script` 可见性）。`Script Function`是通过 `module`名和 `Function`名共同来指定的。

```rust
pub fn execute_script_function(
	&mut self,
	module: &ModuleId,
	function_name: &IdentStr,
	ty_args: Vec<TypeTag>,
	args: Vec<Vec<u8>>,
	senders: Vec<AccountAddress>,
	gas_status: &mut impl GasMeter,
) -> VMResult<()>;
```

执行 `Script Function` 也类似于执行 `Script`。不同于使用来自 `Script` 的Move字节码， `Script Function` 是从链上 `Module` 中加载的，并且 `MoveVM` 会核验它是否拥有 `Script` 可见性。其余执行部分都和执行 `Script` 相同。如果链上加载的该函数不存在，会以 `FUNCTION_RESOLUTION_FAILURE` 的错误码返回。如果该函数没有 `Script` 可见性，则以 `EXECUTE_SCRIPT_FUNCTION_CALLED_ON_NON_SCRIPT_VISIBLE` 的错误码返回。

### Function Execution 

**VM** 允许通过一个 `ModuleId` 和一个 `Function Name` 来执行[*Module* 中任意的 *Function*](#Binary-Format)。每个 `Module` 中的函数名是唯一的（即不允许重载），因此并不需要函数签名。注意：参数核验是通过 [*Interpreter*](#Interpreter) 来完成的。

就像 [Validation]()和[Execution]()中描述的那样，适配器使用该入口点来运行特定的系统函数。鉴于没有可见性检查，这是进入系统的非常强大的入口点。客户端可能会在内部使用（比如构造一个创世状态）使用该入口点，或者对其进行限制性封装和公开。

```rust 
pub fn execute_function(
	&mut self,
	module: &ModuleId,
	function_name: &IdentStr,
	ty_args: Vec<TypeTag>,
	args: Vec<Vec<u8>>,
	gas_status: &mut impl GasMeter,
) -> VMResult<()>;
```

**VM** 执行一下步骤：

* 加载该函数：
	- 先[加载](#Loading)指定的 `Module`，没有对应模块则报错；
	- **VM** 在该 `Module` 内搜寻该 `Function`，找不到时也报对应的错误；
	- [加载](#Loading) `ty_args` 数组中的每一个类型，此时会核验这些参数的类型是否匹配；

* 构建参数列表：参数会和一个由所允许类型构成的白名单核验，如果类型不匹配则会报 `StatusCode::TYPE_MISMATCH` 错误

* 执行该函数： **VM** 调用 *Interpreter* 来[执行该函数](#Interpreter)。执行期间任意的错误都会中止该 `Interpreter` 并返回对应错误。 **VM** 会返回是否执行成功或失败。

## Binary Format

*Module* 和 *Script* 只能以二进制形式进入 **VM**，并且 *Module* 是以二进制形式保存在链上的。*Module* 在逻辑上就是函数和数据结构的集合。*Script* 则只是一个入口点，就是一个带有参数但没有返回值的单个函数。

*Module* 可以被视为库或共享代码，然而 *Script* 只能随着交易作为输入出现。

二进制文件是由 `头部` 和`一组表` 组成。其中一些表对 *Module* 和 *Script* 均适用，而另一些则只适用于其一。还有一些 `数据` 也仅适用于其一。

该二进制格式在压缩整数方面广泛的使用了 [ULEB128](https://en.wikipedia.org/wiki/LEB128)。这种二进制格式中的大部分数据都是以索引的形式存在，这样的压缩在空间节省方面很重要。未经压缩的整数采用 `小端序`。

动态数组（`Vector`）序列化的时候，首先是大小（以`ULEB128`格式），然后就是连续的元素。

### Binary Header

每一个二进制文件都以 `header` 开始，其格式如下：

* `Magic`：占4字节，其值分别是0XA1, 0X1C, 0XEB, 0X0B（即"AIICEB0B"，即近似的"AliceBob"）
* `Version`: 占4字节的小端序的非符号整型
* `Table Count`: 表的数量（以 `ULEB128` 表示）。当前最大表的数量包含在1字节中。并非所有表都需要存在。每一种表只能出现一次，不允许重复。这些表可以以任意顺序序列化。

### Table Header

紧跟着二进制头（`Binary Header`）的便是表头（`Table Header`）。表的数量是定义在上述 `Table Count`中。每一个表头的格式如下：

* `Table Kind`: 使用1个字节来表示[表的种类](#Tables)，被序列化在其后两个`entries`所定义的位置
* `Table Offset`: 从表头结尾处（也是表内容开始的地方）开始的偏移量（以ULEB128表示）
* `Table Length`: 表内容的字节数（以ULEB128表示）

表必须彼此连续，从表头的末尾开始。表内容之间不能有任何间隙。表的内容不能重叠。

### Tables

`Table Kind` 占1个字节，是下列之一：

* `0x1`: `MODULE_HANDLES`            - 适用 Modules 和 Scripts
* `0x2`: `STRUCT_HANDLES` 		     - 适用 Modules 和 Scripts
* `0x3`: `FUNCTION_HANDLES` 	     - 适用 Modules 和 Scripts
* `0x4`: `FUNCTION_INSTANTIATIONS`   - 适用 Modules 和 Scripts
* `0x5`: `SIGNATURES` 			     - 适用 Modules 和 Scripts（译者注: Function参数类型）
* `0x6`: `CONSTANT_POOL` 		     - 适用 Modules 和 Scripts
* `0x7`: `IDENTIFIERS` 			     - 适用 Modules 和 Scripts（译者注: Struct的名字也适用，Function的名字也适用，结构体字段的名字也适用）
* `0x8`: `ADDRESS_IDENTIFIERS`       - 适用 Modules 和 Scripts
* `0xA`: `STRUCT_DEFINITIONS`        - 仅适用于 Modules
* `0xB`: `STRUCT_DEF_INSTANTIATIONS` - 仅适用于 Modules
* `0xC`: `FUNCTION_DEFINITIONS`      - 仅适用于 Modules
* `0xD`: `FIELD_HANDLES`             - 仅适用于 Modules
* `0xE`: `FIELD_INSTANTIATIONS`      - 仅适用于 Modules
* `0xF`: `FRIEND_DECLS`              - 仅适用于 Modules（V2+）

上述这些表的作用及格式是：

* `MODULE_HANDLES`: 每个 `Module Handle` 标识 `Module` 位置的索引对:
    * `address`: `ADDRESS_IDENTIFIERS` 表中的索引位置（ULEB128表示），描述该模块所处账户
    * `name`: `IDENTIFIERS` 表中的索引位置（ULEB128表示），描述该模块的名字

* `STRUCT_HANDLES`: 每个 `STRUCT_HANDLE` 都包含了用来唯一性的标识某个用户类型的所有信息:
	* `module`: `MODULE_HANDLES` 表中的索引位置（ULEB128表示），描述该结构体所处的模块
	* `name`: `IDENTIFIERS` 表中的索引位置（ULEB128表示），描述该结构体的名字
	* `nominal resource`: 定义某个结构体是否是 `Resource`（U8布尔值，true表示是Resource）
	* `type parameters`: 如果结构体是泛型的，则是[type parameter kinds](#Kinds)的数组；否则是个空数组
		* `length`: 数组的长度，实际上是该泛型结构体的泛型参数的个数（ULEB128表示）
		* `kinds`: array of `length` U8 kind values; not present if length is 0 （？？？）

`FUNCTION_HANDLES`: 每个 `Function Handle` 都包含了用来唯一性的标识某个函数的所有信息:
    * `module`: `MODULE_HANDLES` 表中的索引位置（ULEB128表示），描述该函数所处的模块
    * `name`: `IDENTIFIERS` 表中的索引位置（ULEB128表示），描述该函数的名字
    * `parameters`: `SIGNATURES` 表中的索引位置（ULEB128表示）， 描述该函数的参数类型
    * `return`: `SIGNATURES` 表中的索引位置（ULEB128表示），描述该函数的返回值类型
	* `type parameters`: 如果结构体是泛型的，则是[type parameter kinds](#Kinds)的数组；否则是个空数组
		* `length`: 数组的长度，实际上是该泛型结构体的泛型参数的个数（ULEB128表示）
		* `kinds`: array of `length` U8 kind values; not present if length is 0 （？？？）


* `FUNCTION_INSTANTIATIONS`: 每个 `Function Instantiation` 都描述了某个泛型函数的实例。函数实例可以是完整的或部分的。比如：给定一个泛型函数 `f<U8, Bool>()`，其完整实例可能是 `f<U8, Bool>()`，而其部分实例可以是 `f<U8, Z>()`，这里的`Z`是一个给定上下文中的类型参数（典型的就是另一个函数: `g<Z>()`）。

    * `function handle`: ULEB128 index into the `FUNCTION_HANDLES` 表中该泛型函数的索引位置（ULEB128表示），描述该实例
    * `instantiation`:  `SIGNATURES` 表中的索引位置，用以描述该函数的实例

* `SIGNATURES`: 这是二进制文件中的签名集合。每个 `Signature` 都是一个 [Signature Tokens](#SignatureTokens) 的数组，因此每一个 `SIgnature`都将携带对应的长度，其后就是对应的 `Signature Tokens`。

* `CONSTANT_POOL`: 二进制文件中的常量集合。每个 `Constant` 都是一个可复制的原语值，或者是一个原语数组的数组。`Constant` 不能是用户类型。 `Constant` 是根据定义在[Move Values](#Move-Values)的规则来序列化的，并以序列化的形式保存在 *Table* 中。每个 `Constant Pool`中的 `Constant`都拥有下列值：
    * `type`: `Signature Token` 类型
    * `length`: 序列化值的字节长度
    * `value`: 序列化值

* `IDENTIFIERS`: 二进制文件中的标识符集合。标识符是字符数组。这些标识符的格式是长度（ULEB128表示）后跟字符。标识符只能包含ASCII字符集中的字符，具体而言：必须以字母或下划线开头，后跟字母、下划线或数字。

* `ADDRESS_IDENTIFIERS`: 在 `MODULE_HANDLES` 中使用的地址集合。地址具有固定的大小，因此它们在此表中连续存储。

* `STRUCT_DEFINITIONS`: 二进制文件中定义的结构体或用户类型。每个结构体包括了下列字段：
    * `struct_handle`: ULEB128 index in the `STRUCT_HANDLES` table for the
    handle of this definition
    * `field_information`: 字段的额外信息，或者该结构体是否是 `native` 的
        * `tag`: 占1字节，如果该结构体是 `native` 的则值为 `0x1`，如果该结构体包含字段，则为 `0x2`且后跟： 
        * `field count`: 该结构体字段数量（ULEB128表示）
        * `fields`: a field count of
            * `name`: `IDENTIFIERS` 表中的索引位置（ULEB128表示），用以描述结构体中某个字段的名字
            * `field type`: [SignatureToken](#SignatureTokens) - 字段类型

* `STRUCT_DEF_INSTANTIATIONS`: 任意给定泛型结构体的实例集合。包括了下列字段：
    * `struct handle`: ULEB128 index into the `STRUCT_HANDLES` table of the
    generic struct for this instantiation (e.g., `struct X<T>`)
    * `instantiation`: `SIGNATURES` 表中的索引位置，描述该结构体的实例（该实例可以是完整或部分的）(比如`X<U64>` 或 `X<Z>` 内部有另一个泛型函数或泛型结构体拥有泛型参数 `Z`) 

* `FUNCTION_DEFINITIONS`: 该二进制文件中所定义的函数集合。一个函数定义包含了下列字段：
    * `function_handle`: 定义在 `FUNCTION_HANDLES` 表中的索引(ULEB128表示)，用以标识该函数定义的句柄
    * `visibility`: 占1字节的用以表示该函数可见性 (仅适用于V2+)
        * `0x0` 如果该函数是该 `Module` 私有的
        * `0x1` 如果该函数是该 `Module` 公开的（即外部也可用）
        * `0x2` 用于 `script` 函数
        * `0x3` 如果该函数是该 `Module` 私有的，但对 `friend Module` 可见
    * `flags`: 1字节:
        * `0x0` 如果该函数是该 `Module` 私有的（仅适用于V1）
        * `0x1` 如果该函数是该 `Module` 公开的（即外部也可用）（仅适用于V1）
        * `0x2` 如果该函数是 `native` 原生的，即不是基于 `Move` 实现的
    * `acquires_global_resources`: 该函数访问 `Resource`
        * `length`: 数组的长度，即该函数所 `acquired` 的 `Resource` 的个数（ULEB128编码）
        * `resources`: `STRUCT_DEFS` 表中的索引数组（ULEB128表示），指明该函数所 `acquired` 的 `Resources`
    * `code_unit`: if the function is not native, the code unit follows: 如果该函数不是原生的，则
        * `locals`: `SIGNATURES` 表中的索引（ULEB128表示），该函数本地变量的类型
        * `code`: [字节码](#Bytecodes)数组, 即函数体
            * `length`: 字节码的长度
            * `bytecodes`: 字节码（长度非固定）

* `FIELD_HANDLES`: 代码中访问的字段的集合。每个 `field handle（字段句柄）` 都由下列字段组成：
    * `owner`: 字段所属类型在 `STRUCT_DEFS` 表中的索引位置（ULEB128表示）
    * `index`: `owner`的字段数组中的字段的位置（ULEB128表示）

* `FIELD_INSTANTIATIONS`: 代码中访问的泛型字段的集合。每个字段实例都是一个索引对：
    * `field_handle`: 泛型字段所在 `FIELD_HANDLES` 表中的索引（ULEB128表示）
    * `instantiation`: 字段所属类型的实例，所在 `SIGNATURES` 表中的索引位置（ULEB128表示）

* `FRIEND_DECLS`: 所声明的 `friend module` 的集合。每个 `module` 具有以下内容：
    * `address`: `ADDRESS_IDENTIFIERS` 表中的索引位置，标记该 `Module` 发布在哪一个账户下面（ULEB128编码）
    * `name`: `IDENTIFIERS` 表中该`Module`的名字的索引（ULEB128表示）


### kinds

一个 `Type Parameter Kind` 占1字节，是其中之一：

* `0x1`: `ALL` - 该类型参数可以被一个 `Resource` 或 `可复制类型` 替换
* `0x2`: `COPYABLE` - 该类型参数必须被 `可复制类型` 替换
* `0x3`: `RESOURCE` - 该类型参数必须被 `Resource类型` 替换

### SignatureTokens

一个 `SignatureToken` 占1字节，是其中之一：

> 译者注: Move中的SignatureTokens是用来表示该语言中的类型签名。用于描述函数，结构体，模块等定义中的参数类型，返回类型以及字段类型等信息。SignatureTokens提供了一种规范化的方法来表示和处理Move语言中的类型信息，以支持编译、解析、验证等操作。

* `0x1`: `BOOL` 			 - 布尔
* `0x2`: `U8` 				 - U8（字节）
* `0x3`: `U64` 				 - 64位非符号整型
* `0x4`: `U128` 			 - 128位非符号整型
* `0x5`: `ADDRESS`           - 链上的 `AccountAddress`, 可以是16, 20, 或 32字节
* `0x6`: `REFERENCE`         - 引用; 其后必须跟着另一个表示所引用类型的SignatureToken
* `0x7`: `MUTABLE_REFERENCE` - 可变引用; 其后必须跟着另一个表示所引用类型的SignatureToken
* `0x8`: `STRUCT` 			 - 结构体; 其后必须 `STRUCT_HANDLES`表中描述该结构体类型的索引（ULEB128表示）
* `0x9`: `TYPE_PARAMETER`    - 泛型结构体或泛型函数的类型参数; 其后必须跟着其容器中类型参数数组中的索引（ULEB128表示）
* `0xA`: `VECTOR` 			 - 数组; 其后必须跟着该数组元素的类型 SignatureToken
* `0xB`: `STRUCT_INST` 		 - 结构体实例; 必须跟随一个索引，该索引指向 `STRUCT_HANDLES` 表中实例化的泛型类型，并且还需要一个描述替代类型的数组，即一个由SignatureTokens组成的数组
* `0xC`: `SIGNER` 			 - Signer类型，这是VM中表示签署该交易的 "entity" 的一个特殊的类型，`Signer` 是一个资源类型
* `0xD`: `U16` 				 - 16位非符号整型
* `0xE`: `U32` 				 - 32位非符号整型
* `0xF`: `U256` 			 - 256位非符号整型

`SignatureToken`示例：

* `u8, u128` -> `0x2 0x2 0x4` - size(`0x2`), U8(`0x2`), u128(`0x4`)
* `u8, u128, A` where A is a struct -> `0x3 0x2 0x4 0x8 0x10` - size(`0x3`),
U8(`0x2`), u128(`0x4`), Struct::A
(`0x8 0x10` 假定该结构体是在 `STRUCT_HANDLES` 表中 `0x10` 的索引处)
* `vector<address>, &A` 其中的A是一个结构体 -> `0x2 0xA 0x5 0x8 0x10` - size(`0x2`),
vector<address>(`0xA 0x5`), &Struct::A(`0x6 0x8 0x10` 假定该结构体是在 `STRUCT_HANDLES` 表中 `0x10` 的索引处)
* `vector<A>, &A<B>` 其中的A和B是结构体 ->
`0x2 0xA 0x8 0x10 0x6 0xB 0x10 0x1 0x8 0x11` -
size(`0x2`), vector\<A\>(`0xA 0x8 0x10`),
&Struct::A\<Struct::B\> (`0x6` &, `0xB 0x10` A<\_>, `0x1 0x8 0x11` B type
instantiation; 假定该结构体A和B分别是在 `STRUCT_HANDLES` 表中 `0x10`, `0x11` 的索引处)


### Bytecodes

字节码是 **Move VM** 中的可变大小指令。字节码是由操作码（1字节）组成，某些操作码其后可能挂载了一些操作数（表示在括号中）：

* `0x01`: `POP`
* `0x02`: `RET`            
* `0x03`: `BR_TRUE(offset)`  - 如果为true，则跳转到offset(代码流的偏移地址)位置处，执行分支代码
* `0x04`: `BR_FALSE(offset)` - 如果为false(代码流的偏移地址)位置处，执行分支代码
* `0x05`: `BRANCH(offset)`   - 跳转到offset(代码流的偏移地址)位置处，执行分支代码
* `0x06`: `LD_U64(value)`    - 以小端序加载U64值(即Push到栈上)
* `0x07`: `LD_CONST(index)`  - 加载 `CONSTANT_POOL` 表中index位置处的常量到栈上
* `0x08`: `LD_TRUE`          - 加载true到栈上
* `0x09`: `LD_FALSE`         - 加载false到栈上
* `0x0A`: `COPY_LOC(index)`  - 将函数参数或局部变量的值(由index指定的)Copy到栈上
* `0x0B`: `MOVE_LOC(index)`  - 将函数参数或局部变量的值(由index指定的)Move到栈上
* `0x0C`: `ST_LOC(index)`    - 将栈顶的值弹出并保存在局部变量(由index指定的)中
* `0x0D`: `MUT_BORROW_LOC(index)`   - 可变的借用函数参数或局部变量的值(由index指定的)
* `0x0E`: `IMM_BORROW_LOC(index)`   - 不可变的借用函数参数或局部变量的值(由index指定的)
* `0x0F`: `MUT_BORROW_FIELD(index)` - 可变的借用 `FIELD_HANDLES` 表中index指定的值
* `0x10`: `IMM_BORROW_FIELD(index)` - 不可变的借用 `FIELD_HANDLES` 表中index指定的值
* `0x11`: `CALL(index)`             - 调用 `FUNCTION_HANDLES` 表中index指定的函数
* `0x12`: `PACK(index)`             - 将位于 `STRUCT_DEFINITIONS` 表中index指定位置的数据打包成一个结构体或资源对象，并入栈
* `0x13`: `UNPACK(index)`           - 将 `STRUCT_DEFINITIONS` 表中index指定位置的结构体或资源对象解构成多个独立的值
* `0x14`: `READ_REF`
* `0x15`: `WRITE_REF`
* `0x16`: `ADD`
* `0x17`: `SUB`
* `0x18`: `MUL`
* `0x19`: `MOD`
* `0x1A`: `DIV`
* `0x1B`: `BIT_OR`
* `0x1C`: `BIT_AND`
* `0x1D`: `XOR`
* `0x1E`: `OR`
* `0x1F`: `AND`
* `0x20`: `NOT`
* `0x21`: `EQ`
* `0x22`: `NEQ`
* `0x23`: `LT`
* `0x24`: `GT`
* `0x25`: `LE`
* `0x26`: `GE`
* `0x27`: `ABORT`
* `0x28`: `NOP`
* `0x29`: `EXISTS(index)`            - 判断 `STRUCT_DEFINITIONS` 表中index对应的值是否存在
* `0x2A`: `MUT_BORROW_GLOBAL(index)` - 可变的借用 `STRUCT_DEFINITIONS` 表中index对应的值
* `0x2B`: `IMM_BORROW_GLOBAL(index)` - 不可变的借用 `STRUCT_DEFINITIONS` 表中index对应的值
* `0x2C`: `MOVE_FROM(index)`         - 移除 `STRUCT_DEFINITIONS` 表中index对应的值
* `0x2D`: `MOVE_TO(index)`           - 移除 `STRUCT_DEFINITIONS` 表中index对应的值
* `0x2E`: `FREEZE_REF`               - 将可变引用转换成不可变引用
* `0x2F`: `SHL`
* `0x30`: `SHR`
* `0x31`: `LD_U8(value)`
* `0x32`: `LD_U128(value)`
* `0x33`: `CAST_U8`
* `0x34`: `CAST_U64`
* `0x35`: `CAST_U128`
* `0x36`: `MUT_BORROW_FIELD_GENERIC(index)`  - 获取位于 `FIELD_INSTANTIATIONS` 表中index处的字段的可变引用
* `0x37`: `IMM_BORROW_FIELD_GENERIC(index)`  - 获取位于 `FIELD_INSTANTIATIONS` 表中index处的字段的不可变引用
* `0x38`: `CALL_GENERIC(index)` 			 - 获取位于 `FUNCTION_INSTANTIATIONS` 表中index处的函数，作为闭包调用（调用过程中函数实例所引用的泛型类型会被具体化）
* `0x39`: `PACK_GENERIC(index)` 			 - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，将其打包成一个泛型类型的元组
* `0x3A`: `UNPACK_GENERIC(index)` 			 - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，将其解构成多个独立的值
* `0x3B`: `EXISTS_GENERIC(index)` 			 - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，判断是否存在
* `0x3C`: `MUT_BORROW_GLOBAL_GENERIC(index)` - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，获取一个全局资源的可变引用
* `0x3D`: `IMM_BORROW_GLOBAL_GENERIC(index)` - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，获取一个全局资源的不可变引用
* `0x3E`: `MOVE_FROM_GENERIC(index)` 		 - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，将该地址指向的资源移动到当前账户中
* `0x3F`: `MOVE_TO_GENERIC(index)`           - 获取位于 `STRUCT_DEF_INSTANTIATIONS` 表中index处的值，将其存储为指定类型的全局资源

> 译者注: 以上 `index` 均为 `ULEB128` 格式，不再赘述。

> 译者注: 以上操作码对应指令的作用（需要进一步完善），参考自 《Move: A Language With Programmable Resource》

### Module Specific Data

一个 *Module* 的二进制文件中，在所有的 `Tables` 之后，有一个以ULEB128形式包含的一个索引位于最后。该索引指向 `ModuleHanle` 表，它表示模块自身。即指明了 `Module` 的存储位置。

### Script Specific Data

一个 *Script* 是没有 `FUNCTION_DEFINITIONS` 表的，在其二进制文件的末尾按照如下顺序指明了入口点：

* `type parameters`: 如果该 `Script` 的入口点是泛型，则该数组内就是这些类型参数的数量和种类
    * `length`: 类型参数的个数(ULEB128表示)，0表示该Script非泛型
    * `kinds`: [kind](#Kinds)数组, 长度为0则不存在

* `parameters`: `SIGNATURES`表中的索引位置(ULEB128表示)，指明该入口点的参数类型

* `code`: [字节码](#Bytecodes)数组, 函数体
    * `length`: 字节码长度
    * `bytecodes`: 连续序列化的字节码，大小非固定

## Reference
[Move VM Specification](https://github.com/move-language/move/blob/main/language/documentation/spec/vm.md)
