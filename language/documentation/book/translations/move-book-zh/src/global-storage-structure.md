# 全局存储 —— 结构

The purpose of Move programs is to [read from and write to](./global-storage-operators.md) tree-shaped persistent global storage. Programs cannot access the filesystem, network, or any other data outside of this tree.

Move 程序的目的是[读取和写入](./global-storage-operators.md)树形的持久全局存储。程序不能访问文件系统、网络或任何此树以外的数据。

In pseudocode, the global storage looks something like:

在伪代码中，全局存储看起来像：

```move
struct GlobalStorage {
  resources: Map<(address, ResourceType), ResourceValue>
  modules: Map<(address, ModuleName), ModuleBytecode>
}
```

Structurally, global storage is a [forest](https://en.wikipedia.org/wiki/Tree_(graph_theory)) consisting of trees rooted at an account [`address`](./address.md). Each address can store both [resource](./structs-and-resources.md) data values and [module](./modules-and-scripts.md) code values. As the pseudocode above indicates, each `address` can store at most one resource value of a given type and at most one module with a given name.

从结构上讲，全局存储是一个[森林（forest）](https://en.wikipedia.org/wiki/Tree_(graph_theory))，这个森林由以账户[地址（`address`）](./address.md)为根的树组成。每个地址可以存储[资源（resource）](./structs-and-resources.md)数据和[模块（module）](./modules-and-scripts.md)代码。如上面的伪代码所示，每个地址（`address`）最多可以存储一个给定类型的资源值，最多可以存储一个给定名称的模块。
