# 全局存储 - 结构（Global Storage - Structure）

The purpose of Move programs is to [read from and write to](./global-storage-operators.md) tree-shaped persistent global storage. Programs cannot access the filesystem, network, or any other data outside of this tree.

Move程序的目标是[读写](./global-storage-operators.md)树状持久化全局存储。程序不能访问文件系统、网络或任何树外的数据。

In pseudocode, the global storage looks something like

全局存储在下面伪代码中看起来的样子

```move
struct GlobalStorage {
  resources: Map<(address, ResourceType), ResourceValue>
  modules: Map<(address, ModuleName), ModuleBytecode>
}
```

Structurally, global storage is a [forest](https://en.wikipedia.org/wiki/Tree_(graph_theory)) consisting of trees rooted at an account [`address`](./address.md). Each address can store both [resource](./structs-and-resources.md) data values and [module](./modules-and-scripts.md) code values. As the pseudocode above indicates, each `address` can store at most one resource value of a given type and at most one module with a given name.

结构上来看，全局存储是一个森林 [forest](https://en.wikipedia.org/wiki/Tree_(graph_theory))。森林由以[账户地址 account address ](./address.md)为根的树组成。每个地址可以存储[资源](./structs-and-resources.md)（resource）及[模块](./modules-and-scripts.md)（module）代码。如上面伪代码结构，每个地址可以为一个类型或一个模块名称存储最多一个值。
