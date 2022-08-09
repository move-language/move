# Global Storage - Structure

The purpose of Move programs is to [read from and write to](./global-storage-operators.md) tree-shaped persistent global storage. Programs cannot access the filesystem, network, or any other data outside of this tree.

In pseudocode, the global storage looks something like

# 全局存储 - 结构
Move 程序的目的是读取和写入树形持久全局存储。程序无法访问此树之外的文件系统、网络或任何其他数据。

在伪代码中，全局存储看起来像

```move
struct GlobalStorage {
  resources: Map<(address, ResourceType), ResourceValue>
  modules: Map<(address, ModuleName), ModuleBytecode>
}
```

Structurally, global storage is a [forest](https://en.wikipedia.org/wiki/Tree_(graph_theory)) consisting of trees rooted at an account [`address`](./address.md). Each address can store both [resource](./structs-and-resources.md) data values and [module](./modules-and-scripts.md) code values. As the pseudocode above indicates, each `address` can store at most one resource value of a given type and at most one module with a given name.

从结构上讲，全局存储是一个由植根于帐户地址的树组成的森林。每个地址都可以存储资源数据值和模块代码值。如上面的伪代码所示，每个地址最多可以存储一个给定类型的资源值和一个给定名称的模块。
