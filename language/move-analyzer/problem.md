 

不理解的msl语法
Invariant  看起来可以声明范型参数，这个在那使用呢。
Variable   也有范型参数，看起来有点诡异。还有is_global看起来就是在指是不是全局的。
forall 有返回值吗？返回值是最有一个语句？ 返回值只能是true或者false这种bool值？

~~~
forall <binding>, ..., <binding> [ where <exp> ] : <exp>
~~~

//TODO
don't enter type parameter in scope,enter in some fun spec structure.




在同一个模块中
module std::string {
   use std::string:: 引用自身自动完成会崩溃。
   ... 
}




