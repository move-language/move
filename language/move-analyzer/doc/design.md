## The design of move-analyzer2

move-analyzer2 is a novel move-lanugage IDE support.
move-analyzer2 include semantic analyzer of move and MSL.
Here we demonstrate serval core concepts in move-analyzer2 to help you understand how it works.

## Basic

### `Item`

Item is nearly anything that you can define in you program and save for later use.
Variable, parameter, typeparameter are all items.

### `Scope`

Scope is where you defined variable, function, etc.
For example, module, function are all `Scope`.

~~~
// module is a `Scope`, you can define a function inside.
0x1::some_module{
    // define function, const,...
    fun some_fun() {
        // function is a `Scope` too, you can create variable and ...
    }
}
~~~

### `ProjectContext`

`ProjectContext` composed with two important field(`scopes` and `addresses`) and someother addtional information.

`scopes` is simply a stack of `Scope`.

`scopes` works like function calls.

* push a frame on stack when you want call function.

* pop out a frame when function returns.

Example:
~~~
fun some_fun() {

    {   // When enter, we create a `Scope` to hold declared variables.
        // push this `Scope` to `scopes` stack.
        let _x = 100;

        ...

        // At end, we pop out this `Scope`.
    }

}
~~~
`addresses` are just Global `Scope` which contains global struct definition, function definition,etc.

Example:
~~~
0x1::some_module {
    fun some_fun() {  // some_fun will saved in `addresses` and can be accessed.

    }
}
~~~

### `ResolvedType`

~~~
#[derive(Clone)]
pub enum ResolvedType {
    // Structure type
    Struct(item::ItemStruct),
    // Build in type like u8 ...
    BuildInType(BuildInType),
    /// T : drop typeparameter
    TParam(Name, Vec<Ability>),

    /// & mut ...
    Ref(bool, Box<ResolvedType>),

    ...
}
~~~
`ResolvedType` is the type which have semantic meanings.
It's resolved from user defined.
Example:
~~~
struct XXX { ... }
fun some_fun() : XXX  // XXX will resovled to ResolvedType::Struct which will contains info of a structure.
{

}
~~~


### `Access`
`Access` means a access point to a `Item`.
It happens when using a item.

Example:
~~~
fun some_fun() {
    let x = 1;

    some_fun2(x); // When we dealing with access of x
                  // We have a structure below
                  Access::ExprAccessChain(
                    NameAccessChain,  // access point.
                    Option<AddrAndModuleName>,  // The item maybe locate at some module,So we can implement goto to definition,... for module.
                    Box<Item>, // The actual Item.
                )
    ),
~~~

### `ItemOrAccessHandler`.

The trait `ItemOrAccessHandler` is a consumer to consume the information create by `Project`.

~~~

pub trait ItemOrAccessHandler: std::fmt::Display {
    /// Handle this item_or_access.
    fn handle_item_or_access(
        &mut self,
        services: &dyn HandleItemService,
        project_context: &ProjectContext,
        item_or_access: &ItemOrAccess,
    );

    ...

    /// Visitor should finished.
    fn finished(&self) -> bool;
}
~~~
Actualy the `ItemOrAccessHandler` is a consumer and can consume the information create by `Project`.

`ItemOrAccess` is either a `Item` or `Access`. Our `goto to definition` and `auto completion` ,... base On `ItemOrAccessHandler`.


`ItemOrAccess` is either a `Item` or `Access`. Features like `goto to definition` and `auto completion` ,... base On `ScopeVisitor`.

For example

When you want to implement `goto to definition`.
* if the `item_or_access` is `Item` you just return the `def_loc` of the `item_or_access`.
* if the `item_or_access` is `Access` you just return the `def_loc` of the `access`'s `Item`.

So the main purpose of `Project` is to produce `ItemOrAccess`.

### AstProvider

`AstProvider` is a trait that have a lot of `with` function.
~~~
fn with_const(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Constant)) {
    ...
}

fn with_struct(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &StructDefinition)) {
    ...
}
~~~
This is convenient way for someone who interested in gettting all the constants or functions in a module.

And the trait `AstProvider` provides us a way visit some part of the project's AST, We will talk about it later.


### `Project`

`Project` represents a loaded project from a `Move.toml`.
~~~
pub struct Project {
    /// All the AST definition.
    pub(crate) modules: HashMap<
        PathBuf,  // manifest path.
        Rc<RefCell<SourceDefs>>,
    >,
    /// All manifests
    pub(crate) manifests: Vec<move_package::source_package::parsed_manifest::SourceManifest>,

    ...

    /// All manifests related to this project.
    pub(crate) manifest_paths: Vec<PathBuf>,

    /// Global constants,functions.etc...
    pub(crate) project_context: ProjectContext,
    pub(crate) manifest_not_exists: HashSet<PathBuf>,
}
~~~
The project mainly constains the AST and global items assoiate with this Project.

Let me introduce `Project`'s creation.
`Project` creation involves  next steps.

* loading AST and dependency's AST into Memory.
* enter all the global function,const,and struct to `ProjectContext`.`addresses`.

Wait,But How can we do that.

The main entry point for `Project` to enter item and call `ItemOrAccessHandler`.`handle_item_or_access` ... is `visit`.
~~~
pub fn visit(
        &self,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        provider: impl AstProvider,
    ) {
        ...
    }

~~~

function `visit` is reponsible for iteration of all AST,create `ItemOrAccess`,enter `Item` to `scopes`,and call `ItemOrAccessHandler`'s method.

For example.
~~~
pub fn visit(
        &self,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        provider: impl AstProvider,
    ) {
        ...
        provider.with_const(|addr, name, c| {
            self.visit_const(Some((addr, name)), c, project_context, visitor);
        });
        ...
}

 pub(crate) fn visit_const(
        &self,
        enter_top: Option<(AccountAddress, Symbol)>,
        c: &Constant,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        ...
        // Get const's ty
        let ty = project_context.resolve_type(&c.signature, self);
        // Create the ItemOrAccess
        let item = ItemOrAccess::Item(Item::Const(ItemConst {
            name: c.name.clone(),
            ty,
            is_test: attributes_has_test(&c.attributes).is_test(),
        }));
        // Call visitor's handle_item_or_access method.
        visitor.handle_item_or_access(self, project_context, &item);
        // In this case this is a `ItemOrAccess::Item`
        let item: Item = item.into();
        // enter the `Item` into Scope.
        if let Some((address, module)) = enter_top {

            project_context.enter_top_item(self, address, module, c.name.value(), item.clone(), false);
        } else {
            project_context.enter_item(self, c.name.value(), item);
        }
    }
~~~

So create `Porject` basic contains two part `Load all the AST` and call `visit` enter all global items.

### syntax.rs
Why we have `syntax.rs` in source tree?

The official ast parse implemented as return an error when met the first error.

Even through We know we can recover from it.

For example
~~~
module 0x1::some_module {
    use sui::xxx::XXX // a missing semicolon
}
~~~
It is very common user forget enter a `semicolon`.

This is where the `syntax.rs` comming from.

`syntax.rs` is copy from the official version and modified to recover some recoverable errors.

especialy doing `auto completion` , The user's code is always incomplete.


### Multi Project support.

move-analyzer2 supprt multi project development concurrently.
It is convenient for user.
How is multi project support.
Well first We need load it the memory.
~~~
pub struct MultiProject {
    // projects are all loaded project from filesystem.
    pub projects: HashMap<HashSet<PathBuf>, Project>,
    ...
}
~~~
But which project should answer the user call (like `goto to definition`) , the `projects` field is map which the `key`
is `HashSet<PathBuf>`,which contains the all the manifest path the `Project` was loaded from.

And We can know `goto to definition`'s filepath, So we know which manifest file it belongs.

We just select the `Project` who contains the manifest file to handle the request.

There is another thing I want mention about.
Mutlti project can have the same dependency, So there are somethings can share together Like AST definitions.
~~~
pub struct MultiProject {
    ...
    pub asts: HashMap<PathBuf, Rc<RefCell<SourceDefs>>>,
    ...
 }
~~~
Share AST definition reduce memory consumption and save us time for loading project.



### Build in semantic analyzer
move-analyzer has build in semantic analyzer.It's can infer variable's type.
Know about variables in current scope,etc.
