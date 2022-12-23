use crate::modules::ERR_ADDRESS;

use super::scope::*;
use super::types::*;
use move_compiler::shared::Identifier;
use move_compiler::shared::TName;
use move_compiler::{parser::ast::*, shared::*};
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Loc;
use move_ir_types::location::Spanned;
use move_symbol_pool::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Clone)]
pub struct ItemStruct {
    pub(crate) name: StructName,
    pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) type_parameters_ins: Vec<ResolvedType>,
    pub(crate) fields: Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
}

impl std::fmt::Display for ItemStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.name.value().as_str())?;
        write!(f, "{{")?;
        if self.type_parameters.len() > 0 {
            write!(f, "<")?;
            for (index, t) in self.type_parameters.iter().enumerate() {
                write!(
                    f,
                    "{}{}",
                    t.name.value.as_str(),
                    if let Some(ins) = self.type_parameters_ins.get(index) {
                        format!("->{}", ins)
                    } else {
                        format!(":{:?}", t.constraints)
                    }
                )?;
            }
            write!(f, ">")?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone)]
pub enum Item {
    /////////////////////////////
    /// VALUE types
    Parameter(Var, ResolvedType),
    UseModule(
        ModuleIdent,        // 0x111::xxxx
        Option<ModuleName>, // alias
        Rc<RefCell<Scope>>, // module scope.
    ),
    UseMember(
        ModuleIdent, /* access name */
        Name,
        Option<Name>, /* name in the module */
        Rc<RefCell<Scope>>,
    ),

    Const(ConstantName, ResolvedType),
    Var(Var, ResolvedType),
    Field(Field, ResolvedType),

    /////////////////////////
    /// TYPE types
    Struct(ItemStruct),
    StructNameRef(
        AccountAddress,
        Symbol, // module name.
        StructName,
        Vec<StructTypeParameter>,
    ),
    Fun(ItemFun),

    /// build in types.
    BuildInType(BuildInType),
    /// Here are all definition.
    TParam(Name, Vec<Ability>),
    SpecSchema(Name, HashMap<Symbol, (Name, ResolvedType)>),
    /// a module name in 0x1111::module_name
    ModuleName(ModuleName),

    Dummy,
}

#[derive(Clone)]
pub struct ItemFun {
    pub(crate) name: FunctionName,
    pub(crate) type_parameters: Vec<(Name, Vec<Ability>)>,
    pub(crate) parameters: Vec<(Var, ResolvedType)>,
    pub(crate) ret_type: Box<ResolvedType>,
    pub(crate) ret_type_unresolved: Type,
}

impl std::fmt::Display for ItemFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}", self.name.value().as_str())?;
        if self.type_parameters.len() > 0 {
            write!(f, "<")?;
            for (name, a) in self.type_parameters.iter() {
                write!(f, "{}:{:?},", name.value.as_str(), a)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (name, t) in self.parameters.iter() {
            write!(f, "{}:{},", name.value().as_str(), t)?;
        }
        write!(f, ")")?;
        if !self.ret_type.as_ref().is_unit() {
            write!(f, ":{}", self.ret_type.as_ref())?;
        }
        Ok(())
    }
}

impl Item {
    ///   
    pub(crate) fn to_type(&self) -> Option<ResolvedType> {
        // TODO maybe a parameter to decide return TParam or not.
        let x = match self {
            Item::TParam(name, ab) => ResolvedType::TParam(name.clone(), ab.clone()),
            Item::Struct(x) => ResolvedType::Struct(x.clone()),
            Item::StructNameRef(addr, name, x, t) => ResolvedType::StructRef(
                addr.clone(),
                name.clone(),
                x.clone(),
                t.clone(),
                Default::default(),
            ),
            Item::BuildInType(b) => ResolvedType::BuildInType(*b),
            Item::Parameter(_, ty) | Item::Var(_, ty) | Item::Const(_, ty) => ty.clone(),
            Item::Field(_, ty) => ty.clone(),
            Item::Fun(x) => ResolvedType::Fun(x.clone()),
            Item::UseMember(_, name, _alias, module) => {
                return module
                    .as_ref()
                    .borrow()
                    .items
                    .get(&name.value)
                    .map(|i| i.to_type())
                    .flatten();
            }
            Item::UseModule(_, _, _) => return None,
            Item::Dummy => return None,
            Item::SpecSchema(_, _) => return Some(ResolvedType::UnKnown),
            Item::ModuleName(_) => return None,
        };
        Some(x)
    }

    pub(crate) fn def_loc(&self) -> Loc {
        match self {
            Item::Parameter(var, _) => var.loc(),
            Item::UseMember(_, name, _, module) => module
                .borrow()
                .items
                .get(&name.value)
                .map(|u| u.def_loc())
                .unwrap_or(UNKNOWN_LOC),
            Item::Struct(x) => x.name.loc(),
            Item::BuildInType(_) => UNKNOWN_LOC,
            Item::TParam(name, _) => name.loc,
            Item::Const(name, _) => name.loc(),
            Item::StructNameRef(_, _, name, _) => name.0.loc,
            Item::Fun(f) => f.name.0.loc,
            Item::UseModule(module, name, s) => s
                .borrow()
                .module_scope
                .clone()
                .expect(&format!("not found,module:{:?} name:{:?}", module, name))
                .name
                .loc(),
            Item::Var(name, _) => name.loc(),
            Item::Field(f, _) => f.loc(),
            Item::Dummy => UNKNOWN_LOC,
            Item::SpecSchema(name, _) => name.loc,
            Item::ModuleName(name) => name.loc(),
        }
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::Dummy
    }
}
#[derive(Clone, Copy, Debug)]
pub enum MacroCall {
    Assert,
}

impl MacroCall {
    pub(crate) fn from_chain(chain: &NameAccessChain) -> Self {
        match &chain.value {
            NameAccessChain_::One(name) => Self::from_symbol(name.value),
            NameAccessChain_::Two(_, _) => unreachable!(),
            NameAccessChain_::Three(_, _) => unreachable!(),
        }
    }
    pub(crate) fn from_symbol(s: Symbol) -> Self {
        match s.as_str() {
            "assert" => Self::Assert,
            _ => unreachable!(),
        }
    }
    pub(crate) fn to_static_str(self) -> &'static str {
        match self {
            MacroCall::Assert => "assert",
        }
    }
}

/// Get the last name of a access chain.
pub(crate) fn get_name_chain_last_name(x: &NameAccessChain) -> &Name {
    match &x.value {
        move_compiler::parser::ast::NameAccessChain_::One(name)
        | move_compiler::parser::ast::NameAccessChain_::Two(_, name)
        | move_compiler::parser::ast::NameAccessChain_::Three(_, name) => name,
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Parameter(var, t) => {
                write!(f, "parameter {}:{}", var.0.value.as_str(), t)
            }
            Item::UseModule(x, _, _) => {
                write!(f, "use {:?} {}", x, "_")
            }
            Item::ModuleName(name) => {
                write!(f, "module {}", name.value().as_str())
            }

            Item::UseMember(module, name, alias, _) => {
                write!(
                    f,
                    "use {:?}::{:?} {}",
                    module,
                    name,
                    if let Some(alias) = alias {
                        format!(" as {}", alias.value.as_str())
                    } else {
                        String::from_str("").unwrap()
                    },
                )
            }
            Item::Const(name, ty) => {
                write!(f, "const {}:{}", name.0.value.as_str(), ty)
            }
            Item::Struct(s) => {
                write!(f, "{}", s)
            }
            Item::StructNameRef(_, _, name, _) => {
                write!(f, "struct {}", name.value().as_str())
            }
            Item::Fun(x) => write!(f, "{}", x),
            Item::BuildInType(x) => {
                write!(f, "build in '{:?}'", x)
            }
            Item::TParam(tname, abilities) => {
                write!(f, "{}:", tname.value.as_str())?;
                for i in 0..abilities.len() {
                    let x = abilities.get(i).unwrap();
                    write!(f, "{:?},", x.value)?;
                }
                std::result::Result::Ok(())
            }
            Item::Var(name, ty) => {
                write!(f, "var {}:{}", name.0.value.as_str(), ty)
            }
            Item::Field(x, ty) => {
                write!(f, "field {}:{}", x.0.value.as_str(), ty)
            }
            Item::Dummy => {
                write!(f, "dummy")
            }
            Item::SpecSchema(name, _) => {
                write!(f, "schema {}", name.value.as_str())
            }
        }
    }
}

#[derive(Clone)]
pub enum Access {
    ApplyType(NameAccessChain, Box<ResolvedType>),

    ExprVar(Var, Box<Item>),
    ExprAccessChain(
        NameAccessChain,
        Option<ModuleScope>,
        Box<Item>, /* The item that you want to access.  */
    ),
    // Maybe the same as ExprName.
    // TODO   @XXX 目前知道的可以在Move.toml中定义
    // 可以在源代码中定义吗?
    ExprAddressName(Name),
    AccessFiled(
        Field,        // from
        Field,        // to
        ResolvedType, // field type
    ),
    ///////////////
    /// key words
    KeyWords(&'static str),
    /////////////////
    /// Marco call
    MacroCall(MacroCall, NameAccessChain),

    Friend(NameAccessChain, ModuleName),

    MoveBuildInFun(MoveBuildInFun, NameAccessChain),
    SpecBuildInFun(SpecBuildInFun, NameAccessChain),

    IncludeSchema(
        NameAccessChain, // access name. TODO if this can only be a simple name,So we can make it simpler.
        Name,            // schema name.
    ),
    SpecFor(
        Name, // access name like spec xxx {}
        Name, // for some item like fun xxx() {}
    ), //

    PragmaProperty(PragmaProperty),
}

pub enum FriendElement {
    Module(Rc<RefCell<Scope>>),
}
impl std::fmt::Display for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Access::ApplyType(a, x) => {
                write!(f, "apply type {:?}->{}", a.value, x)
            }

            Access::ExprVar(var, item) => {
                write!(f, "expr {}->{}", var.borrow().1.as_str(), item)
            }
            Access::ExprAccessChain(chain, _, item) => {
                write!(f, "expr {:?}->{}", chain, item)
            }
            Access::ExprAddressName(chain) => {
                write!(f, "{:?}", chain)
            }
            Access::AccessFiled(from, to, _) => {
                write!(f, "access_field {:?}->{:?}", from, to)
            }
            Access::KeyWords(k) => write!(f, "{}", *k),
            Access::MacroCall(macro_, _) => write!(f, "{:?}", macro_),
            Access::Friend(name, item) => {
                write!(
                    f,
                    "friend {}->{}",
                    get_name_chain_last_name(name).value.as_str(),
                    item
                )
            }
            Access::MoveBuildInFun(b, _) => {
                write!(f, "move buildin {}", b)
            }
            Access::SpecBuildInFun(b, _) => {
                write!(f, "spec buildin {}", b)
            }
            Access::IncludeSchema(name, spec) => {
                write!(
                    f,
                    "include {}->{}",
                    get_name_chain_last_name(name).value.as_str(),
                    spec.value.as_str()
                )
            }
            Access::SpecFor(name, spec) => {
                write!(
                    f,
                    "include {}->{}",
                    name.value.as_str(),
                    spec.value.as_str()
                )
            }
            Access::PragmaProperty(x) => {
                write!(
                    f,
                    "{}{}",
                    x.value.name.value.as_str(),
                    if let Some(_value) = &x.value.value {
                        //TODO. actual.
                        String::from("...")
                    } else {
                        String::from("...")
                    }
                )
            }
        }
    }
}

impl Access {
    pub(crate) fn access_def_loc(&self) -> (Loc /* access loc */, Loc /* def loc */) {
        match self {
            Access::ApplyType(name, x) => (name.loc, x.as_ref().def_loc()),
            Access::ExprVar(var, x) => (var.loc(), x.def_loc()),
            Access::ExprAccessChain(name, _, item) => {
                (get_name_chain_last_name(name).loc, item.as_ref().def_loc())
            }
            Access::ExprAddressName(_) => (UNKNOWN_LOC, UNKNOWN_LOC),
            Access::AccessFiled(a, d, _) => (a.loc(), d.loc()),
            Access::KeyWords(_) => (UNKNOWN_LOC, UNKNOWN_LOC),
            Access::MacroCall(_, chain) => (chain.loc, chain.loc),

            Access::Friend(name, item) => (get_name_chain_last_name(name).loc.clone(), item.loc()),
            Access::MoveBuildInFun(_, chain) => (chain.loc, chain.loc),
            Access::SpecBuildInFun(_, chain) => (chain.loc, chain.loc),
            Access::IncludeSchema(chain, x) => (get_name_chain_last_name(chain).loc.clone(), x.loc),
            Access::SpecFor(name, origin) => (name.loc, origin.loc),
            Access::PragmaProperty(x) => (x.loc, x.loc),
        }
    }

    /// Get loc
    pub(crate) fn access_module(&self) -> Option<(Loc, Loc)> {
        match self {
            Self::ExprAccessChain(chain, Option::Some(module), _) => match &chain.value {
                NameAccessChain_::One(_) => return None,
                NameAccessChain_::Two(m, _) => Some((m.loc, module.name.loc())),
                NameAccessChain_::Three(x, _) => Some((x.value.1.loc, module.name.loc())),
            },

            Self::IncludeSchema(chain, name) => match &chain.value {
                NameAccessChain_::One(_) => return None,
                NameAccessChain_::Two(m, _) => Some((m.loc, name.loc)),
                NameAccessChain_::Three(x, _) => Some((x.value.1.loc, name.loc)),
            },

            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum ItemOrAccess {
    Item(Item),
    Access(Access),
}

impl Into<Item> for ItemOrAccess {
    fn into(self) -> Item {
        match self {
            Self::Item(x) => x,
            _ => unreachable!(),
        }
    }
}

impl Into<Access> for ItemOrAccess {
    fn into(self) -> Access {
        match self {
            Self::Access(x) => x,
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for ItemOrAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Access(a) => a.fmt(f),
            Self::Item(x) => x.fmt(f),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MoveBuildInFun {
    MoveTo,
    MoveFrom,
    BorrowGlobalMut,
    BorrowGlobal,
    Exits,
}

impl MoveBuildInFun {
    fn to_static_str(self) -> &'static str {
        match self {
            MoveBuildInFun::MoveTo => "move_to",
            MoveBuildInFun::MoveFrom => "move_from",
            MoveBuildInFun::BorrowGlobalMut => "borrow_global_mut",
            MoveBuildInFun::BorrowGlobal => "borrow_global",
            MoveBuildInFun::Exits => "exists",
        }
    }

    pub(crate) fn to_notice(self) -> &'static str {
        match self {
            MoveBuildInFun::MoveTo => {
                r#"
                move_to<T>(&signer,T)
                Publish T under signer.address.
"#
            }
            MoveBuildInFun::MoveFrom => {
                r#"
                move_from<T>(address): T
                Remove T from address and return it.
"#
            }
            MoveBuildInFun::BorrowGlobalMut => {
                r#"
                borrow_global_mut<T>(address): &mut T
                Return a mutable reference to the T stored under address.
"#
            }
            MoveBuildInFun::BorrowGlobal => {
                r#"
                borrow_global<T>(address): &T
                Return an immutable reference to the T stored under address.
            "#
            }
            MoveBuildInFun::Exits => {
                r#"
                exists<T>(address): bool
                Return true if a T is stored under address.
            "#
            }
        }
    }
}

impl MoveBuildInFun {
    pub(crate) fn from_symbol(s: Symbol) -> Option<Self> {
        Self::from_str(s.as_str())
    }
    pub(crate) fn from_str(s: &str) -> Option<Self> {
        let x = match s {
            "move_to" => MoveBuildInFun::MoveTo,
            "move_from" => MoveBuildInFun::MoveFrom,
            "borrow_global_mut" => MoveBuildInFun::BorrowGlobalMut,
            "borrow_global" => MoveBuildInFun::BorrowGlobal,
            "exists" => MoveBuildInFun::Exits,
            _ => return None,
        };
        Some(x)
    }
    pub(crate) fn from_chain(s: &NameAccessChain) -> Option<Self> {
        match &s.value {
            NameAccessChain_::One(x) => Self::from_symbol(x.value),
            NameAccessChain_::Two(_, _) => return None,
            NameAccessChain_::Three(_, _) => return None,
        }
    }
}

impl std::fmt::Display for MoveBuildInFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpecBuildInFun {
    Exists,
    Global,
    Len,
    Update,
    Vec,
    Concat,
    Contains,
    IndexOf,
    Range,
    InRange,
    UpdateField,
    Old,
    TRACE,
    SpecDomain,
}

impl SpecBuildInFun {
    pub(crate) fn from_symbol(s: Symbol) -> Option<Self> {
        Self::from_str(s.as_str())
    }
    pub(crate) fn from_chain(s: &NameAccessChain) -> Option<Self> {
        match &s.value {
            NameAccessChain_::One(s) => Self::from_symbol(s.value),
            NameAccessChain_::Two(_, _) => return None,
            NameAccessChain_::Three(_, _) => return None,
        }
    }

    pub(crate) fn from_str(s: &str) -> Option<Self> {
        let x = match s {
            "exists" => Self::Exists,
            "global" => Self::Global,
            "len" => Self::Len,
            "update" => Self::Update,
            "vec" => Self::Vec,
            "concat" => Self::Concat,
            "contains" => Self::Contains,
            "index_of" => Self::IndexOf,
            "range" => Self::Range,
            "in_range" => Self::InRange,
            "update_field" => Self::UpdateField,
            "old" => Self::Old,
            "TRACE" => Self::TRACE,
            crate::module_visitor::SPEC_DOMAIN => Self::SpecDomain,
            _ => return None,
        };
        Some(x)
    }

    fn to_static_str(self) -> &'static str {
        match self {
            Self::Exists => "exists",
            Self::Global => "global",
            Self::Len => "len",
            Self::Update => "update",
            Self::Vec => "vec",
            Self::Concat => "concat",
            Self::Contains => "contains",
            Self::IndexOf => "index_of",
            Self::Range => "range",
            Self::InRange => "in_range",
            Self::UpdateField => "update_field",
            Self::Old => "old",
            Self::TRACE => "TRACE",
            Self::SpecDomain => crate::module_visitor::SPEC_DOMAIN,
        }
    }

    pub(crate) fn to_notice(self) -> &'static str {
        match self {
            SpecBuildInFun::Exists => {
                r#"
                exists<T>(address): bool 
                Returns true if the resource T exists at address.
                "#
            }
            SpecBuildInFun::Global => {
                r#"
                global<T>(address): 
                T returns the resource value at address."#
            }
            SpecBuildInFun::Len => {
                r#"
                len<T>(vector<T>): num 
                Returns the length of the vector."#
            }
            SpecBuildInFun::Update => {
                r#"
                update<T>(vector<T>, num, T>): vector<T> 
                Returns a new vector with the element replaced at the given index."#
            }
            SpecBuildInFun::Vec => {
                r#"
                vec<T>(): vector<T> 
                Returns an empty vector."#
            }
            SpecBuildInFun::Concat => {
                r#"
                concat<T>(vector<T>, vector<T>): vector<T> 
                Returns the concatenation of the parameters."#
            }
            SpecBuildInFun::Contains => {
                r#"
                contains<T>(vector<T>, T): bool 
                Returns true if element is in vector."#
            }
            SpecBuildInFun::IndexOf => {
                r#"index_of<T>(vector<T>, T): num 
                Returns the index of the element in the vector, or the length of the vector if it does not contain it."#
            }
            SpecBuildInFun::Range => {
                r#"
                range<T>(vector<T>): range 
                Returns the index range of the vector."#
            }
            SpecBuildInFun::InRange => {
                r#"
                in_range<T>(vector<T>, num): bool 
                Returns true if the number is in the index range of the vector."#
            }
            SpecBuildInFun::UpdateField => {
                r#"
                update_field(S, F, T): S 
                Updates a field in a struct, preserving the values of other fields, where S is some struct, F the name of a field in S, and T a value for this field."#
            }
            SpecBuildInFun::Old => {
                r#"
                old(T): T 
                T delivers the value of the passed argument at point of entry into a Move function. This is allowed in ensures post-conditions, inline spec blocks (with additional restrictions), and certain forms of invariants, as discussed later."#
            }
            SpecBuildInFun::TRACE => {
                r#"
                TRACE(T): T
                T is semantically the identity function and causes visualization of the argument's value in error messages created by the prover.
            "#
            }
            SpecBuildInFun::SpecDomain => r#""#,
        }
    }
}

impl std::fmt::Display for SpecBuildInFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}
