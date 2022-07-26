use move_ir_types::location::Loc;

use crate::parser::{
    ast::{BinOp_, QuantKind_, UnaryOp_},
    comments::Comment,
};

use super::super::lexer::Tok;

#[derive(Debug, Clone)]
pub struct Program {
    pub source_trees: Vec<ParsedTree>,
}

#[derive(Debug, Clone)]
pub struct SpannedWithComment<T> {
    pub token: T,
    pub range: Loc,
    pub leading_comments: Vec<Comment>,
    pub trailing_comments: Vec<Comment>,
}

pub type ParsedToken = SpannedWithComment<Tok>;
pub type Name = ParsedToken;

pub type ParsedAttributes = Vec<SpannedWithComment<ParsedAttribute>>;
pub type Ability = ParsedToken;
pub type Var = ParsedToken;
pub type Field = ParsedToken;

// (<Name>|<Num>)::<Name>::<Name>
pub type NameAccessChain = Vec<Name>;
pub type LeadingNameAccess = Name;
pub type PragmaProperty = ParsedToken;
pub type Visibility = ParsedToken;
pub type SpecApplyFragment = ParsedToken;

pub type QuantKind = SpannedWithComment<QuantKind_>;
pub type BinOp = SpannedWithComment<BinOp_>;
pub type UnaryOp = SpannedWithComment<UnaryOp_>;

#[derive(Debug, Clone)]
pub enum ParsedTree_ {
    Module {
        module_keyword: Name,
        address: Option<Name>,
        body: Vec<ParsedTree>,
    },
    Attributes {
        attrs: ParsedAttributes,
    },
    Address {
        address: LeadingNameAccess,
        modules: Vec<ParsedTree>,
    },
    Script {
        body: Vec<ParsedTree>,
    },
    Function {
        visibility: Name,
        signatures: FunctionSignature,
        acquire: Vec<NameAccessChain>,
        name: Name,
        body: FunctionBody,
        is_native: bool,
    },
    Struct {
        ability: Vec<Ability>,
        name: Name,
        type_parameters: Vec<StructTypeParameter>,
        fields: StructFields,
    },
    Sequence(SequenceItem),
    Spec(SpecBlock),
}

pub type ParsedTree = SpannedWithComment<ParsedTree_>;

#[derive(Debug, Clone)]
pub struct SpecBlock {
    pub target: SpecBlockTarget,
    pub uses: ParsedAttributes,
    pub members: Vec<SpecBlockMember>,
}

#[derive(Debug, Clone)]
pub enum SpecConditionKind {
    Condition(ParsedToken),
    Invariant(Vec<(Name, Vec<Ability>)>),
    InvariantUpdate(Vec<(Name, Vec<Ability>)>),
    Axiom(Vec<(Name, Vec<Ability>)>),
}

#[derive(Debug, Clone)]
pub enum SequenceItem {
    UseDecl(Use),
    FriendDecl(NameAccessChain),
    // let b
    Declare(BindList, Option<Type>),
    // let b : t = e;
    // let b = e;
    Bind(BindList, Option<Type>, Box<ParsedTree>),
    Constant {
        signature: Type,
        name: Name,
        exp: Exp,
    },
    Exp(Exp),
}

pub type Block = Vec<SpannedWithComment<SequenceItem>>;

#[derive(Debug, Clone)]
pub enum SpecBlockMember_ {
    Condition {
        kind: SpecConditionKind,
        properties: Vec<PragmaProperty>,
        exp: Exp,
        additional_exps: Vec<Exp>,
    },
    Function {
        uninterpreted: bool,
        name: Name,
        signature: FunctionSignature,
        body: FunctionBody,
    },
    Variable {
        is_global: bool,
        name: Name,
        type_parameters: Vec<(Name, Vec<Ability>)>,
        type_: Type,
        init: Option<Exp>,
    },
    Let {
        name: Name,
        post_state: bool,
        def: Exp,
    },
    Update {
        lhs: Exp,
        rhs: Exp,
    },
    Include {
        properties: Vec<PragmaProperty>,
        exp: Exp,
    },
    Apply {
        exp: Exp,
        patterns: Vec<SpecApplyPattern>,
        exclusion_patterns: Vec<SpecApplyPattern>,
    },
    Pragma {
        properties: Vec<PragmaProperty>,
    },
}

pub type SpecBlockMember = SpannedWithComment<SpecBlockMember_>;

#[derive(Debug, Clone)]
pub struct SpecApplyPattern_ {
    pub visibility: Option<Visibility>,
    pub name_pattern: Vec<SpecApplyPattern>,
    pub type_parameters: Vec<(Name, Vec<Ability>)>,
}
pub type SpecApplyPattern = SpannedWithComment<SpecApplyPattern_>;

#[derive(Debug, Clone)]
pub struct FunctionBody_ {
    pub body: Vec<ParsedTree>,
    pub is_semicolon_end: bool,
}

pub type FunctionBody = SpannedWithComment<FunctionBody_>;

#[derive(Debug, Clone)]
pub enum Exp_ {
    Value(ParsedToken),
    // [m::]n[<t1, .., tn>]
    Name(NameAccessChain, Option<Vec<Type>>),

    // f(earg,*)
    // f!(earg,*)
    Call(NameAccessChain, bool, Option<Vec<Type>>, Vec<Exp>),

    // tn {f1: e1, ... , f_n: e_n }
    Pack(NameAccessChain, Option<Vec<Type>>, Vec<(Field, Exp)>),

    // vector [ e1, ..., e_n ]
    // vector<t> [e1, ..., en ]
    Vector(/* name loc */ Loc, Option<Vec<Type>>, Vec<Exp>),

    // if (eb) et else ef
    IfElse(Box<Exp>, Box<Exp>, Option<Box<Exp>>),
    // while (eb) eloop
    While(Box<Exp>, Box<Exp>),
    // loop eloop
    Loop(Box<Exp>),

    // { seq }
    Block(Block),
    // fun (x1, ..., xn) e
    Lambda(BindList, Box<Exp>), // spec only
    // forall/exists x1 : e1, ..., xn [{ t1, .., tk } *] [where cond]: en.
    Quant(
        QuantKind,
        BindWithRangeList,
        Vec<Vec<Exp>>,
        Option<Box<Exp>>,
        Box<Exp>,
    ), // spec only
    // (e1, ..., en)
    ExpList(Vec<Exp>),
    // ()
    Unit,

    // a = e
    Assign(Box<Exp>, Box<Exp>),

    // return e
    Return(Option<Box<Exp>>),
    // abort e
    Abort(Box<Exp>),
    // break
    Break,
    // continue
    Continue,

    // *e
    Dereference(Box<Exp>),
    // op e
    UnaryExp(UnaryOp, Box<Exp>),
    // e1 op e2
    BinopExp(Box<Exp>, BinOp, Box<Exp>),

    // &e
    // &mut e
    Borrow(bool, Box<Exp>),

    // e.f
    Dot(Box<Exp>, Name),
    // e[e']
    Index(Box<Exp>, Box<Exp>), // spec only

    // (e as t)
    Cast(Box<Exp>, Type),
    // (e: t)
    Annotate(Box<Exp>, Type),

    // spec { ... }
    Spec(SpecBlock),

    // Internal node marking an error was added to the error list
    // This is here so the pass can continue even when an error is hit
    UnresolvedError,
}

pub type Exp = SpannedWithComment<Exp_>;

#[derive(Debug, Clone)]
pub enum Bind_ {
    Var(Var),
    Unpack(Box<NameAccessChain>, Option<Vec<Type>>, Vec<(Field, Bind)>),
}

pub type Bind = SpannedWithComment<Bind_>;

pub type BindList = Vec<Bind>;

pub type BindWithRange = (Bind, Exp);
pub type BindWithRangeList = Vec<BindWithRange>;
#[derive(Debug, Clone)]
pub enum SpecBlockTarget_ {
    Code,
    Module,
    Member(Name, Option<Box<FunctionSignature>>),
    Schema(Name, Vec<(Name, Vec<Ability>)>),
}

pub type SpecBlockTarget = SpannedWithComment<SpecBlockTarget_>;
#[derive(Debug, Clone)]
pub enum Use {
    Module(ParsedToken, Option<Name>),
    Members(ParsedToken, Vec<(Name, Option<Name>)>),
}

#[derive(Debug, Clone)]
pub enum StructFields {
    Defined(Vec<(Field, Type)>),
    Native(ParsedToken),
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub type_parameters: Vec<(Name, Vec<Ability>)>,
    pub parameters: Vec<(Var, Type)>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructTypeParameter {
    pub is_phantom: bool,
    pub name: Name,
    pub constraints: Vec<Ability>,
}

#[derive(Debug, Clone)]
pub enum Type {
    // N
    // N<t1, ... , tn>
    Apply(NameAccessChain, Vec<Type>),
    // &t
    // &mut t
    Ref(bool, Box<Type>),
    // (t1,...,tn):t
    Fun(Vec<Type>, Box<Type>),
    // ()
    Unit,
    // Used for return values and expression blocks
    Multiple(Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum ParsedAttributeValue {
    Value(ParsedToken),
    ModuleAccess(NameAccessChain),
}

#[derive(Debug, Clone)]
pub enum ParsedAttribute {
    Name(ParsedToken),
    Assigned(ParsedToken, ParsedAttributeValue),
    Parameterized(ParsedToken, ParsedAttributes),
}
