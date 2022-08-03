// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_command_line_common::files::FileHash;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;

use crate::{
    parser::{
        ast::{BinOp_, QuantKind_},
        comments::Comment,
        lexer::Tok,
    },
    shared::{NamedAddressMapIndex, NamedAddressMaps},
};

use super::{
    comments::CommentKind,
    lexer::{is_comment_spaces, Token},
    token_range::TokenRange,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub source_definitions: Vec<PackageDefinition>,
    pub lib_definitions: Vec<PackageDefinition>,
    pub named_address_maps: NamedAddressMaps,
}

#[derive(Debug, Clone)]
pub struct PackageDefinition {
    pub package: Option<Symbol>,
    pub named_address_map: NamedAddressMapIndex,
    pub source_trees: Vec<ParseTree>,
    pub source_tokens: Vec<Token>,
    pub file_hash: FileHash,
    pub path: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokensSpanned<T> {
    pub value: T,
    pub token_range: TokenRange,
}

impl<T> TokensSpanned<T> {
    pub fn new(value: T, token_range: TokenRange) -> Self {
        TokensSpanned { value, token_range }
    }

    pub fn loc(&self, tokens: &[Token]) -> Loc {
        self.token_range.loc(tokens)
    }

    pub fn full_loc(&self, tokens: &[Token]) -> Loc {
        self.token_range.full_loc(tokens)
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn trailing_comments(&self, source_tokens: &[Token]) -> Vec<Comment> {
        let end_pos = source_tokens[self.token_range.start..self.token_range.end]
            .iter()
            .rev()
            .position(|t| !is_comment_spaces(t.kind));
        match end_pos {
            Some(end) => {
                let trailing_tokens =
                    &source_tokens[self.token_range.end - end..self.token_range.end];
                trailing_tokens
                    .iter()
                    .filter_map(|t| match t.kind {
                        Tok::Comment(
                            kind @ CommentKind::DocComment | kind @ CommentKind::DocBlockComment,
                        ) => Some(Comment::new(kind, t.content, t.loc)),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }
            None => vec![],
        }
    }

    pub fn leading_tokens<'a, 'b>(&'a self, source_tokens: &'b [Token]) -> Option<&'b [Token]> {
        let end_pos = source_tokens[self.token_range.start..self.token_range.end]
            .iter()
            .position(|t| !is_comment_spaces(t.kind));
        end_pos.map(|end| &source_tokens[self.token_range.start..self.token_range.start + end])
    }
}

pub type ParsedToken = TokensSpanned<Token>;

pub type Name = ParsedToken;

pub type Attribute = TokensSpanned<Attribute_>;
pub type Attributes = TokensSpanned<Vec<Attribute>>;
pub type Ability = ParsedToken;
pub type Var = ParsedToken;
pub type Field = ParsedToken;
pub type Op = ParsedToken;

// (<Name>|<Num>)::<Name>::<Name>
pub type NameAccessChain = TokensSpanned<Vec<Name>>;
pub type LeadingNameAccess = Name;
pub type SpecApplyFragment = ParsedToken;

pub type QuantKind = TokensSpanned<QuantKind_>;
pub type BinOp = TokensSpanned<BinOp_>;
pub type UnaryOp = TokensSpanned<UnaryOp_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseTree {
    Module(Module),
    Script(Script),
    Address(Address),

    Function(Function),
    Struct(Struct),
    Attribute(Attributes),
    UseDecl(UseDecl),

    FriendDecl(FriendDecl),
    // let b;
    Declare(LetDeclare),
    // let b : t = e;
    // let b = e;
    LetAssign(LetAssign),
    Constant(Constant),
    // e
    // e;
    Exp(Exp, SemicolonEnd),

    // spec {}
    Spec(SpecBlock, SemicolonEnd),
    SpecMember(SpecMember),
}

impl ParseTree {
    pub fn loc(&self, tokens: &[Token]) -> Loc {
        match self {
            ParseTree::Module(i) => i.loc(tokens),
            ParseTree::Script(i) => i.loc(tokens),
            ParseTree::Address(i) => i.loc(tokens),
            ParseTree::Function(i) => i.loc(tokens),
            ParseTree::Struct(i) => i.loc(tokens),
            ParseTree::Attribute(i) => i.loc(tokens),
            ParseTree::UseDecl(i) => i.loc(tokens),
            ParseTree::FriendDecl(i) => i.loc(tokens),
            ParseTree::Declare(i) => i.loc(tokens),
            ParseTree::LetAssign(i) => i.loc(tokens),
            ParseTree::Constant(i) => i.loc(tokens),
            ParseTree::Exp(i, _) => i.loc(tokens),
            ParseTree::Spec(i, _) => i.loc(tokens),
            ParseTree::SpecMember(i) => i.loc(tokens),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Script_ {
    pub members: BlockSequence,
}

pub type Script = TokensSpanned<Script_>;

// { ParsedTree , ..... }
pub type BlockSequence = TokensSpanned<Vec<ParseTree>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module_ {
    pub address: Option<Name>,
    pub name: Name,
    pub body: BlockSequence,
}

pub type Module = TokensSpanned<Module_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Address_ {
    pub address: LeadingNameAccess,
    pub modules: BlockSequence,
}
pub type Address = TokensSpanned<Address_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function_ {
    pub modifiers: Modifiers,
    pub signatures: FunctionSignature,
    pub acquires: Vec<NameAccessChain>,
    pub name: Name,
    pub body: Option<FunctionBody>,
}

pub type Function = TokensSpanned<Function_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct_ {
    pub modifiers: Modifiers,
    pub abilities: Vec<Ability>,
    pub name: Name,
    pub type_parameters: Vec<StructTypeParameter>,
    pub fields: Option<StructFields>,
}

pub type Struct = TokensSpanned<Struct_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpecBlock_ {
    pub target: SpecBlockTarget,
    pub members: BlockSequence,
}

pub type SpecBlock = TokensSpanned<SpecBlock_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SingleSpecCondition {
    Assert,
    Assume,
    Ensures,
    Requires,
    Decreases,
    SucceedsIf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommaSpecCondition {
    AbortsWith,
    Modifies,
}

pub type SpecTypeParameters = TokensSpanned<Vec<(Name, Vec<Ability>)>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecConditionKind_ {
    // ("assert" | "assume" | "ensures" | "requires" | "decreases"| "succeeds_if" ) <ConditionProperties> <Exp> ";"
    SingleExpCondition {
        kind: TokensSpanned<SingleSpecCondition>,
        properties: Vec<PragmaProperty>,
        exp: Box<Exp>,
    },
    // "aborts_if" <ConditionProperties> <Exp> ["with" <Exp>] ";"
    AbortsIf {
        loc: ParsedToken,
        properties: Vec<PragmaProperty>,
        exp: Box<Exp>,
        with_exp: Option<Box<Exp>>,
    },
    //  "aborts_with" | "modifies" <ConditionProperties> [Comma <Exp>]* ";"
    CommaExpCondition {
        kind: TokensSpanned<CommaSpecCondition>,
        properties: Vec<PragmaProperty>,
        exps: Vec<Exp>,
    },
    //  "emits" <ConditionProperties> <Exp> "to" <Exp> [If <Exp>] ";"
    Emits {
        loc: ParsedToken,
        properties: Vec<PragmaProperty>,
        exp: Box<Exp>,
        to_exp: Box<Exp>,
        if_exp: Option<Box<Exp>>,
    },
    // "invariant" <OptionalTypeParameters> [ "update" ] <ConditionProperties> <Exp> ";"
    Invariant {
        types: SpecTypeParameters,
        properties: Vec<PragmaProperty>,
        exp: Box<Exp>,
    },
    InvariantUpdate {
        types: SpecTypeParameters,
        properties: Vec<PragmaProperty>,
        exp: Box<Exp>,
    },
    // "axiom" <OptionalTypeParameters> <ConditionProperties> <Exp> ";"
    Axiom {
        types: SpecTypeParameters,
        properties: Vec<PragmaProperty>,
        exp: Box<Exp>,
    },
}

pub type SpecConditionKind = TokensSpanned<SpecConditionKind_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDeclare_ {
    pub var: BindList,
    pub type_: Option<Type>,
}
pub type LetDeclare = TokensSpanned<LetDeclare_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetAssign_ {
    pub var: BindList,
    pub is_post: bool,
    pub type_: Option<Type>,
    pub exp: Exp,
}
pub type LetAssign = TokensSpanned<LetAssign_>;

pub type FriendDecl = TokensSpanned<NameAccessChain>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemicolonEnd {
    IsSemicolonEnd(Token),
    NotSemicolonEnd,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constant_ {
    pub signature: Type,
    pub name: Name,
    pub exp: Exp,
}

pub type Constant = TokensSpanned<Constant_>;

pub type SpecMember = TokensSpanned<SpecMember_>;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecMember_ {
    Condition(Box<SpecConditionKind>),
    // global expected_coin_sum: u64;
    Variable {
        is_global: bool,
        name: Name,
        type_parameters: Vec<(Name, Vec<Ability>)>,
        type_: Type,
        init: Option<Exp>,
    },
    //let post post_now = spec_now_microseconds();
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PragmaProperty_ {
    pub name: ParsedToken,
    pub value: Option<PragmaValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PragmaValue {
    Literal(Value),
    Ident(NameAccessChain),
}
pub type PragmaProperty = TokensSpanned<PragmaProperty_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpecApplyPattern_ {
    pub visibility: Option<Visibility>,
    pub name_pattern: Vec<Name>,
    pub type_parameters: Vec<(Name, Vec<Ability>)>,
}
pub type SpecApplyPattern = TokensSpanned<SpecApplyPattern_>;

pub type FunctionBody = BlockSequence;

#[derive(Debug, PartialEq, Copy, Clone, Eq)]
pub enum UnaryOp_ {
    // !
    Not,
    // &UnaryOp_
    Borrow,
    // &mut
    BorrowMut,
    // *
    Dereference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp_ {
    Value(Value),
    // [m::]n[<t1, .., tn>]
    Name(NameAccessChain, Option<Vec<Type>>),
    // move(x)
    Move(Var),
    // copy(x)
    Copy(Var),
    // f(earg,*)
    // f!(earg,*)
    Call(
        NameAccessChain,
        bool,
        Option<Vec<Type>>,
        TokensSpanned<Vec<Exp>>,
    ),

    // tn {f1: e1, ... , f_n: e_n }
    Pack(
        NameAccessChain,
        Option<Vec<Type>>,
        Vec<(Field, Option<Exp>)>,
    ),

    // vector [ e1, ..., e_n ]
    // vector<t> [e1, ..., en ]
    Vector(Name, Option<Vec<Type>>, TokensSpanned<Vec<Exp>>),

    // if (eb) et else ef
    IfElse(Box<Exp>, Box<Exp>, Option<Box<Exp>>),
    // while (eb) eloop (spec)
    While(Box<Exp>, Box<Exp>, Option<SpecBlock>),
    // loop eloop
    Loop(Box<Exp>),

    // { seq }
    Block(BlockSequence),
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

    // op e
    UnaryExp(UnaryOp, Box<Exp>),
    // e1 op e2
    BinopExp(Box<Exp>, BinOp, Box<Exp>),

    // e.f
    Dot(Box<Exp>, Name),
    // e[e']
    Index(Box<Exp>, Box<Exp>), // spec only

    // spec { .. }
    Spec(SpecBlock),

    // (e as t)
    Cast(Box<Exp>, Type),
    // (e: t)
    Annotate(Box<Exp>, Type),

    // Internal node marking an error was added to the error list
    // This is here so the pass can continue even when an error is hit
    UnresolvedError,
}

pub type Exp = TokensSpanned<Exp_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value_ {
    // @<num>
    Address(ParsedToken),
    // bool, x"[0..9A..F]+",
    // b"(<ascii> | \n | \r | \t | \\ | \0 | \" | \x[0..9A..F][0..9A..F])+"
    // <num>
    Literal(Token),
}
pub type Value = TokensSpanned<Value_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bind_ {
    Var(Var),
    // <NameAccessChain> <OptionalTypeArgs> "{" Comma<BindField> "}"
    Unpack(
        Box<NameAccessChain>,
        Option<Vec<Type>>,
        Vec<(Field, Option<Bind>)>,
    ),
}

pub type Bind = TokensSpanned<Bind_>;

pub type BindList = TokensSpanned<Vec<Bind>>;

pub type BindWithRange = TokensSpanned<QuantBind>;
pub type BindWithRangeList = TokensSpanned<Vec<BindWithRange>>;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecBlockTarget_ {
    Code,
    Module,
    Member(Name, Option<Box<FunctionSignature>>),
    Schema(Name, Vec<(Name, Vec<Ability>)>),
    IdentModule(Name, Name),
    Func,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QuantBind {
    // ident:type
    TypeBind(Var, Type),
    // i in Exp
    InBind(Var, Exp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleIdent {
    pub address: LeadingNameAccess,
    pub module: Name,
}

pub type SpecBlockTarget = TokensSpanned<SpecBlockTarget_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Use {
    Module(ModuleIdent, Option<Name>),
    Members(ModuleIdent, Vec<(Name, Option<Name>)>),
}

pub type UseDecl = TokensSpanned<Use>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructFields {
    pub members: Vec<(Field, Type)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub type_parameters: Vec<(Name, Vec<Ability>)>,
    pub parameters: Vec<(Var, Type)>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructTypeParameter {
    pub is_phantom: bool,
    pub name: Name,
    pub constraints: Vec<Ability>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type_ {
    // N
    // N<t1, ... , tn>
    Apply(NameAccessChain, Vec<Type>),
    // &t
    // &mut t
    Ref(bool, Box<Type>),
    // (t1,...,tn):t
    Fun(Vec<Type>, Box<Type>),
    // ()
    // (t1, t2, ... , tn)
    // Used for return values and expression blocks
    Sequance(Vec<Type>),
}

pub type Type = TokensSpanned<Type_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeValue {
    Value(Value),
    ModuleAccess(NameAccessChain),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute_ {
    Name(ParsedToken),
    Assigned(ParsedToken, AttributeValue),
    Parameterized(ParsedToken, Attributes),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility_ {
    Public,
    Script,
    Friend,
    Internal,
}

pub type Visibility = TokensSpanned<Visibility_>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier_ {
    Visibility(Visibility),
    Native,
    Entry,
}

pub type Modifier = TokensSpanned<Modifier_>;
pub type Modifiers = Vec<Modifier>;
