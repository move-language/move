// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{mem, vec};

use move_command_line_common::address::NumericalAddress;
use move_ir_types::location::{sp, Loc};
use move_symbol_pool::Symbol;

use super::{
    ast::{self, Modifiers, Visibility, ENTRY_MODIFIER},
    cst::{self, TokensSpanned},
    lexer::Tok,
};
use crate::{
    attr_derivation, diag,
    diagnostics::{
        codes::{Declarations, Syntax},
        Diagnostic,
    },
    shared::{CompilationEnv, Identifier, Name},
};
pub struct Context<'env> {
    env: &'env mut CompilationEnv,
}

impl<'env> Context<'env> {
    pub fn new(env: &'env mut CompilationEnv) -> Self {
        Self { env }
    }
}

fn make_builtin_call(
    loc: Loc,
    name: Symbol,
    type_args: Option<Vec<ast::Type>>,
    args: Vec<ast::Exp>,
) -> ast::Exp {
    let maccess = sp(loc, ast::NameAccessChain_::One(sp(loc, name)));
    sp(
        loc,
        ast::Exp_::Call(maccess, false, type_args, sp(loc, args)),
    )
}

fn sp_between_token(first: &cst::ParsedToken, second: &cst::ParsedToken) -> Loc {
    Loc::new(
        first.value.loc.file_hash(),
        first.value.loc.start(),
        second.value.loc.end(),
    )
}

fn invalid_declaration<T, R>(
    ast: &cst::PackageDefinition,
    statement: &TokensSpanned<T>,
    error_kind: Declarations,
    item_description: &str,
) -> Result<R, Diagnostic> {
    let loc = statement.loc(&ast.source_tokens);
    Err(diag!(error_kind, (loc, item_description)))
}

fn invalid_syntax<T, R>(
    ast: &cst::PackageDefinition,
    statement: &TokensSpanned<T>,
    error_kind: Syntax,
    item_description: &str,
) -> Result<R, Diagnostic> {
    let loc = statement.loc(&ast.source_tokens);
    Err(diag!(error_kind, (loc, item_description)))
}

pub fn translate_parsetree_to_def(
    context: &mut Context,
    ast: &cst::PackageDefinition,
) -> Result<Vec<ast::PackageDefinition>, Diagnostic> {
    let mut attributes = vec![];
    let mut defs = vec![];
    for tree in &ast.source_trees[..] {
        match tree {
            cst::ParseTree::Module(module) => {
                let attrs = mem::take(&mut attributes);
                let module_res = translate_module(context, ast, module, attrs)?;
                defs.push(ast::PackageDefinition {
                    package: ast.package,
                    named_address_map: ast.named_address_map,
                    def: ast::Definition::Module(module_res),
                })
            }
            cst::ParseTree::Script(script) => {
                let attrs = mem::take(&mut attributes);
                let script_res = translate_script(context, ast, script, attrs)?;
                defs.push(ast::PackageDefinition {
                    package: ast.package,
                    named_address_map: ast.named_address_map,
                    def: ast::Definition::Script(script_res),
                })
            }
            cst::ParseTree::Address(address) => {
                let attrs = mem::take(&mut attributes);
                let script_res = translate_address(context, ast, address, attrs)?;
                defs.push(ast::PackageDefinition {
                    package: ast.package,
                    named_address_map: ast.named_address_map,
                    def: ast::Definition::Address(script_res),
                })
            }
            cst::ParseTree::Function(function) => {
                return invalid_declaration(
                    ast,
                    function,
                    Declarations::InvalidFunction,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'fun'",
                )
            }
            cst::ParseTree::Struct(script) => {
                return invalid_declaration(
                    ast,
                    script,
                    Declarations::InvalidStruct,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'struct'",
                )
            }
            cst::ParseTree::Attribute(attrs) => {
                attributes.push(translate_attributes(context, ast, attrs)?)
            }
            cst::ParseTree::UseDecl(use_) => {
                return invalid_declaration(
                    ast,
                    use_,
                    Declarations::InvalidUse,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'use'",
                )
            }
            cst::ParseTree::FriendDecl(friend) => {
                return invalid_declaration(
                    ast,
                    friend,
                    Declarations::InvalidFriendDeclaration,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'friend'",
                )
            }
            cst::ParseTree::Declare(decl) => {
                return invalid_declaration(
                    ast,
                    decl,
                    Declarations::InvalidLet,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'let'",
                )
            }
            cst::ParseTree::LetAssign(bind) => {
                return invalid_declaration(
                    ast,
                    bind,
                    Declarations::InvalidLet,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'let'",
                )
            }
            cst::ParseTree::Constant(constant) => {
                return invalid_declaration(
                    ast,
                    constant,
                    Declarations::InvalidConstant,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got 'const'",
                )
            }
            cst::ParseTree::Exp(exp, _) => {
                return invalid_declaration(
                    ast,
                    exp,
                    Declarations::InvalidExpression,
                    "Invalid code unit. Expected 'address', 'module', or 'script'. Got expression",
                )
            }
            cst::ParseTree::Spec(spec, _) => match spec.value().target.value() {
                cst::SpecBlockTarget_::IdentModule(leading, module_name) => {
                    let attrs = mem::take(&mut attributes);
                    let members = translate_module_members(context, ast, &spec.value().members)?;
                    let loc = spec.loc(&ast.source_tokens);
                    let name = translate_module_name(ast, module_name)?;
                    let address = Some(translate_token_to_leading_name_access(
                        context, ast, leading,
                    )?);
                    defs.push(ast::PackageDefinition {
                        package: ast.package,
                        named_address_map: ast.named_address_map,
                        def: ast::Definition::Module(ast::ModuleDefinition {
                            attributes: attrs,
                            loc,
                            address,
                            name,
                            is_spec_module: true,
                            members,
                        }),
                    })
                }
                _ => {
                    return invalid_declaration(
                        ast,
                        spec,
                        Declarations::InvalidSpec,
                        "'spec' is not allowed in top definition",
                    )
                }
            },
            cst::ParseTree::SpecMember(member) => return invalid_declaration(
                ast,
                member,
                Declarations::InvalidSpecStatement,
                "Invalid code unit. Expected 'address', 'module', or 'script'. Got spec member.",
            ),
        }
    }
    Ok(defs)
}

pub fn translate_token_to_name(
    ast: &cst::PackageDefinition,
    token: &cst::ParsedToken,
) -> Result<Name, Diagnostic> {
    let loc = token.token_range.loc(&ast.source_tokens);

    if token.value.kind != Tok::Identifier {
        return Err(unexpected_token_error(loc, "an identifier", ""));
    }
    let symbol = token.value.content;
    Ok(sp(loc, symbol))
}

pub fn translate_token_to_leading_name_access(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    token: &cst::ParsedToken,
) -> Result<ast::LeadingNameAccess, Diagnostic> {
    let loc = token.token_range.loc(&ast.source_tokens);
    match token.value.kind {
        Tok::Identifier => {
            let n = translate_token_to_name(ast, token)?;
            Ok(sp(loc, ast::LeadingNameAccess_::Name(n)))
        }
        Tok::NumValue => {
            let (loc, addr) = translate_token_to_address_byte(context, ast, token)?;
            Ok(sp(loc, ast::LeadingNameAccess_::AnonymousAddress(addr)))
        }
        _ => Err(unexpected_token_error(
            loc,
            "an address or an identifier",
            "",
        )),
    }
}

fn translate_token_to_address_byte(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    token: &cst::ParsedToken,
) -> Result<(Loc, NumericalAddress), Diagnostic> {
    let loc = token.token_range.loc(&ast.source_tokens);
    let addr_res = NumericalAddress::parse_str(token.value.content.as_str());
    match addr_res {
        Ok(addr_) => Ok((loc, addr_)),
        Err(msg) => {
            context
                .env
                .add_diag(diag!(Syntax::InvalidAddress, (loc, msg)));
            Ok((loc, NumericalAddress::DEFAULT_ERROR_ADDRESS))
        }
    }
}

pub fn translate_value(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    token: &cst::Value,
) -> Result<ast::Value, Diagnostic> {
    let loc = token.token_range.loc(&ast.source_tokens);

    let value = match token.value() {
        cst::Value_::Address(t) => {
            ast::Value_::Address(translate_token_to_leading_name_access(context, ast, t)?)
        }
        cst::Value_::Literal(t) => match t.kind {
            Tok::True => ast::Value_::Bool(true),
            Tok::False => ast::Value_::Bool(false),
            Tok::NumValue => {
                let num = t.content;
                ast::Value_::Num(num)
            }
            Tok::NumTypedValue => {
                let num = t.content;
                ast::Value_::Num(num)
            }

            Tok::ByteStringValue => parse_byte_string(&t.content)?,
            _ => {
                return Err(unexpected_token_error(
                    loc,
                    "parse_value called with invalid token",
                    "",
                ))
            }
        },
    };
    Ok(sp(loc, value))
}

pub fn unexpected_token_error(loc: Loc, unexpected: &str, expected: &str) -> Diagnostic {
    diag!(
        Syntax::UnexpectedToken,
        (loc, format!("Unexpected {}", unexpected)),
        (loc, format!("Expected {}", expected)),
    )
}

// Parse a byte string:
//      ByteString = <ByteStringValue>
fn parse_byte_string(s: &str) -> Result<ast::Value_, Diagnostic> {
    let text = Symbol::from(&s[2..s.len() - 1]);
    let value_ = if s.starts_with("x\"") {
        ast::Value_::HexString(text)
    } else {
        assert!(s.starts_with("b\""));
        ast::Value_::ByteString(text)
    };
    Ok(value_)
}

pub fn translate_name_access_chain(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    names: &cst::NameAccessChain,
) -> Result<ast::NameAccessChain, Diagnostic> {
    let loc = names.token_range.loc(&ast.source_tokens);
    match &names.value[..] {
        [fir] => {
            let name = match fir.value.kind {
                Tok::Identifier => ast::NameAccessChain_::One(translate_token_to_name(ast, fir)?),
                Tok::NumValue => {
                    return Err(unexpected_token_error(
                        Loc::loc_end(&loc),
                        "';'",
                        "'::' after an address in a module access chain",
                    ));
                }
                _ => {
                    return Err(unexpected_token_error(
                        loc,
                        format!("{}", fir.value().content).as_str(),
                        "",
                    ))
                }
            };
            Ok(sp(loc, name))
        }
        [fir, sec] => {
            let leading = translate_token_to_leading_name_access(context, ast, fir)?;
            let name = translate_token_to_name(ast, sec)?;
            Ok(sp(loc, ast::NameAccessChain_::Two(leading, name)))
        }
        [fir, sec, third] => {
            let leading = translate_token_to_leading_name_access(context, ast, fir)?;
            let sec_name = translate_token_to_name(ast, sec)?;
            let third_name = translate_token_to_name(ast, third)?;
            let span_fir_sec = sp_between_token(fir, sec);
            Ok(sp(
                loc,
                ast::NameAccessChain_::Three(sp(span_fir_sec, (leading, sec_name)), third_name),
            ))
        }
        _ => Err(diag!(
            Declarations::InvalidAddress,
            (
                loc,
                "The amount of names of access chain could not exceed 3."
            )
        )),
    }
}

pub fn translate_ability(
    ast: &cst::PackageDefinition,
    token: &cst::Name,
) -> Result<ast::Ability, Diagnostic> {
    let loc = token.loc(&ast.source_tokens);
    let value = match token.value().kind {
        Tok::Copy => ast::Ability_::Copy,
        Tok::Identifier if token.value().content.as_str() == ast::Ability_::DROP => {
            ast::Ability_::Drop
        }
        Tok::Identifier if token.value().content.as_str() == ast::Ability_::STORE => {
            ast::Ability_::Store
        }
        Tok::Identifier if token.value().content.as_str() == ast::Ability_::KEY => {
            ast::Ability_::Key
        }
        _ => {
            let msg = format!(
                "Unexpected {}. Expected a type ability, one of: 'copy', 'drop', 'store', or 'key'",
                token.value().content
            );
            return Err(diag!(Syntax::UnexpectedToken, (loc, msg),));
        }
    };
    Ok(sp(loc, value))
}

fn translate_field(
    ast: &cst::PackageDefinition,
    field: &cst::Field,
) -> Result<ast::Field, Diagnostic> {
    Ok(ast::Field(translate_token_to_name(ast, field)?))
}

pub fn translate_var(
    ast: &cst::PackageDefinition,
    token: &cst::Name,
) -> Result<ast::Var, Diagnostic> {
    let name = translate_token_to_name(ast, token)?;
    Ok(ast::Var(name))
}

pub fn translate_address(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    address: &cst::Address,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::AddressDefinition, Diagnostic> {
    let loc = address.loc(&ast.source_tokens);

    let addr = translate_token_to_leading_name_access(context, ast, &address.value().address)?;
    let mut internal_attrs = vec![];
    let mut modules = vec![];
    for tree in address.value().modules.value() {
        match tree {
            cst::ParseTree::Module(module) => {
                let attrs = mem::take(&mut internal_attrs);
                modules.push(translate_module(context, ast, module, attrs)?);
            }
            cst::ParseTree::Attribute(attribute) => {
                internal_attrs.push(translate_attributes(context, ast, attribute)?)
            }

            _ => return Err(diag!(Declarations::InvalidAddress, (loc, ""))),
        }
    }

    Ok(ast::AddressDefinition {
        attributes,
        loc,
        addr,
        modules,
    })
}

pub fn translate_attributes(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    attrs: &cst::Attributes,
) -> Result<ast::Attributes, Diagnostic> {
    let loc = attrs.loc(&ast.source_tokens);
    let target_attrs: Result<Vec<ast::Attribute>, Diagnostic> = attrs
        .value()
        .iter()
        .map(|a| {
            let loc = a.loc(&ast.source_tokens);
            let attr_ = match a.value() {
                cst::Attribute_::Name(n) => {
                    let name = translate_token_to_name(ast, n)?;
                    ast::Attribute_::Name(name)
                }
                cst::Attribute_::Assigned(n, value) => {
                    let name = translate_token_to_name(ast, n)?;
                    let values = translate_attribute_value(context, ast, value)?;

                    ast::Attribute_::Assigned(name, Box::new(values))
                }
                cst::Attribute_::Parameterized(n, attributes) => {
                    let name = translate_token_to_name(ast, n)?;
                    let attributes_ = translate_attributes(context, ast, attributes)?;
                    ast::Attribute_::Parameterized(name, attributes_)
                }
            };
            Ok(sp(loc, attr_))
        })
        .collect();
    Ok(sp(loc, target_attrs?))
}

fn translate_attribute_value(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    attr_value: &cst::AttributeValue,
) -> Result<ast::AttributeValue, Diagnostic> {
    match attr_value {
        cst::AttributeValue::Value(v) => {
            let loc = v.loc(&ast.source_tokens);
            Ok(sp(
                loc,
                ast::AttributeValue_::Value(translate_value(context, ast, v)?),
            ))
        }
        cst::AttributeValue::ModuleAccess(n) => {
            let names = translate_name_access_chain(context, ast, n)?;
            Ok(sp(names.loc, ast::AttributeValue_::ModuleAccess(names)))
        }
    }
}

pub fn translate_constant(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    constant: &cst::Constant,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::Constant, Diagnostic> {
    let loc = constant.loc(&ast.source_tokens);
    let constant_value = constant.value();
    let signature = translate_type(context, ast, &constant_value.signature)?;
    let name = translate_constant_name(ast, &constant_value.name)?;
    let value = translate_exp(context, ast, &constant_value.exp)?;

    Ok(ast::Constant {
        attributes,
        loc,
        signature,
        name,
        value: *value,
    })
}

fn translate_constant_name(
    ast: &cst::PackageDefinition,
    name: &cst::Name,
) -> Result<ast::ConstantName, Diagnostic> {
    Ok(ast::ConstantName(translate_token_to_name(ast, name)?))
}

fn translate_exp(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    exp: &cst::Exp,
) -> Result<Box<ast::Exp>, Diagnostic> {
    let loc = exp.loc(&ast.source_tokens);
    let exp_ = match exp.value() {
        cst::Exp_::Value(val) => Ok(ast::Exp_::Value(translate_value(context, ast, val)?)),
        cst::Exp_::Move(var) => Ok(ast::Exp_::Move(translate_var(ast, var)?)),
        cst::Exp_::Copy(var) => Ok(ast::Exp_::Copy(translate_var(ast, var)?)),
        cst::Exp_::Name(names, types) => {
            let (names_res, types_res) = translate_exp_name(context, ast, names, types)?;
            Ok(ast::Exp_::Name(names_res, types_res))
        }
        cst::Exp_::Call(names, is_macro, type_, e) => {
            let args_loc = e.loc(&ast.source_tokens);
            let (name, type_res, exp_res) =
                translate_exp_call(context, ast, names, type_, e.value())?;
            Ok(ast::Exp_::Call(
                name,
                *is_macro,
                type_res,
                sp(args_loc, exp_res),
            ))
        }
        cst::Exp_::Pack(name, types, fields) => {
            translate_exp_pack(context, ast, name, types, fields)
        }
        cst::Exp_::Vector(name, type_, contents) => {
            translate_exp_vector(context, ast, name, type_, contents)
        }
        cst::Exp_::IfElse(cond, if_statement, else_statement) => {
            translate_exp_if_else(context, ast, cond, if_statement, else_statement)
        }
        cst::Exp_::While(cond, body, spec_opt) => {
            translate_exp_while(context, ast, cond, body, spec_opt)
        }
        cst::Exp_::Loop(body) => translate_exp_loop(context, ast, body),
        cst::Exp_::Block(block) => translate_exp_block(context, ast, block),
        cst::Exp_::Lambda(bind_list, body) => {
            let bind_list_res = translate_bind_list(context, ast, bind_list)?;
            let body_res = translate_exp(context, ast, body)?;
            Ok(ast::Exp_::Lambda(bind_list_res, body_res))
        }
        cst::Exp_::Quant(kind, binds, exps, where_cond, exp) => {
            translate_quant(context, ast, kind, binds, exps, where_cond, exp)
        }
        cst::Exp_::ExpList(exps) => {
            let exps_res = exps
                .iter()
                .map(|e| translate_exp(context, ast, e).map(|r| *r))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ast::Exp_::ExpList(exps_res))
        }
        cst::Exp_::Unit => Ok(ast::Exp_::Unit),
        cst::Exp_::Assign(var, body) => {
            let var_res = translate_exp(context, ast, var)?;
            let body_res = translate_exp(context, ast, body)?;
            Ok(ast::Exp_::Assign(var_res, body_res))
        }
        cst::Exp_::Return(exp_opt) => match exp_opt {
            Some(exp) => {
                let exp_res = translate_exp(context, ast, exp)?;
                Ok(ast::Exp_::Return(Some(exp_res)))
            }
            _ => Ok(ast::Exp_::Return(None)),
        },
        cst::Exp_::Abort(e) => {
            let exp_res = translate_exp(context, ast, e)?;
            Ok(ast::Exp_::Abort(exp_res))
        }
        cst::Exp_::Break => Ok(ast::Exp_::Break),
        cst::Exp_::Continue => Ok(ast::Exp_::Continue),
        cst::Exp_::UnaryExp(op, e) => translate_unary_exp(context, ast, op, e),
        cst::Exp_::BinopExp(lhs, op, rhs) => translate_binop_exp(context, ast, op, lhs, rhs),
        cst::Exp_::Dot(e, name) => {
            let exp_res = translate_exp(context, ast, e)?;
            let name = translate_token_to_name(ast, name)?;
            Ok(ast::Exp_::Dot(exp_res, name))
        }
        cst::Exp_::Index(e, exp_in) => {
            let exp_res = translate_exp(context, ast, e)?;
            let exp_in_res = translate_exp(context, ast, exp_in)?;
            Ok(ast::Exp_::Index(exp_res, exp_in_res))
        }
        cst::Exp_::Spec(spec_block) => Ok(ast::Exp_::Spec(translate_spec_block(
            context,
            ast,
            spec_block,
            vec![],
        )?)),
        cst::Exp_::Cast(e, type_) => {
            let exp_res = translate_exp(context, ast, e)?;
            let type_res = translate_type(context, ast, type_)?;
            Ok(ast::Exp_::Cast(exp_res, type_res))
        }
        cst::Exp_::Annotate(e, type_) => {
            let exp_res = translate_exp(context, ast, e)?;
            let type_res = translate_type(context, ast, type_)?;
            Ok(ast::Exp_::Annotate(exp_res, type_res))
        }
        cst::Exp_::UnresolvedError => Ok(ast::Exp_::UnresolvedError),
    }?;

    Ok(Box::new(sp(loc, exp_)))
}

fn translate_quant(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    quant: &cst::QuantKind,
    binds: &cst::BindWithRangeList,
    exps: &[Vec<cst::Exp>],
    where_cond: &Option<Box<cst::Exp>>,
    exp: &cst::Exp,
) -> Result<ast::Exp_, Diagnostic> {
    let kind = translate_quant_kind(ast, quant)?;
    let binds_res = transalte_quant_bind_list(context, ast, binds)?;
    let exps_res = exps
        .iter()
        .map(|exps_| {
            exps_
                .iter()
                .map(|e| translate_exp(context, ast, e).map(|r| *r))
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;
    let where_cond_res = match where_cond {
        Some(e) => Ok(Some(translate_exp(context, ast, e)?)),
        None => Ok(None),
    }?;
    let exp_res = translate_exp(context, ast, exp)?;
    Ok(ast::Exp_::Quant(
        kind,
        binds_res,
        exps_res,
        where_cond_res,
        exp_res,
    ))
}

fn transalte_quant_bind_list(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    binds: &cst::BindWithRangeList,
) -> Result<ast::BindWithRangeList, Diagnostic> {
    let loc = binds.loc(&ast.source_tokens);
    let res = binds
        .value()
        .iter()
        .map(|b| {
            let loc = b.loc(&ast.source_tokens);
            match b.value() {
                cst::QuantBind::TypeBind(var, type_) => {
                    let var_res = ast::Bind_::Var(translate_var(ast, var)?);
                    let bind_res = sp(var.loc(&ast.source_tokens), var_res);
                    let type_res = translate_type(context, ast, type_)?;
                    let exp = make_builtin_call(
                        type_res.loc,
                        Symbol::from("$spec_domain"),
                        Some(vec![type_res]),
                        vec![],
                    );
                    Ok(sp(loc, (bind_res, exp)))
                }
                cst::QuantBind::InBind(var, exp) => {
                    let var_res = ast::Bind_::Var(translate_var(ast, var)?);
                    let bind_res = sp(var.loc(&ast.source_tokens), var_res);
                    let exp_res = translate_exp(context, ast, exp)?;
                    Ok(sp(loc, (bind_res, *exp_res)))
                }
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(sp(loc, res))
}

fn translate_quant_kind(
    ast: &cst::PackageDefinition,
    quant_kind: &cst::QuantKind,
) -> Result<ast::QuantKind, Diagnostic> {
    let loc = quant_kind.loc(&ast.source_tokens);
    let kind = quant_kind.value;
    Ok(sp(loc, kind))
}

fn translate_binop_exp(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    op: &cst::BinOp,
    lhs: &cst::Exp,
    rhs: &cst::Exp,
) -> Result<ast::Exp_, Diagnostic> {
    let lhs_res = translate_exp(context, ast, lhs)?;
    let rhs_res = translate_exp(context, ast, rhs)?;
    let op_loc = op.loc(&ast.source_tokens);

    Ok(ast::Exp_::BinopExp(lhs_res, sp(op_loc, op.value), rhs_res))
}

fn translate_unary_exp(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    op: &cst::UnaryOp,
    exp: &cst::Exp,
) -> Result<ast::Exp_, Diagnostic> {
    let exp_res = translate_exp(context, ast, exp)?;
    let op_loc = op.loc(&ast.source_tokens);
    match op.value() {
        cst::UnaryOp_::Not => Ok(ast::Exp_::UnaryExp(sp(op_loc, ast::UnaryOp_::Not), exp_res)),
        cst::UnaryOp_::Borrow => Ok(ast::Exp_::Borrow(false, exp_res)),
        cst::UnaryOp_::BorrowMut => Ok(ast::Exp_::Borrow(true, exp_res)),
        cst::UnaryOp_::Dereference => Ok(ast::Exp_::Dereference(exp_res)),
    }
}

fn translate_exp_block(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    block: &cst::BlockSequence,
) -> Result<ast::Exp_, Diagnostic> {
    let res = translate_exp_sequence(context, ast, block)?;
    Ok(ast::Exp_::Block(res))
}

fn check_invalid_attributes(
    attributes: &[ast::Attributes],
    item_description: &str,
) -> Result<(), Diagnostic> {
    if attributes.is_empty() {
        Ok(())
    } else {
        let locs = attributes.iter().map(|a| a.loc).collect::<Vec<Loc>>();
        let loc = Loc::between(locs.first().unwrap(), locs.last().unwrap());
        Err(diag!(
            Declarations::InvalidAttribute,
            (loc, item_description)
        ))
    }
}

fn translate_exp_sequence(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    block: &cst::BlockSequence,
) -> Result<ast::Sequence, Diagnostic> {
    let mut uses = vec![];
    let mut sequences: Vec<ast::SequenceItem> = vec![];
    let mut end_semicolon: Option<Loc> = None;
    let mut end_exp: Option<ast::Exp> = None;
    let mut internal_attrs: Vec<ast::Attributes> = vec![];
    let mut block_iter = block.value().iter().peekable();
    while let Some(item) = block_iter.next() {
        match item {
            cst::ParseTree::Declare(decl) => {
                check_invalid_attributes(&internal_attrs, "attribute before let is not valid")?;
                sequences.push(sp(
                    decl.loc(&ast.source_tokens),
                    translate_let_declare(context, ast, decl)?,
                ))
            }
            cst::ParseTree::LetAssign(bind) => {
                check_invalid_attributes(&internal_attrs, "attribute before let is not valid")?;
                sequences.push(sp(
                    bind.loc(&ast.source_tokens),
                    translate_let_assign(context, ast, bind)?,
                ))
            }
            cst::ParseTree::UseDecl(use_) => {
                let attrs = mem::take(&mut internal_attrs);
                uses.push(translate_use_decl(context, ast, use_, attrs)?)
            }
            cst::ParseTree::Exp(exp, semicolon_end) => match (block_iter.peek(), semicolon_end) {
                (_, cst::SemicolonEnd::IsSemicolonEnd(tok)) => {
                    end_semicolon = Some(tok.loc);
                    sequences.push(sp(
                        exp.loc(&ast.source_tokens),
                        ast::SequenceItem_::Seq(translate_exp(context, ast, exp)?),
                    ))
                }
                (None, cst::SemicolonEnd::NotSemicolonEnd) => {
                    end_exp = Some(*translate_exp(context, ast, exp)?);
                }
                (Some(_), cst::SemicolonEnd::NotSemicolonEnd) => {
                    return Err(diag!(
                        Declarations::InvalidExpression,
                        (
                            exp.loc(&ast.source_tokens),
                            "expected ';' at the end of expression"
                        )
                    ))
                }
            },
            cst::ParseTree::Spec(spec_block, _) => {
                let attrs = mem::take(&mut internal_attrs);
                let spec_res = translate_spec_block(context, ast, spec_block, attrs)?;
                sequences.push(sp(
                    spec_block.loc(&ast.source_tokens),
                    ast::SequenceItem_::Seq(Box::new(sp(
                        spec_block.loc(&ast.source_tokens),
                        ast::Exp_::Spec(spec_res),
                    ))),
                ))
            }
            cst::ParseTree::Attribute(attrs) => {
                internal_attrs.push(translate_attributes(context, ast, attrs)?)
            }

            cst::ParseTree::Module(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidModule,
                    "'module' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::Script(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidScript,
                    "'script' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::Address(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidAddress,
                    "'address' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::Function(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidFunction,
                    "'fun' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::Struct(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidStruct,
                    "'struct' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::FriendDecl(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidFriendDeclaration,
                    "'friend' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::Constant(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidConstant,
                    "'const' is not allowed in expression sequence",
                )
            }
            cst::ParseTree::SpecMember(s) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidSpec,
                    "'spec' is not allowed in expression sequence",
                )
            }
        }
    }
    Ok((uses, sequences, end_semicolon, Box::new(end_exp)))
}

fn translate_exp_loop(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    body: &cst::Exp,
) -> Result<ast::Exp_, Diagnostic> {
    let body_res = translate_exp(context, ast, body)?;
    Ok(ast::Exp_::Loop(body_res))
}

fn translate_exp_while(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    cond: &cst::Exp,
    body: &cst::Exp,
    spec_opt: &Option<cst::SpecBlock>,
) -> Result<ast::Exp_, Diagnostic> {
    let cond_res = translate_exp(context, ast, cond)?;
    let loop_res = translate_exp(context, ast, body)?;
    let econd = match spec_opt {
        Some(spec) => {
            for member in spec.value().members.value() {
                match member {
                    cst::ParseTree::SpecMember(
                        val @ TokensSpanned {
                            value: cst::SpecMember_::Condition(kind),
                            ..
                        },
                    ) => match kind.value() {
                        cst::SpecConditionKind_::Invariant { .. } => {}
                        _ => {
                            return Err(diag!(
                                Syntax::InvalidSpecBlockMember,
                                (val.loc(&ast.source_tokens), "only 'invariant' allowed here")
                            ))
                        }
                    },
                    _ => {
                        return Err(diag!(
                            Syntax::InvalidSpecBlockMember,
                            (
                                member.loc(&ast.source_tokens),
                                "only 'invariant' allowed here"
                            )
                        ))
                    }
                }
            }
            let spec_loc = spec.loc(&ast.source_tokens);
            let spec_res = translate_spec_block(context, ast, spec, vec![])?;
            Ok(sp(
                cond_res.loc,
                ast::Exp_::Block((
                    vec![],
                    vec![sp(
                        spec_loc,
                        ast::SequenceItem_::Seq(Box::new(sp(spec_loc, ast::Exp_::Spec(spec_res)))),
                    )],
                    None,
                    Box::new(Some(*cond_res)),
                )),
            ))
        }
        None => Ok(*cond_res),
    }?;

    Ok(ast::Exp_::While(Box::new(econd), loop_res))
}

fn translate_exp_if_else(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    cond: &cst::Exp,
    if_statement: &cst::Exp,
    else_statement: &Option<Box<cst::Exp>>,
) -> Result<ast::Exp_, Diagnostic> {
    let cond_res = translate_exp(context, ast, cond)?;
    let if_statement_res = translate_exp(context, ast, if_statement)?;
    let else_statement_res = match else_statement {
        Some(el) => Some(translate_exp(context, ast, el)?),
        None => None,
    };
    Ok(ast::Exp_::IfElse(
        cond_res,
        if_statement_res,
        else_statement_res,
    ))
}

fn translate_exp_vector(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    name: &cst::Name,
    type_: &Option<Vec<cst::Type>>,
    contents: &cst::TokensSpanned<Vec<cst::Exp>>,
) -> Result<ast::Exp_, Diagnostic> {
    let name_loc = name.loc(&ast.source_tokens);
    let type_res = translate_optional_types(context, ast, type_)?;
    let content_loc = contents.loc(&ast.source_tokens);
    let content_res = contents
        .value()
        .iter()
        .map(|e| translate_exp(context, ast, e).map(|r| *r))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ast::Exp_::Vector(
        name_loc,
        type_res,
        sp(content_loc, content_res),
    ))
}

fn translate_exp_pack(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    leading: &cst::NameAccessChain,
    type_: &Option<Vec<cst::Type>>,
    fields: &[(cst::Field, Option<cst::Exp>)],
) -> Result<ast::Exp_, Diagnostic> {
    let leading_name = translate_name_access_chain(context, ast, leading)?;
    let type_res = translate_optional_types(context, ast, type_)?;
    let fields_res = fields
        .iter()
        .map(|(f, e)| {
            let field_res = translate_field(ast, f)?;
            let exp_res = match e {
                Some(exp_) => translate_exp(context, ast, exp_).map(|r| *r)?,
                // field only situation
                None => {
                    let f_loc = field_res.0.loc;
                    sp(
                        f_loc,
                        ast::Exp_::Name(sp(f_loc, ast::NameAccessChain_::One(field_res.0)), None),
                    )
                }
            };
            Ok((field_res, exp_res))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ast::Exp_::Pack(leading_name, type_res, fields_res))
}

fn translate_exp_call(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    leading: &cst::NameAccessChain,
    type_: &Option<Vec<cst::Type>>,
    args: &[cst::Exp],
) -> Result<(ast::NameAccessChain, Option<Vec<ast::Type>>, Vec<ast::Exp>), Diagnostic> {
    let leading_name = translate_name_access_chain(context, ast, leading)?;
    let type_res = translate_optional_types(context, ast, type_)?;

    let exp_res = args
        .iter()
        .map(|exp_| translate_exp(context, ast, exp_).map(|r| *r))
        .collect::<Result<Vec<_>, _>>()?;
    Ok((leading_name, type_res, exp_res))
}

fn translate_exp_name(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    leading: &cst::NameAccessChain,
    type_: &Option<Vec<cst::Type>>,
) -> Result<(ast::NameAccessChain, Option<Vec<ast::Type>>), Diagnostic> {
    let leading_name = translate_name_access_chain(context, ast, leading)?;
    let type_res = translate_optional_types(context, ast, type_)?;
    Ok((leading_name, type_res))
}

pub fn translate_friend(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    friend: &cst::FriendDecl,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::FriendDecl, Diagnostic> {
    let loc = friend.loc(&ast.source_tokens);
    let friend = translate_name_access_chain(context, ast, friend.value())?;
    Ok(ast::FriendDecl {
        attributes,
        loc,
        friend,
    })
}

pub fn translate_function(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    function: &cst::Function,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::Function, Diagnostic> {
    let loc = function.loc(&ast.source_tokens);
    let function_content = function.value();
    let mods = translate_modifiers(context, ast, &function_content.modifiers[..])?;
    let Modifiers {
        visibility: visibility_,
        mut entry,
        native,
    } = mods;
    if let Some(ast::Visibility::Script(vloc)) = visibility_ {
        let msg = format!(
            "'{script}' is deprecated in favor of the '{entry}' modifier. \
            Replace with '{public} {entry}'",
            script = ast::Visibility::SCRIPT,
            public = ast::Visibility::PUBLIC,
            entry = ast::ENTRY_MODIFIER,
        );
        context
            .env
            .add_diag(diag!(Uncategorized::DeprecatedWillBeRemoved, (vloc, msg,)));
        if entry.is_none() {
            entry = Some(vloc)
        }
    }
    let visibility = visibility_.unwrap_or(ast::Visibility::Internal);
    let name = translate_function_name(ast, &function_content.name)?;
    let signature =
        translate_function_signature(context, ast, &function_content.signatures, name.loc())?;
    let acquires = function_content
        .acquires
        .iter()
        .map(|a| translate_name_access_chain(context, ast, a))
        .collect::<Result<Vec<_>, _>>()?;
    let body = translate_function_body(context, ast, loc, &function_content.body, native)?;
    Ok(ast::Function {
        attributes,
        loc,
        visibility,
        entry,
        signature,
        acquires,
        name,
        body,
    })
}

pub fn translate_function_body(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    function_loc: Loc,
    body: &Option<cst::FunctionBody>,
    native: Option<Loc>,
) -> Result<ast::FunctionBody, Diagnostic> {
    match (native, &body) {
        (None, None) => Err(diag!(
            Declarations::InvalidFunction,
            (function_loc, "Expected body '{..}' for function.")
        )),
        (None, Some(b)) => {
            let loc = b.loc(&ast.source_tokens);

            let body = translate_exp_sequence(context, ast, b)?;
            Ok(sp(loc, ast::FunctionBody_::Defined(body)))
        }
        (Some(loc), None) => Ok(sp(loc, ast::FunctionBody_::Native)),
        (Some(_), Some(b)) => {
            let loc2 = b.loc(&ast.source_tokens);
            Err(diag!(
                Declarations::InvalidFunction,
                (
                    function_loc,
                    "Function body '{..}' is not expected in  native function"
                ),
                (loc2, "Remove '{..}' here.")
            ))
        }
    }
}

pub fn translate_function_name(
    ast: &cst::PackageDefinition,
    token: &cst::Name,
) -> Result<ast::FunctionName, Diagnostic> {
    let name = translate_token_to_name(ast, token)?;
    Ok(ast::FunctionName(name))
}

pub fn translate_function_signature(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    signature: &cst::FunctionSignature,
    function_name_loc: Loc,
) -> Result<ast::FunctionSignature, Diagnostic> {
    let type_parameters = translate_type_abilities(ast, &signature.type_parameters)?;

    let parameters = signature
        .parameters
        .iter()
        .map(|(v, t)| {
            let var = translate_var(ast, v)?;
            let type_ = translate_type(context, ast, t)?;
            Ok((var, type_))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let return_type = match &signature.return_type {
        Some(t) => translate_type(context, ast, t),
        None => Ok(sp(function_name_loc, ast::Type_::Unit)),
    }?;
    Ok(ast::FunctionSignature {
        type_parameters,
        parameters,
        return_type,
    })
}

pub fn translate_let_declare(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    decl: &cst::LetDeclare,
) -> Result<ast::SequenceItem_, Diagnostic> {
    let decl_content = decl.value();
    let bind_res = translate_bind_list(context, ast, &decl_content.var)?;
    let type_res = translate_optional_type(context, ast, &decl_content.type_)?;
    Ok(ast::SequenceItem_::Declare(bind_res, type_res))
}

pub fn translate_let_assign(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    assign: &cst::LetAssign,
) -> Result<ast::SequenceItem_, Diagnostic> {
    let bind_res = translate_bind_list(context, ast, &assign.value().var)?;
    let type_res = translate_optional_type(context, ast, &assign.value().type_)?;
    let exp_res = translate_exp(context, ast, &assign.value().exp)?;
    Ok(ast::SequenceItem_::Bind(bind_res, type_res, exp_res))
}

pub fn translate_bind_list(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    bind: &cst::BindList,
) -> Result<ast::BindList, Diagnostic> {
    let loc = bind.loc(&ast.source_tokens);
    let binds = bind
        .value()
        .iter()
        .map(|b| translate_bind(context, ast, b))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(sp(loc, binds))
}

fn translate_bind(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    bind: &cst::Bind,
) -> Result<ast::Bind, Diagnostic> {
    let loc = bind.loc(&ast.source_tokens);
    let bind_res = match bind.value() {
        cst::Bind_::Var(v) => Ok(ast::Bind_::Var(translate_var(ast, v)?)),
        cst::Bind_::Unpack(name, type_opt, fields) => {
            let name_res = translate_name_access_chain(context, ast, name)?;
            let type_res = translate_optional_types(context, ast, type_opt)?;
            let fields_res = fields
                .iter()
                .map(|(f, bi)| translate_bind_field(context, ast, f, bi))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ast::Bind_::Unpack(Box::new(name_res), type_res, fields_res))
        }
    }?;
    Ok(sp(loc, bind_res))
}

fn translate_bind_field(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    field: &cst::Field,
    bind: &Option<cst::Bind>,
) -> Result<(ast::Field, ast::Bind), Diagnostic> {
    let field_res = translate_field(ast, field)?;
    let bind_res = match bind {
        Some(b) => translate_bind(context, ast, b),
        None => {
            let v = ast::Var(field_res.0);
            Ok(sp(v.loc(), ast::Bind_::Var(v)))
        }
    }?;
    Ok((field_res, bind_res))
}

pub fn translate_modifiers(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    modifiers: &[cst::Modifier],
) -> Result<Modifiers, Diagnostic> {
    let mut mods = Modifiers::empty();
    for m in modifiers.iter() {
        let loc = m.token_range.loc(&ast.source_tokens);
        match m.value() {
            cst::Modifier_::Native => {
                if let Some(prev_loc) = mods.native {
                    let msg = "Duplicate 'native' modifier".to_string();
                    let prev_msg = "'native' modifier previously given here".to_string();
                    context.env.add_diag(diag!(
                        Declarations::DuplicateItem,
                        (loc, msg),
                        (prev_loc, prev_msg)
                    ))
                }
                mods.native = Some(loc);
            }
            cst::Modifier_::Entry => {
                if let Some(prev_loc) = mods.entry {
                    let msg = format!("Duplicate '{}' modifier", ENTRY_MODIFIER);
                    let prev_msg = format!("'{}' modifier previously given here", ENTRY_MODIFIER);
                    context.env.add_diag(diag!(
                        Declarations::DuplicateItem,
                        (loc, msg),
                        (prev_loc, prev_msg)
                    ))
                }
                mods.entry = Some(loc);
            }
            cst::Modifier_::Visibility(v) => {
                if let Some(prev_vis) = &mods.visibility {
                    let msg = "Duplicate visibility modifier".to_string();
                    let prev_msg = "Visibility modifier previously given here".to_string();
                    context.env.add_diag(diag!(
                        Declarations::DuplicateItem,
                        (loc, msg),
                        (prev_vis.loc().unwrap(), prev_msg),
                    ));
                }
                mods.visibility = Some(translate_visibility(ast, v));
            }
        }
    }
    Ok(mods)
}

pub fn translate_visibility(
    ast: &cst::PackageDefinition,
    visibility: &cst::Visibility,
) -> ast::Visibility {
    let loc = visibility.token_range.loc(&ast.source_tokens);
    match visibility.value() {
        cst::Visibility_::Friend => ast::Visibility::Friend(loc),
        cst::Visibility_::Public => ast::Visibility::Public(loc),
        cst::Visibility_::Script => ast::Visibility::Script(loc),
        cst::Visibility_::Internal => ast::Visibility::Internal,
    }
}

pub fn translate_module(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    module: &cst::Module,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::ModuleDefinition, Diagnostic> {
    let loc = module.loc(&ast.source_tokens);
    let address = match &module.value().address {
        Some(addr) => Ok(Some(translate_token_to_leading_name_access(
            context, ast, addr,
        )?)),
        None => Ok(None),
    }?;
    let name = translate_module_name(ast, &module.value().name)?;
    let members = translate_module_members(context, ast, &module.value().body)?;
    Ok(ast::ModuleDefinition {
        attributes,
        loc,
        address,
        name,
        is_spec_module: false,
        members,
    })
}

fn translate_module_members(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    trees: &cst::BlockSequence,
) -> Result<Vec<ast::ModuleMember>, Diagnostic> {
    let mut members = vec![];
    let mut attributes = vec![];
    for tree in trees.value() {
        match tree {
            cst::ParseTree::Function(func) => {
                let attrs = mem::take(&mut attributes);
                members.push(ast::ModuleMember::Function(translate_function(
                    context, ast, func, attrs,
                )?))
            }
            cst::ParseTree::Struct(struct_) => {
                let attrs = mem::take(&mut attributes);
                members.push(ast::ModuleMember::Struct(translate_struct(
                    context, ast, struct_, attrs,
                )?))
            }
            cst::ParseTree::Attribute(attr) => {
                attributes.push(translate_attributes(context, ast, attr)?)
            }
            cst::ParseTree::UseDecl(use_) => {
                let attrs = mem::take(&mut attributes);
                members.push(ast::ModuleMember::Use(translate_use_decl(
                    context, ast, use_, attrs,
                )?))
            }
            cst::ParseTree::FriendDecl(friend) => {
                let attrs = mem::take(&mut attributes);
                members.push(ast::ModuleMember::Friend(translate_friend(
                    context, ast, friend, attrs,
                )?))
            }
            cst::ParseTree::Constant(constant) => {
                let attrs = mem::take(&mut attributes);
                members.push(ast::ModuleMember::Constant(translate_constant(
                    context, ast, constant, attrs,
                )?))
            }
            cst::ParseTree::Spec(spec, _) => {
                let attrs = mem::take(&mut attributes);
                members.push(ast::ModuleMember::Spec(translate_spec_block(
                    context, ast, spec, attrs,
                )?))
            }

            cst::ParseTree::Module(module) => {
                return invalid_declaration(
                    ast,
                    module,
                    Declarations::InvalidModule,
                    "'module' is not allowed in module",
                )
            }
            cst::ParseTree::Script(script) => {
                return invalid_declaration(
                    ast,
                    script,
                    Declarations::InvalidScript,
                    "'script' is not allowed in module",
                )
            }
            cst::ParseTree::Address(address) => {
                return invalid_declaration(
                    ast,
                    address,
                    Declarations::InvalidAddress,
                    "'address' is not allowed in module",
                )
            }
            cst::ParseTree::Declare(decl) => {
                return invalid_declaration(
                    ast,
                    decl,
                    Declarations::InvalidLet,
                    "'let' is not allowed in module",
                )
            }
            cst::ParseTree::LetAssign(bind_) => {
                return invalid_declaration(
                    ast,
                    bind_,
                    Declarations::InvalidLet,
                    "'let' is not allowed in module",
                )
            }
            cst::ParseTree::Exp(exp, _) => {
                return invalid_declaration(
                    ast,
                    exp,
                    Declarations::InvalidExpression,
                    "expression is not allowed in module",
                )
            }
            cst::ParseTree::SpecMember(spec) => match spec.value() {
                cst::SpecMember_::Condition(kind) => match kind.value() {
                    cst::SpecConditionKind_::Invariant { .. } => {
                        let member = translate_spec_member(context, ast, spec)?;
                        let loc = spec.loc(&ast.source_tokens);
                        let invariant_loc = Loc::loc_start(&loc);
                        let spec_b = ast::ModuleMember::Spec(sp(
                            loc,
                            ast::SpecBlock_ {
                                attributes: vec![],
                                target: sp(invariant_loc, ast::SpecBlockTarget_::Module),
                                uses: vec![],
                                members: vec![member],
                            },
                        ));
                        members.push(spec_b)
                    }
                    _ => {
                        return invalid_declaration(
                            ast,
                            spec,
                            Declarations::InvalidSpecStatement,
                            "spec member is not allowed in module",
                        )
                    }
                },
                _ => {
                    return invalid_declaration(
                        ast,
                        spec,
                        Declarations::InvalidSpecStatement,
                        "spec member is not allowed in module",
                    )
                }
            },
        }
    }
    Ok(members)
}

fn translate_module_name(
    ast: &cst::PackageDefinition,
    name: &cst::ParsedToken,
) -> Result<ast::ModuleName, Diagnostic> {
    Ok(ast::ModuleName(translate_token_to_name(ast, name)?))
}

pub fn translate_script(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    script: &cst::Script,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::Script, Diagnostic> {
    let loc = script.loc(&ast.source_tokens);
    let mut uses = vec![];
    let mut constants = vec![];
    let mut function = None;
    let mut specs = vec![];
    let mut internal_attrs = vec![];
    for member in script.value().members.value() {
        match member {
            cst::ParseTree::UseDecl(use_) => {
                let attrs = mem::take(&mut internal_attrs);
                uses.push(translate_use_decl(context, ast, use_, attrs)?)
            }
            cst::ParseTree::Constant(constant) => {
                let attrs = mem::take(&mut internal_attrs);
                constants.push(translate_constant(context, ast, constant, attrs)?)
            }
            cst::ParseTree::Function(func) => match function {
                Some(_) => {
                    let sec_loc = func.loc(&ast.source_tokens);
                    return Err(diag!(
                        Declarations::InvalidScript,
                        (loc, "Expected only one function in 'Script'"),
                        (sec_loc, "Found the second function in 'Script'")
                    ));
                }
                None => {
                    let attrs = mem::take(&mut internal_attrs);
                    function = Some(translate_function(context, ast, func, attrs)?);
                }
            },
            cst::ParseTree::Spec(spec_block, _) => {
                let attrs = mem::take(&mut internal_attrs);
                specs.push(translate_spec_block(context, ast, spec_block, attrs)?)
            }

            cst::ParseTree::Module(module) => {
                return invalid_declaration(
                    ast,
                    module,
                    Declarations::InvalidModule,
                    "'module' is not allowed in script",
                )
            }
            cst::ParseTree::Script(script) => {
                return invalid_declaration(
                    ast,
                    script,
                    Declarations::InvalidScript,
                    "'script' is not allowed in script",
                )
            }
            cst::ParseTree::Address(address) => {
                return invalid_declaration(
                    ast,
                    address,
                    Declarations::InvalidAddress,
                    "'address' is not allowed in script",
                )
            }
            cst::ParseTree::Struct(struct_) => {
                return invalid_declaration(
                    ast,
                    struct_,
                    Declarations::InvalidStruct,
                    "'struct' is not allowed in script",
                )
            }
            cst::ParseTree::Attribute(attrs) => {
                internal_attrs.push(translate_attributes(context, ast, attrs)?)
            }
            cst::ParseTree::FriendDecl(friend) => {
                return invalid_declaration(
                    ast,
                    friend,
                    Declarations::InvalidFriendDeclaration,
                    "'friend' is not allowed in script",
                )
            }
            cst::ParseTree::Declare(decl) => {
                return invalid_declaration(
                    ast,
                    decl,
                    Declarations::InvalidLet,
                    "'let' is not allowed in script",
                )
            }
            cst::ParseTree::LetAssign(bind) => {
                return invalid_declaration(
                    ast,
                    bind,
                    Declarations::InvalidLet,
                    "'let' is not allowed in script",
                )
            }
            cst::ParseTree::Exp(exp, _) => {
                return invalid_declaration(
                    ast,
                    exp,
                    Declarations::InvalidExpression,
                    "plain expression is not allowed in script",
                )
            }
            cst::ParseTree::SpecMember(member) => {
                return invalid_declaration(
                    ast,
                    member,
                    Declarations::InvalidSpecStatement,
                    "plain spec is not allowed in script",
                )
            }
        }
    }
    match function {
        None => Err(diag!(
            Declarations::InvalidScript,
            (loc, "Expected a function in 'Script'")
        )),
        Some(func) => Ok(ast::Script {
            attributes,
            loc,
            uses,
            constants,
            function: func,
            specs,
        }),
    }
}

pub fn translate_spec_block(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    spec_block: &cst::SpecBlock,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::SpecBlock, Diagnostic> {
    let loc = spec_block.loc(&ast.source_tokens);
    let target = translate_spec_target(context, ast, &spec_block.value().target)?;

    let mut uses = vec![];
    let mut internal_attrs = vec![];
    let mut members = vec![];
    for member in spec_block.value().members.value() {
        match member {
            cst::ParseTree::SpecMember(spec_member) => {
                check_invalid_attributes(
                    &internal_attrs,
                    "attribute before spec member is not valid",
                )?;
                members.push(translate_spec_member(context, ast, spec_member)?)
            }
            cst::ParseTree::UseDecl(use_) => {
                let attrs = mem::take(&mut internal_attrs);
                uses.push(translate_use_decl(context, ast, use_, attrs)?)
            }
            cst::ParseTree::Attribute(attribute) => {
                internal_attrs.push(translate_attributes(context, ast, attribute)?)
            }
            cst::ParseTree::Function(function) => {
                check_invalid_attributes(
                    &internal_attrs,
                    "attribute before spec function is not valid",
                )?;
                members.push(translate_function_to_spec_memeber(context, ast, function)?)
            }

            cst::ParseTree::LetAssign(let_assign) => {
                check_invalid_attributes(
                    &internal_attrs,
                    "attribute before spec function is not valid",
                )?;
                members.push(translate_let_assign_to_spec_member(
                    context, ast, let_assign,
                )?)
            }
            cst::ParseTree::Exp(e, _) => {
                return invalid_declaration(
                    ast,
                    e,
                    Declarations::InvalidExpression,
                    "expression is not allowed in spec block",
                )
            }
            cst::ParseTree::Declare(declare) => {
                return invalid_declaration(
                    ast,
                    declare,
                    Declarations::InvalidLet,
                    "'let <Var>(:<Type>);' is not allowed in spec members",
                )
            }
            cst::ParseTree::Module(module) => {
                return invalid_declaration(
                    ast,
                    module,
                    Declarations::InvalidModule,
                    "'module' is not allowed in spec members",
                )
            }
            cst::ParseTree::Script(script) => {
                return invalid_declaration(
                    ast,
                    script,
                    Declarations::InvalidScript,
                    "'script' is not allowed in spec members",
                )
            }
            cst::ParseTree::Address(address) => {
                return invalid_declaration(
                    ast,
                    address,
                    Declarations::InvalidAddress,
                    "'address' is not allowed in spec members",
                )
            }
            cst::ParseTree::Struct(struct_) => {
                return invalid_declaration(
                    ast,
                    struct_,
                    Declarations::InvalidStruct,
                    "'struct' is not allowed in spec members",
                )
            }
            cst::ParseTree::FriendDecl(friend_) => {
                return invalid_declaration(
                    ast,
                    friend_,
                    Declarations::InvalidFriendDeclaration,
                    "'friend' is not allowed in spec members",
                )
            }
            cst::ParseTree::Constant(constant) => {
                return invalid_declaration(
                    ast,
                    constant,
                    Declarations::InvalidConstant,
                    "'const' is not allowed in spec members",
                )
            }
            cst::ParseTree::Spec(s, _) => {
                return invalid_declaration(
                    ast,
                    s,
                    Declarations::InvalidSpec,
                    "'spec' is not allowed in spec members",
                )
            }
        }
    }

    Ok(sp(
        loc,
        ast::SpecBlock_ {
            attributes,
            target,
            uses,
            members,
        },
    ))
}

fn translate_one_bind(
    ast: &cst::PackageDefinition,
    bind: &cst::BindList,
) -> Result<Name, Diagnostic> {
    match &bind.value()[..] {
        [one] => match one.value() {
            cst::Bind_::Var(var) => translate_token_to_name(ast, var),
            _ => invalid_syntax(
                ast,
                bind,
                Syntax::InvalidNameAccessChain,
                "only one name is allowed",
            ),
        },
        _ => invalid_syntax(
            ast,
            bind,
            Syntax::InvalidNameAccessChain,
            "only one name is allowed",
        ),
    }
}

fn translate_let_assign_to_spec_member(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    let_assign: &cst::LetAssign,
) -> Result<ast::SpecBlockMember, Diagnostic> {
    let loc = let_assign.loc(&ast.source_tokens);

    let name_res = translate_one_bind(ast, &let_assign.value.var)?;
    let def_res = translate_exp(context, ast, &let_assign.value().exp)?;
    Ok(sp(
        loc,
        ast::SpecBlockMember_::Let {
            name: name_res,
            post_state: let_assign.value.is_post,
            def: *def_res,
        },
    ))
}

fn translate_function_to_spec_memeber(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    function: &cst::Function,
) -> Result<ast::SpecBlockMember, Diagnostic> {
    let loc = function.loc(&ast.source_tokens);
    let cst::Function_ {
        modifiers,
        signatures,
        acquires,
        name,
        body,
    } = function.value();
    let mods = translate_modifiers(context, ast, modifiers)?;
    let Modifiers {
        native: native_opt, ..
    } = mods;
    if !acquires.is_empty() {
        return Err(diag!(
            Declarations::InvalidFunction,
            (loc, "acquire is not allowed  in spec function")
        ));
    }
    let func_name = translate_function_name(ast, name)?;
    let signature_res = translate_function_signature(context, ast, signatures, func_name.loc())?;
    let (uninterpreted, body_) = match (native_opt, body) {
        (None, None) => (
            native_opt.is_none(),
            sp(Loc::loc_end(&loc), ast::FunctionBody_::Native),
        ),
        (None, Some(b)) => {
            let loc = b.loc(&ast.source_tokens);

            let body = translate_exp_sequence(context, ast, b)?;

            (false, sp(loc, ast::FunctionBody_::Defined(body)))
        }
        (Some(_), None) => (false, sp(Loc::loc_end(&loc), ast::FunctionBody_::Native)),
        (Some(_), Some(b)) => {
            let loc2 = b.loc(&ast.source_tokens);
            return Err(diag!(
                Declarations::InvalidFunction,
                (
                    loc,
                    "Function body '{..}' is not expected in  native function"
                ),
                (loc2, "Remove '{..}' here.")
            ));
        }
    };

    Ok(sp(
        loc,
        ast::SpecBlockMember_::Function {
            uninterpreted,
            name: func_name,
            signature: signature_res,
            body: body_,
        },
    ))
}

fn translate_spec_condition_kind(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    cond: &cst::SpecConditionKind,
) -> Result<ast::SpecBlockMember_, Diagnostic> {
    match cond.value() {
        cst::SpecConditionKind_::SingleExpCondition {
            kind,
            properties,
            exp,
        } => {
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            let kind_loc = kind.loc(&ast.source_tokens);
            let res_func = |k: ast::SpecConditionKind_| {
                Ok(ast::SpecBlockMember_::Condition {
                    kind: sp(kind_loc, k),
                    properties: properties_res,
                    exp: *exp_res,
                    additional_exps: vec![],
                })
            };
            match kind.value() {
                cst::SingleSpecCondition::Assert => res_func(ast::SpecConditionKind_::Assert),
                cst::SingleSpecCondition::Assume => res_func(ast::SpecConditionKind_::Assume),
                cst::SingleSpecCondition::Ensures => res_func(ast::SpecConditionKind_::Ensures),
                cst::SingleSpecCondition::Requires => res_func(ast::SpecConditionKind_::Requires),
                cst::SingleSpecCondition::Decreases => res_func(ast::SpecConditionKind_::Decreases),
                cst::SingleSpecCondition::SucceedsIf => {
                    res_func(ast::SpecConditionKind_::SucceedsIf)
                }
            }
        }
        cst::SpecConditionKind_::AbortsIf {
            loc,
            properties,
            exp,
            with_exp,
        } => {
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            let with_exp_res = match with_exp {
                Some(e) => Ok(vec![*translate_exp(context, ast, e)?]),
                None => Ok(vec![]),
            }?;
            let keyword_loc = loc.loc(&ast.source_tokens);
            Ok(ast::SpecBlockMember_::Condition {
                kind: sp(keyword_loc, ast::SpecConditionKind_::AbortsIf),
                properties: properties_res,
                exp: *exp_res,
                additional_exps: with_exp_res,
            })
        }
        cst::SpecConditionKind_::CommaExpCondition {
            kind,
            properties,
            exps,
        } => {
            let kind_loc = kind.loc(&ast.source_tokens);
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exps_res = exps
                .iter()
                .map(|e| translate_exp(context, ast, e).map(|r| *r))
                .collect::<Result<Vec<_>, _>>()?;
            let dummy_exp = sp(
                kind_loc,
                ast::Exp_::Value(sp(kind_loc, ast::Value_::Bool(false))),
            );
            let res = |k: ast::SpecConditionKind_| {
                Ok(ast::SpecBlockMember_::Condition {
                    kind: sp(kind_loc, k),
                    properties: properties_res,
                    exp: dummy_exp,
                    additional_exps: exps_res,
                })
            };

            match kind.value() {
                cst::CommaSpecCondition::AbortsWith => res(ast::SpecConditionKind_::AbortsWith),
                cst::CommaSpecCondition::Modifies => res(ast::SpecConditionKind_::Modifies),
            }
        }
        cst::SpecConditionKind_::Emits {
            loc,
            properties,
            exp,
            to_exp,
            if_exp,
        } => {
            let name_loc = loc.loc(&ast.source_tokens);
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            let mut additional_exp = vec![];
            let to_exp_res = *translate_exp(context, ast, to_exp)?;
            additional_exp.push(to_exp_res);
            match if_exp {
                Some(e) => {
                    let e_res = translate_exp(context, ast, e)?;
                    additional_exp.push(*e_res);
                }
                None => (),
            };
            Ok(ast::SpecBlockMember_::Condition {
                kind: sp(name_loc, ast::SpecConditionKind_::Emits),
                properties: properties_res,
                exp: *exp_res,
                additional_exps: additional_exp,
            })
        }
        cst::SpecConditionKind_::Invariant {
            types,
            properties,
            exp,
        } => {
            let loc = types.loc(&ast.source_tokens);
            let types = translate_type_abilities(ast, types.value())?;
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            Ok(ast::SpecBlockMember_::Condition {
                kind: sp(loc, ast::SpecConditionKind_::Invariant(types)),
                properties: properties_res,
                exp: *exp_res,
                additional_exps: vec![],
            })
        }
        cst::SpecConditionKind_::InvariantUpdate {
            types,
            properties,
            exp,
        } => {
            let loc = types.loc(&ast.source_tokens);
            let types = translate_type_abilities(ast, types.value())?;
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            Ok(ast::SpecBlockMember_::Condition {
                kind: sp(loc, ast::SpecConditionKind_::InvariantUpdate(types)),
                properties: properties_res,
                exp: *exp_res,
                additional_exps: vec![],
            })
        }
        cst::SpecConditionKind_::Axiom {
            types,
            properties,
            exp,
        } => {
            let loc = types.loc(&ast.source_tokens);
            let types = translate_type_abilities(ast, types.value())?;
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            Ok(ast::SpecBlockMember_::Condition {
                kind: sp(loc, ast::SpecConditionKind_::Axiom(types)),
                properties: properties_res,
                exp: *exp_res,
                additional_exps: vec![],
            })
        }
    }
}

fn translate_spec_member(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    members: &cst::SpecMember,
) -> Result<ast::SpecBlockMember, Diagnostic> {
    let loc = members.loc(&ast.source_tokens);

    let member_res = match members.value() {
        cst::SpecMember_::Condition(kind) => translate_spec_condition_kind(context, ast, kind)?,
        cst::SpecMember_::Variable {
            is_global,
            name,
            type_parameters,
            type_,
            init,
        } => {
            let name_res = translate_token_to_name(ast, name)?;
            let type_parameter_res = translate_type_abilities(ast, type_parameters)?;
            let type_res = translate_type(context, ast, type_)?;
            let init_res = match init {
                None => Ok(None),
                Some(i) => Ok(Some(*translate_exp(context, ast, i)?)),
            }?;
            ast::SpecBlockMember_::Variable {
                is_global: *is_global,
                name: name_res,
                type_parameters: type_parameter_res,
                type_: type_res,
                init: init_res,
            }
        }
        cst::SpecMember_::Update { lhs, rhs } => {
            let lhs_res = translate_exp(context, ast, lhs)?;
            let rhs_res = translate_exp(context, ast, rhs)?;
            ast::SpecBlockMember_::Update {
                lhs: *lhs_res,
                rhs: *rhs_res,
            }
        }
        cst::SpecMember_::Include { properties, exp } => {
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exp_res = translate_exp(context, ast, exp)?;
            ast::SpecBlockMember_::Include {
                properties: properties_res,
                exp: *exp_res,
            }
        }
        cst::SpecMember_::Apply {
            exp,
            patterns,
            exclusion_patterns,
        } => {
            let exp_res = translate_exp(context, ast, exp)?;
            let patters_res = patterns
                .iter()
                .map(|p| translate_spec_pattern(ast, p))
                .collect::<Result<Vec<_>, _>>()?;
            let exclusion_patterns_res = exclusion_patterns
                .iter()
                .map(|p| translate_spec_pattern(ast, p))
                .collect::<Result<Vec<_>, _>>()?;

            ast::SpecBlockMember_::Apply {
                exp: *exp_res,
                patterns: patters_res,
                exclusion_patterns: exclusion_patterns_res,
            }
        }
        cst::SpecMember_::Pragma { properties } => {
            let properties_res = properties
                .iter()
                .map(|p| translate_progma_property(context, ast, p))
                .collect::<Result<Vec<_>, _>>()?;

            ast::SpecBlockMember_::Pragma {
                properties: properties_res,
            }
        }
    };
    Ok(sp(loc, member_res))
}

fn translate_spec_pattern(
    ast: &cst::PackageDefinition,
    pattern: &cst::SpecApplyPattern,
) -> Result<ast::SpecApplyPattern, Diagnostic> {
    let loc = pattern.loc(&ast.source_tokens);
    let visibility = match &pattern.value().visibility {
        Some(v) => {
            let vis = translate_visibility(ast, v);
            Ok(Some(vis))
        }
        None => Ok(None),
    }?;
    let name_pattern = pattern
        .value()
        .name_pattern
        .iter()
        .map(|np| translate_spec_apply(ast, np))
        .collect::<Result<Vec<_>, _>>()?;
    let type_parameters = translate_type_abilities(ast, &pattern.value().type_parameters)?;
    Ok(sp(
        loc,
        ast::SpecApplyPattern_ {
            visibility,
            name_pattern,
            type_parameters,
        },
    ))
}

fn translate_spec_apply(
    ast: &cst::PackageDefinition,
    apply: &cst::SpecApplyFragment,
) -> Result<ast::SpecApplyFragment, Diagnostic> {
    let loc = apply.loc(&ast.source_tokens);
    let fragment = match apply.value().kind {
        Tok::Identifier => {
            let name = translate_token_to_name(ast, apply)?;
            Ok(ast::SpecApplyFragment_::NamePart(name))
        }
        Tok::Star => Ok(ast::SpecApplyFragment_::Wildcard),
        _ => {
            return Err(unexpected_token_error(
                apply.loc(&ast.source_tokens),
                "",
                "a name fragment or `*`",
            ))
        }
    }?;
    Ok(sp(loc, fragment))
}
fn translate_progma_property(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    property: &cst::PragmaProperty,
) -> Result<ast::PragmaProperty, Diagnostic> {
    let loc = property.loc(&ast.source_tokens);
    let property_name = if property.value().name.value.content.as_str() == "friend" {
        let name_loc = property.value().name.loc(&ast.source_tokens);
        Name::new(name_loc, Symbol::from("friend"))
    } else {
        translate_token_to_name(ast, &property.value().name)?
    };

    let property = match &property.value().value {
        Some(p) => match p {
            cst::PragmaValue::Literal(val) => {
                let val_res = translate_value(context, ast, val)?;
                Ok(Some(ast::PragmaValue::Literal(val_res)))
            }
            cst::PragmaValue::Ident(names) => {
                let names_res = translate_name_access_chain(context, ast, names)?;
                Ok(Some(ast::PragmaValue::Ident(names_res)))
            }
        },
        None => Ok(None),
    }?;
    Ok(sp(
        loc,
        ast::PragmaProperty_ {
            name: property_name,
            value: property,
        },
    ))
}

fn translate_spec_target(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    spec_target: &cst::SpecBlockTarget,
) -> Result<ast::SpecBlockTarget, Diagnostic> {
    let loc = spec_target.loc(&ast.source_tokens);
    let target_res = match spec_target.value() {
        cst::SpecBlockTarget_::Code => Ok(ast::SpecBlockTarget_::Code),
        cst::SpecBlockTarget_::Module => Ok(ast::SpecBlockTarget_::Module),
        cst::SpecBlockTarget_::Member(name, signature_opt) => {
            let name_res = translate_token_to_name(ast, name)?;
            let signature = match signature_opt {
                None => Ok(None),
                Some(sig) => {
                    let sigs = translate_function_signature(context, ast, sig, name_res.loc)?;
                    Ok(Some(Box::new(sigs)))
                }
            }?;
            Ok(ast::SpecBlockTarget_::Member(name_res, signature))
        }
        cst::SpecBlockTarget_::Schema(name, type_parameters) => {
            let name_res = translate_token_to_name(ast, name)?;
            let type_parameters_res = translate_type_abilities(ast, type_parameters)?;
            Ok(ast::SpecBlockTarget_::Schema(name_res, type_parameters_res))
        }
        cst::SpecBlockTarget_::IdentModule(_, _) => {
            return invalid_declaration(
                ast,
                spec_target,
                Declarations::InvalidSpec,
                "'spec address:name {..}' is only allowed in the top level",
            )
        }
        cst::SpecBlockTarget_::Func => Ok(ast::SpecBlockTarget_::Module),
    }?;
    Ok(sp(loc, target_res))
}

pub fn translate_struct(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    struct_: &cst::Struct,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::StructDefinition, Diagnostic> {
    let loc = struct_.loc(&ast.source_tokens);
    let struct_content = struct_.value();
    let mods = translate_modifiers(context, ast, &struct_content.modifiers[..])?;
    let Modifiers {
        visibility,
        entry,
        native,
    } = mods;
    if let Some(vis) = visibility {
        let msg = format!(
            "Invalid struct declaration. Structs cannot have visibility modifiers as they are \
             always '{}'",
            Visibility::PUBLIC
        );
        context
            .env
            .add_diag(diag!(Syntax::InvalidModifier, (vis.loc().unwrap(), msg)));
    }
    if let Some(entry_loc) = entry {
        let msg = format!(
            "Invalid constant declaration. '{}' is used only on functions",
            ENTRY_MODIFIER
        );
        context
            .env
            .add_diag(diag!(Syntax::InvalidModifier, (entry_loc, msg)));
    }

    let struct_name = translate_struct_name(ast, &struct_content.name)?;

    let abilities = struct_content
        .abilities
        .iter()
        .map(|a| translate_ability(ast, a))
        .collect::<Result<Vec<_>, _>>()?;

    let type_parameters = translate_struct_type_parameter(ast, &struct_content.type_parameters)?;

    let fields = match (native, &struct_content.fields) {
        (None, None) => {
            return Err(diag!(
                Declarations::InvalidStruct,
                (Loc::loc_end(&loc), "Expected struct body '{..}'")
            ))
        }
        (None, Some(fields)) => {
            let fields_target = fields
                .members
                .iter()
                .map(|(field, type_)| {
                    let f = translate_field(ast, field)?;
                    let ty = translate_type(context, ast, type_)?;
                    Ok((f, ty))
                })
                .collect::<Result<Vec<_>, _>>()?;
            ast::StructFields::Defined(fields_target)
        }
        (Some(l), None) => ast::StructFields::Native(l),
        (Some(_), Some(_)) => {
            return Err(diag!(
                Declarations::InvalidStruct,
                (loc, "Struct body '{..}' is not allowed for native struct."),
            ))
        }
    };

    Ok(ast::StructDefinition {
        attributes,
        loc,
        abilities,
        name: struct_name,
        type_parameters,
        fields,
    })
}

fn translate_struct_name(
    ast: &cst::PackageDefinition,
    token: &cst::Name,
) -> Result<ast::StructName, Diagnostic> {
    let name = translate_token_to_name(ast, token)?;
    Ok(ast::StructName(name))
}

fn translate_struct_type_parameter(
    ast: &cst::PackageDefinition,
    types: &[cst::StructTypeParameter],
) -> Result<Vec<ast::StructTypeParameter>, Diagnostic> {
    types
        .iter()
        .map(|type_| {
            let name = translate_token_to_name(ast, &type_.name)?;
            let constraints = type_
                .constraints
                .iter()
                .map(|c| translate_ability(ast, c))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ast::StructTypeParameter {
                is_phantom: type_.is_phantom,
                name,
                constraints,
            })
        })
        .collect()
}

pub fn translate_type(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    type_: &cst::Type,
) -> Result<ast::Type, Diagnostic> {
    let loc = type_.loc(&ast.source_tokens);
    match type_.value() {
        cst::Type_::Apply(names, types) => {
            let name_chains = translate_name_access_chain(context, ast, names)?;
            let typ = types
                .iter()
                .map(|t| translate_type(context, ast, t))
                .collect::<Result<Vec<ast::Type>, _>>()?;
            Ok(sp(loc, ast::Type_::Apply(Box::new(name_chains), typ)))
        }
        cst::Type_::Ref(is_mute, typ) => {
            let ty = translate_type(context, ast, typ)?;
            Ok(sp(loc, ast::Type_::Ref(*is_mute, Box::new(ty))))
        }
        cst::Type_::Fun(typ1, typ2) => {
            let para_type = typ1
                .iter()
                .map(|t| translate_type(context, ast, t))
                .collect::<Result<Vec<ast::Type>, _>>()?;
            let res_type = translate_type(context, ast, typ2)?;
            Ok(sp(loc, ast::Type_::Fun(para_type, Box::new(res_type))))
        }
        cst::Type_::Sequance(types) => {
            if types.is_empty() {
                Ok(sp(loc, ast::Type_::Unit))
            } else {
                let typ = types
                    .iter()
                    .map(|t| translate_type(context, ast, t))
                    .collect::<Result<Vec<ast::Type>, _>>()?;
                Ok(sp(loc, ast::Type_::Multiple(typ)))
            }
        }
    }
}

pub fn translate_type_abilities(
    ast: &cst::PackageDefinition,
    type_parameters: &[(cst::Name, Vec<cst::Ability>)],
) -> Result<Vec<(Name, Vec<ast::Ability>)>, Diagnostic> {
    type_parameters
        .iter()
        .map(|(name, abilities)| {
            let n = translate_token_to_name(ast, name)?;
            let abilities_res = abilities
                .iter()
                .map(|a| translate_ability(ast, a))
                .collect::<Result<Vec<_>, _>>()?;
            Ok((n, abilities_res))
        })
        .collect::<Result<Vec<_>, _>>()
}

pub fn translate_optional_types(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    types: &Option<Vec<cst::Type>>,
) -> Result<Option<Vec<ast::Type>>, Diagnostic> {
    match types {
        Some(tys) => {
            let res = tys
                .iter()
                .map(|t| translate_type(context, ast, t))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Some(res))
        }
        None => Ok(None),
    }
}

pub fn translate_optional_type(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    type_opt: &Option<cst::Type>,
) -> Result<Option<ast::Type>, Diagnostic> {
    match type_opt {
        None => Ok(None),
        Some(type_) => {
            let res = translate_type(context, ast, type_);
            match res {
                Ok(r) => Ok(Some(r)),
                Err(e) => Err(e),
            }
        }
    }
}

pub fn translate_use_decl(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    use_: &cst::UseDecl,
    attributes: Vec<ast::Attributes>,
) -> Result<ast::UseDecl, Diagnostic> {
    let uses_ = translate_use(context, ast, use_.value())?;
    Ok(ast::UseDecl {
        attributes,
        use_: uses_,
    })
}

pub fn translate_use(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    use_: &cst::Use,
) -> Result<ast::Use, Diagnostic> {
    match use_ {
        cst::Use::Module(ident, name) => {
            let ident_ = translate_module_ident(context, ast, ident)?;
            let name_ = match name {
                Some(n) => translate_token_to_module_name(ast, n).map(Some),
                None => Ok(None),
            }?;
            Ok(ast::Use::Module(ident_, name_))
        }
        cst::Use::Members(ident, names) => {
            let ident_ = translate_module_ident(context, ast, ident)?;
            let names_: Result<Vec<(Name, Option<Name>)>, Diagnostic> = names
                .iter()
                .map(|(name, name_opt)| match name_opt {
                    Some(n) => {
                        let lead_name = translate_token_to_name(ast, name)?;
                        translate_token_to_name(ast, n).map(|n| (lead_name, Some(n)))
                    }
                    None => {
                        let lead_name = translate_token_to_name(ast, name)?;
                        Ok((lead_name, None))
                    }
                })
                .collect();
            Ok(ast::Use::Members(ident_, names_?))
        }
    }
}

fn translate_token_to_module_name(
    ast: &cst::PackageDefinition,
    name: &cst::Name,
) -> Result<ast::ModuleName, Diagnostic> {
    let name = translate_token_to_name(ast, name)?;
    Ok(ast::ModuleName(name))
}

fn translate_module_ident(
    context: &mut Context,
    ast: &cst::PackageDefinition,
    module: &cst::ModuleIdent,
) -> Result<ast::ModuleIdent, Diagnostic> {
    let loc = sp_between_token(&module.address, &module.module);
    let address = translate_token_to_leading_name_access(context, ast, &module.address)?;
    let module = translate_token_to_module_name(ast, &module.module)?;
    Ok(sp(loc, ast::ModuleIdent_ { address, module }))
}

pub fn translate_program(
    compilation_env: &mut CompilationEnv,
    parsed_tree: &cst::Program,
) -> ast::Program {
    let mut context = Context::new(compilation_env);

    translate_program_(&mut context, parsed_tree)
}

fn translate_program_(context: &mut Context, ast: &cst::Program) -> ast::Program {
    let mut source_defs = vec![];
    for def in ast.source_definitions.iter() {
        match translate_parsetree_to_def(context, def) {
            Ok(res) => source_defs.extend(res),
            Err(diag) => context.env.add_diag(diag),
        };
    }

    let mut lib_def = vec![];
    for def in ast.lib_definitions.iter() {
        match translate_parsetree_to_def(context, def) {
            Ok(res) => lib_def.extend(res),
            Err(diag) => context.env.add_diag(diag),
        };
    }

    // Run attribute expansion on all source definitions, passing in the matching named address map.
    for ast::PackageDefinition {
        named_address_map: idx,
        def,
        ..
    } in source_defs.iter_mut()
    {
        attr_derivation::derive_from_attributes(context.env, ast.named_address_maps.get(*idx), def);
    }

    ast::Program {
        named_address_maps: ast.named_address_maps.clone(),
        source_definitions: source_defs,
        lib_definitions: lib_def,
    }
}
