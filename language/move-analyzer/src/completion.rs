// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::item::*;
use super::modules::*;
use super::scopes::*;
use super::types::ResolvedType;
use super::utils::*;
use crate::context::Context;
use crate::scope::Address;
use lsp_server::*;
use lsp_types::*;
use move_compiler::parser::ast::LeadingNameAccess_;
use move_compiler::parser::ast::ModuleName;
use move_compiler::parser::keywords::{BUILTINS, CONTEXTUAL_KEYWORDS, KEYWORDS, PRIMITIVE_TYPES};
use move_compiler::shared::Identifier;
use move_compiler::shared::Name;
use move_core_types::account_address::AccountAddress;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::path::*;
use std::vec;

/// Constructs an `lsp_types::CompletionItem` with the given `label` and `kind`.
fn completion_item(label: &str, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: label.to_owned(),
        kind: Some(kind),
        ..Default::default()
    }
}

/// Return a list of completion items corresponding to each one of Move's keywords.
///
/// Currently, this does not filter keywords out based on whether they are valid at the completion
/// request's cursor position, but in the future it ought to. For example, this function returns
/// all specification language keywords, but in the future it should be modified to only do so
/// within a spec block.
fn keywords() -> Vec<CompletionItem> {
    KEYWORDS
        .iter()
        .chain(CONTEXTUAL_KEYWORDS.iter())
        .chain(PRIMITIVE_TYPES.iter())
        .map(|label| {
            let kind = if label == &"copy" || label == &"move" {
                CompletionItemKind::Operator
            } else {
                CompletionItemKind::Keyword
            };
            completion_item(label, kind)
        })
        .collect()
}

/// Return a list of completion items of Move's primitive types
fn primitive_types() -> Vec<CompletionItem> {
    PRIMITIVE_TYPES
        .iter()
        .map(|label| completion_item(label, CompletionItemKind::Keyword))
        .collect()
}

/// Return a list of completion items corresponding to each one of Move's builtin functions.
fn builtins() -> Vec<CompletionItem> {
    BUILTINS
        .iter()
        .map(|label| completion_item(label, CompletionItemKind::Function))
        .collect()
}

/// Sends the given connection a response to a completion request.
///
/// The completions returned depend upon where the user's cursor is positioned.
pub fn on_completion_request(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<CompletionParams>(request.params.clone())
        .expect("could not deserialize references request");
    let fpath = parameters
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let loc = parameters.text_document_position.position;
    let line = loc.line;
    let col = loc.character;
    let fpath = path_concat(
        PathBuf::from(std::env::current_dir().unwrap()).as_path(),
        fpath.as_path(),
    );
    let mut visitor = Visitor::new(fpath, line, col);
    context.modules.run_visitor(&mut visitor);

    fn all_intrinsic() -> Vec<CompletionItem> {
        let mut x = builtins();
        x.extend(primitive_types().into_iter());
        x.extend(keywords().into_iter());
        x
    }

    let mut result = visitor.result.unwrap_or(all_intrinsic());
    if result.len() == 0 {
        result = all_intrinsic();
    }
    let ret = Some(CompletionResponse::Array(result));
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(ret).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

pub(crate) struct Visitor {
    /// The file we are looking for.
    pub(crate) filepath: PathBuf,
    pub(crate) line: u32,
    pub(crate) col: u32,
    pub(crate) result: Option<Vec<CompletionItem>>,
}

impl Visitor {
    pub(crate) fn new(filepath: impl Into<PathBuf>, line: u32, col: u32) -> Self {
        Self {
            filepath: filepath.into(),
            line,
            col,
            result: None,
        }
    }

    ///  match loc   
    fn match_loc(&self, loc: &Loc, services: &dyn HandleItemService) -> bool {
        let r = services.convert_loc_range(loc);
        match &r {
            Some(r) => r.in_range(self.filepath.clone(), self.line, self.col),
            None => false,
        }
    }
}

impl ScopeVisitor for Visitor {
    fn handle_item(
        &mut self,
        services: &dyn HandleItemService,
        scopes: &Scopes,
        item_or_access: &ItemOrAccess,
    ) {
        let push_items = |visitor: &mut Visitor, items: &Vec<Item>| {
            if visitor.result.is_none() {
                visitor.result = Some(vec![]);
            }
            let x: Vec<_> = items.iter().map(|x| item_to_completion_item(x)).collect();
            x.into_iter().for_each(|x| {
                if let Some(x) = x {
                    visitor.result.as_mut().unwrap().push(x);
                }
            });
        };
        let push_addr_spaces = |visitor: &mut Visitor, items: &Vec<AddressSpace>| {
            if visitor.result.is_none() {
                visitor.result = Some(vec![]);
            }
            let x: Vec<_> = name_spaces_to_completion_items(items);
            x.into_iter()
                .for_each(|x| visitor.result.as_mut().unwrap().push(x));
        };
        let push_completion_items = |visitor: &mut Visitor, items: Vec<CompletionItem>| {
            if visitor.result.is_none() {
                visitor.result = Some(vec![]);
            }
            items
                .into_iter()
                .for_each(|x| visitor.result.as_mut().unwrap().push(x));
        };
        let push_fields = |visitor: &mut Visitor, items: &HashMap<Symbol, (Name, ResolvedType)>| {
            if visitor.result.is_none() {
                visitor.result = Some(vec![]);
            }
            let x: Vec<_> = fields_2_completion_items(items);
            x.into_iter()
                .for_each(|x| visitor.result.as_mut().unwrap().push(x));
        };

        let push_name_module_names = |visitor: &mut Visitor, items: &Vec<ModuleName>| {
            if visitor.result.is_none() {
                visitor.result = Some(vec![]);
            }
            let x: Vec<_> = module_names_2_completion_items(items);
            x.into_iter()
                .for_each(|x| visitor.result.as_mut().unwrap().push(x));
        };
        log::trace!("completion access:{}", item_or_access);
        match item_or_access {
            ItemOrAccess::Item(item) => match item {
                Item::UseMember(module_ident, name, _alias, scope) => {
                    let addr = match &module_ident.value.address.value {
                        LeadingNameAccess_::AnonymousAddress(addr) => addr.bytes,
                        LeadingNameAccess_::Name(name) => services.name_2_addr(name.value),
                    };
                    if self.match_loc(&module_ident.value.address.loc, services) {
                        let items = services.get_all_addrs(scopes);
                        push_addr_spaces(self, &items);
                    } else if self.match_loc(&module_ident.value.module.loc(), services)
                        || self.match_loc(&module_ident.loc, services)
                    {
                        let items = scopes.collect_modules(&addr);
                        push_name_module_names(self, &items);
                    } else if self.match_loc(&name.loc, services) {
                        let mut items = Vec::new();
                        scope
                            .as_ref()
                            .borrow()
                            .items
                            .iter()
                            .for_each(|(_, x)| match x {
                                Item::Struct(_)
                                | Item::Fun(_)
                                | Item::Const(_, _)
                                | Item::SpecSchema(_, _) => {
                                    items.push(x.clone());
                                }
                                _ => {}
                            });

                        push_items(self, &items);
                    }
                }
                Item::UseModule(module_ident, _alias, _) => {
                    let addr = match &module_ident.value.address.value {
                        LeadingNameAccess_::AnonymousAddress(addr) => addr.bytes,
                        LeadingNameAccess_::Name(name) => services.name_2_addr(name.value),
                    };
                    if self.match_loc(&module_ident.value.address.loc, services) {
                        let items = services.get_all_addrs(scopes);
                        push_addr_spaces(self, &items);
                    } else if self.match_loc(&module_ident.value.module.loc(), services)
                        || self.match_loc(&module_ident.loc, services)
                    {
                        let items = scopes.collect_modules(&addr);
                        push_name_module_names(self, &items);
                    }
                }
                _ => {
                    // TODO.
                    // can item definition have auto completion items.
                    // may be type parameter can have completion items.
                    if self.match_loc(&item.def_loc(), services) {
                        self.result = Some(Vec::new());
                    }
                }
            },
            ItemOrAccess::Access(access) => {
                match access {
                    Access::ApplyType(x, _, _) => match &x.value {
                        move_compiler::parser::ast::NameAccessChain_::One(x) => {
                            if self.match_loc(&x.loc, services) {
                                push_items(self, &scopes.collect_all_type_items());
                            }
                        }
                        move_compiler::parser::ast::NameAccessChain_::Two(space, name) => {
                            if self.match_loc(&space.loc, services) {
                                let items = scopes.collect_imported_modules();
                                push_items(self, &items);
                            } else if self.match_loc(&name.loc, services)
                                || self.match_loc(&x.loc, services)
                            {
                                let items = scopes.collect_use_module_items(space);
                                push_items(self, &items);
                            }
                        }
                        move_compiler::parser::ast::NameAccessChain_::Three(x, z) => {
                            let (addr_, module) = x.value;
                            let addr = match &addr_.value {
                                LeadingNameAccess_::AnonymousAddress(addr) => addr.bytes,
                                LeadingNameAccess_::Name(name) => services.name_2_addr(name.value),
                            };
                            if self.match_loc(&addr_.loc, services) {
                                let items = services.get_all_addrs(scopes);
                                push_addr_spaces(self, &items);
                            } else if self.match_loc(&module.loc, services) {
                                let items = scopes.collect_modules(&addr);
                                push_name_module_names(self, &items);
                            } else if self.match_loc(&z.loc, services)
                                || self.match_loc(&x.loc, services)
                            {
                                let items = scopes.collect_modules_items(
                                    &addr,
                                    module.value,
                                    |x| match x {
                                        Item::Struct(_) | Item::StructNameRef(_, _, _, _) => true,
                                        _ => false,
                                    },
                                );
                                push_items(self, &items);
                            }
                        }
                    },
                    Access::ExprVar(_, _) => {
                        let items = scopes.collect_all_var_items();
                        push_items(self, &items);
                    }
                    Access::ExprAccessChain(name, _, _) => match &name.value {
                        move_compiler::parser::ast::NameAccessChain_::One(x) => {
                            if self.match_loc(&x.loc, services) {
                                push_items(self, &scopes.collect_all_var_items());
                            }
                        }
                        move_compiler::parser::ast::NameAccessChain_::Two(x, name) => {
                            if self.match_loc(&x.loc, services) {
                                let items = scopes.collect_imported_modules();
                                push_items(self, &items);
                            } else if self.match_loc(&name.loc, services) {
                                let items = scopes.collect_use_module_items(x);
                                push_items(self, &items);
                            }
                        }
                        move_compiler::parser::ast::NameAccessChain_::Three(x, z) => {
                            let (x, y) = x.value;
                            let addr = match &x.value {
                                LeadingNameAccess_::AnonymousAddress(addr) => addr.bytes,
                                LeadingNameAccess_::Name(name) => services.name_2_addr(name.value),
                            };
                            if self.match_loc(&x.loc, services) {
                                let items = services.get_all_addrs(scopes);
                                push_addr_spaces(self, &items);
                            } else if self.match_loc(&y.loc, services) {
                                let items = scopes.collect_modules(&addr);
                                push_name_module_names(self, &items);
                            } else if self.match_loc(&z.loc, services) {
                                let items =
                                    scopes.collect_modules_items(&addr, y.value, |x| match x {
                                        // top level can only have const as expr.
                                        Item::Const(_, _) | Item::Fun(_) => true,
                                        _ => false,
                                    });
                                push_items(self, &items);
                            }
                        }
                    },
                    Access::AccessFiled(from, _, _, all) => {
                        if self.match_loc(&from.loc(), services) {
                            push_fields(self, all);
                        }
                    }
                    Access::KeyWords(x) => {}
                    Access::MacroCall(_, x) => {}
                    Access::Friend(chain, _) => match &chain.value {
                        move_compiler::parser::ast::NameAccessChain_::One(name) => {
                            if self.match_loc(&name.loc, services) {
                                let items = services.get_all_addrs(scopes);
                                push_addr_spaces(self, &items);
                            }
                        }
                        move_compiler::parser::ast::NameAccessChain_::Two(addr, name) => {
                            if self.match_loc(&addr.loc, services) {
                                let items = services.get_all_addrs(scopes);
                                push_addr_spaces(self, &items);
                            } else if self.match_loc(&name.loc, services) {
                                let addr = match &addr.value {
                                    LeadingNameAccess_::AnonymousAddress(addr) => addr.bytes,
                                    LeadingNameAccess_::Name(name) => {
                                        services.name_2_addr(name.value)
                                    }
                                };

                                let items = scopes.collect_modules(&addr);
                                push_name_module_names(self, &items);
                            }
                        }
                        move_compiler::parser::ast::NameAccessChain_::Three(_, _) => {
                            // not a valid friend statement
                        }
                    },
                    Access::MoveBuildInFun(_, _) => {}
                    Access::SpecBuildInFun(_, _) => {}
                    Access::IncludeSchema(x, _) => {
                        if self.match_loc(&x.loc, services) {
                            let items = scopes.collect_all_spec_schema();
                            push_items(self, &items);
                        }
                    }
                    Access::PragmaProperty(x) => {
                        if self.match_loc(&x.loc, services) {
                            let items = pragma_property_completion_items();
                            push_completion_items(self, items);
                        }
                    }
                    Access::ExprAddressName(_) => {
                        // TODO. handle address name.
                    }
                };
            }
        }
    }

    fn file_should_visit(&self, p: &PathBuf) -> bool {
        let x = self.filepath == *p;
        x
    }
    fn finished(&self) -> bool {
        self.result.is_some()
    }
}

impl std::fmt::Display for Visitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "completion,file:{:?} line:{} col:{}",
            self.filepath, self.line, self.col
        )
    }
}

fn pragma_property_completion_items() -> Vec<CompletionItem> {
    let mut ret = Vec::new();
    ret.push(CompletionItem {
        label: String::from("verify = true;"),
        kind: Some(CompletionItemKind::Text),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    });
    ret.push(CompletionItem {
        label: String::from("intrinsic;"),
        kind: Some(CompletionItemKind::Text),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    });

    ret.push(CompletionItem {
        label: String::from("timeout=1000;"),
        kind: Some(CompletionItemKind::Text),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    });

    ret.push(CompletionItem {
        label: String::from("verify_duration_estimate=1000;"),
        kind: Some(CompletionItemKind::Text),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    });

    ret.push(CompletionItem {
        label: String::from("seed;"),
        kind: Some(CompletionItemKind::Text),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    });
    ret
}

fn fields_2_completion_items(x: &HashMap<Symbol, (Name, ResolvedType)>) -> Vec<CompletionItem> {
    let mut ret = Vec::new();
    x.values()
        .for_each(|(name, ty)| ret.push(field_2_completion_item(name, ty)));
    ret
}

fn field_2_completion_item(field: &Name, ty: &ResolvedType) -> CompletionItem {
    CompletionItem {
        label: String::from(field.value.as_str()),
        kind: Some(CompletionItemKind::Field),
        detail: Some(format!("field {}:{}", field.value.as_str(), ty)),
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: None,
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    }
}

fn module_names_2_completion_items(x: &Vec<ModuleName>) -> Vec<CompletionItem> {
    let mut ret = Vec::with_capacity(x.len());
    for xx in x.iter() {
        ret.push(CompletionItem {
            label: String::from(xx.0.value.as_str()),
            kind: Some(CompletionItemKind::Module),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        })
    }
    ret
}

fn name_spaces_to_completion_items(x: &Vec<AddressSpace>) -> Vec<CompletionItem> {
    let mut ret = Vec::with_capacity(x.len() * 2);
    for space in x.iter() {
        let (addr, name) = space.get_addr_and_name();

        if let Some(name) = name {
            ret.push(CompletionItem {
                label: String::from(name.as_str()),
                kind: Some(ADDR_COMPLETION_KIND), // TODO this should be a module,should be a namespace.
                detail: None,
                documentation: None,
                deprecated: None,
                preselect: None,
                sort_text: None,
                filter_text: None,
                insert_text: None,
                insert_text_format: None,
                insert_text_mode: None,
                text_edit: None,
                additional_text_edits: None,
                command: None,
                commit_characters: None,
                data: None,
                tags: None,
            });
        }
        ret.push(CompletionItem {
            label: addr.short_str_lossless(),
            kind: Some(ADDR_COMPLETION_KIND),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        });
    }
    ret
}

const ADDR_COMPLETION_KIND: CompletionItemKind = CompletionItemKind::Unit;

fn item_to_completion_item(item: &Item) -> Option<CompletionItem> {
    let x = match item {
        Item::Parameter(var, _) => CompletionItem {
            label: String::from(var.0.value.as_str()),
            kind: Some(CompletionItemKind::Variable),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },

        Item::UseModule(module_ident, alias, _) => CompletionItem {
            label: if let Some(alias) = alias {
                String::from(alias.value().as_str())
            } else {
                String::from(module_ident.value.module.value().as_str())
            },
            kind: Some(CompletionItemKind::Module),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },

        Item::UseMember(_, name, alias, _) => CompletionItem {
            label: String::from(if let Some(alias) = alias {
                alias.value.as_str()
            } else {
                name.value.as_str()
            }),
            kind: Some(CompletionItemKind::Module),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::Const(name, _) => CompletionItem {
            label: String::from(name.0.value.as_str()),
            kind: Some(CompletionItemKind::Constant),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::Var(name, _) => CompletionItem {
            label: String::from(name.0.value.as_str()),
            kind: Some(CompletionItemKind::Variable),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::Field(field, _) => CompletionItem {
            label: String::from(field.0.value.as_str()),
            kind: Some(CompletionItemKind::Field),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::Struct(x) => CompletionItem {
            label: String::from(x.name.0.value.as_str()),
            kind: Some(CompletionItemKind::Struct),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::StructNameRef(_, _, name, _) => CompletionItem {
            label: String::from(name.0.value.as_str()),
            kind: Some(CompletionItemKind::Struct),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::Fun(x) => CompletionItem {
            label: String::from(x.name.0.value.as_str()),
            kind: Some(CompletionItemKind::Function),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::BuildInType(x) => CompletionItem {
            label: String::from(x.to_static_str()),
            kind: Some(CompletionItemKind::Keyword),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::TParam(name, _) => CompletionItem {
            label: String::from(name.value.as_str()),
            kind: Some(CompletionItemKind::TypeParameter),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::SpecSchema(name, _) => CompletionItem {
            label: String::from(name.value.as_str()),
            kind: Some(CompletionItemKind::TypeParameter),
            detail: None,
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: None,
            filter_text: None,
            insert_text: None,
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
        },
        Item::ModuleName(_) => {
            // TODO.
            return None;
        }
        Item::Dummy => {
            return None;
        }
    };
    Some(x)
}
