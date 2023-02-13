#![allow(deprecated)] // https://github.com/rust-lang/rust/issues/102777

use super::context::*;
use super::modules::*;

use lsp_server::*;
use lsp_types::*;
use move_compiler::parser::ast::SpecBlockMember_;
use move_compiler::shared::Identifier;

pub fn on_document_symbol_request(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<DocumentSymbolParams>(request.params.clone())
        .expect("could not deserialize document symbol request");

    let fpath = parameters.text_document.uri.to_file_path().unwrap();
    let modules = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => return,
    };
    let mut result = vec![];
    let _ = modules.get_defs(&fpath, |defs| {
        defs.with_module_member(|_, _, m, _| match m {
            move_compiler::parser::ast::ModuleMember::Function(f) => {
                if let Some(range) = modules.convert_loc_range(&f.name.loc()) {
                    result.push(SymbolInformation {
                        name: format!("{}", f.name.value().as_str()),
                        location: range.mk_location(),
                        kind: SymbolKind::Function,
                        tags: None,
                        container_name: None,
                        deprecated: None,
                    });
                }
            }
            move_compiler::parser::ast::ModuleMember::Struct(f) => {
                if let Some(range) = modules.convert_loc_range(&f.name.loc()) {
                    result.push(SymbolInformation {
                        name: format!("{}", f.name.value().as_str()),
                        location: range.mk_location(),
                        kind: SymbolKind::Struct,
                        tags: None,
                        container_name: None,
                        deprecated: None,
                    });
                }
            }

            move_compiler::parser::ast::ModuleMember::Constant(f) => {
                if let Some(range) = modules.convert_loc_range(&f.name.loc()) {
                    result.push(SymbolInformation {
                        name: format!("{}", f.name.value().as_str()),
                        location: range.mk_location(),
                        kind: SymbolKind::Constant,
                        tags: None,
                        container_name: None,
                        deprecated: None,
                    });
                }
            }
            move_compiler::parser::ast::ModuleMember::Spec(s) => {
                for m in s.value.members.iter() {
                    match &m.value {
                        SpecBlockMember_::Function {
                            uninterpreted: _uninterpreted,
                            name,
                            signature: _,
                            body: _,
                        } => {
                            if let Some(range) = modules.convert_loc_range(&name.loc()) {
                                {
                                    result.push(SymbolInformation {
                                        name: format!("{}", name.value().as_str()),
                                        location: range.mk_location(),
                                        kind: SymbolKind::Function,
                                        tags: None,
                                        container_name: None,
                                        deprecated: None,
                                    });
                                }
                            }
                        }
                        _ => {}
                    }
                }
                match &s.value.target.value {
                    move_compiler::parser::ast::SpecBlockTarget_::Member(name, _) => {
                        if let Some(range) = modules.convert_loc_range(&name.loc) {
                            result.push(SymbolInformation {
                                name: format!("{}", name.value.as_str()),
                                location: range.mk_location(),
                                kind: SymbolKind::Property,
                                tags: None,
                                container_name: None,
                                deprecated: None,
                            });
                        }
                    }
                    move_compiler::parser::ast::SpecBlockTarget_::Schema(name, _) => {
                        if let Some(range) = modules.convert_loc_range(&name.loc) {
                            result.push(SymbolInformation {
                                name: format!("{}", name.value.as_str()),
                                kind: SymbolKind::Property,
                                tags: None,
                                deprecated: None,
                                location: range.mk_location(),
                                container_name: None,
                            });
                        }
                    }
                    _ => {}
                };
            }
            _ => {}
        });
    });
    let result = Response::new_ok(
        request.id.clone(),
        serde_json::to_value(DocumentSymbolResponse::Flat(result)).unwrap(),
    );
    context
        .connection
        .sender
        .send(Message::Response(result))
        .unwrap();
}
