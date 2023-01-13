use super::context::*;
use super::modules::*;
use super::utils::discover_manifest_and_kind;
use lsp_server::*;
use lsp_types::*;
use move_compiler::shared::Identifier;

pub fn on_document_symbol_request(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<DocumentSymbolParams>(request.params.clone())
        .expect("could not deserialize document symbol request");

    let fpath = parameters.text_document.uri.to_file_path().unwrap();
    let (manifest_dir, layout) = match discover_manifest_and_kind(fpath.as_path()) {
        Some(x) => x,
        None => {
            log::error!(
                "fpath:{:?} can't find manifest_dir or kind",
                fpath.as_path()
            );
            return;
        }
    };
    let defs = context.modules.get_defs(&manifest_dir, &fpath, layout);
    let mut result = vec![];
    defs.with_module_member(|_, _, m, _| match m {
        move_compiler::parser::ast::ModuleMember::Function(f) => {
            if let Some(range) = context.modules.convert_loc_range(&f.name.loc()) {
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
            if let Some(range) = context.modules.convert_loc_range(&f.name.loc()) {
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
            if let Some(range) = context.modules.convert_loc_range(&f.name.loc()) {
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
        move_compiler::parser::ast::ModuleMember::Spec(s) => match &s.value.target.value {
            move_compiler::parser::ast::SpecBlockTarget_::Member(name, _) => {
                if let Some(range) = context.modules.convert_loc_range(&name.loc) {
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
                if let Some(range) = context.modules.convert_loc_range(&name.loc) {
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
            _ => {}
        },
        _ => {}
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
