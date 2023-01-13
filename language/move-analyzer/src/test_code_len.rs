use super::context::*;
use super::item::*;
use super::utils::*;
use lsp_server::*;
use lsp_types::CodeLens;
use lsp_types::Command;
use move_compiler::shared::Identifier;
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, str::FromStr};

pub fn move_gen_test_code_lens(context: &Context, request: &lsp_server::Request) {
    let parameters = serde_json::from_value::<FilePath>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = PathBuf::from_str(parameters.filepath.as_str()).unwrap();
    let fpath = path_concat(
        PathBuf::from(std::env::current_dir().unwrap()).as_path(),
        fpath.as_path(),
    );
    let send_err = |msg: String| {
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let (manifest, layout) = match discover_manifest_and_kind(fpath.as_path()) {
        Some(x) => x,
        None => {
            send_err(String::from("get manifest failed,"));
            return;
        }
    };
    let mut v = Visitor::new();
    context
        .modules
        .run_visitor_for_file(&mut v, &manifest, &fpath, layout);
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(v.result).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct FilePath {
    pub filepath: String,
}

pub struct Visitor {
    result: Vec<CodeLens>,
}
impl Visitor {
    fn new() -> Self {
        Self { result: vec![] }
    }
}

impl std::fmt::Display for Visitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "move test code lens")
    }
}

impl super::modules::ScopeVisitor for Visitor {
    fn handle_item_or_access(
        &mut self,
        services: &dyn crate::modules::HandleItemService,
        scopes: &crate::scopes::Scopes,
        item: &crate::item::ItemOrAccess,
    ) {
        match item {
            ItemOrAccess::Item(x) => match x {
                Item::Fun(f) => {
                    if f.is_test {
                        let addr = scopes.get_current_addr_and_module_name();
                        if let Some(range) = services.convert_loc_range(&f.name.loc()) {
                            self.result.push(CodeLens {
                                range: range.mk_location().range,
                                command: Some(Command::new(
                                    format!("RUN TEST"),
                                    format!("sui"),
                                    Some({
                                        let mut x = vec![
                                            serde_json::Value::String("move".to_string()),
                                            serde_json::Value::String("test".to_string()),
                                        ];
                                        x.push(serde_json::Value::String(format!(
                                            "0x{}::{}::{}",
                                            addr.addr.short_str_lossless(),
                                            addr.name.value().as_str(),
                                            f.name.0.value.as_str()
                                        )));
                                        x
                                    }),
                                )),
                                data: None,
                            });
                        }
                    }
                }
                _ => {}
            },
            ItemOrAccess::Access(_) => {}
        }
    }

    fn function_or_spec_body_should_visit(&self, _range: &FileRange) -> bool {
        false
    }

    fn visit_fun_or_spec_body(&self) -> bool {
        false
    }

    fn finished(&self) -> bool {
        false
    }
}
