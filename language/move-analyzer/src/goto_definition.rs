use std::path::PathBuf;

use crate::utils::FileRange;

use super::context::*;
use super::item::*;
use super::modules::*;

use super::scopes::*;

use lsp_server::*;
use lsp_types::*;

use move_compiler::shared::TName;
use move_ir_types::location::Loc;

/// Handles go-to-def request of the language server
pub fn on_go_to_def_request(context: &mut Context, request: &Request) {
    let parameters = serde_json::from_value::<GotoDefinitionParams>(request.params.clone())
        .expect("could not deserialize go-to-def request");

    let fpath = parameters
        .text_document_position_params
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let loc = parameters.text_document_position_params.position;
    let line = loc.line;
    let col = loc.character;

    let mut visitor = Visitor::new(fpath, line, col);
    context.modules.run_visitor(&mut visitor);
    match &visitor.result {
        Some(x) => context.connection.sender.send(unimplemented!()).unwrap(),
        None => log::error!(
            "{:?}:{}:{} not found definition.",
            visitor.filepath,
            line,
            col
        ),
    }
}

pub(crate) struct Visitor {
    /// The file we are looking for.
    pub(crate) filepath: PathBuf,
    pub(crate) line: u32,
    pub(crate) col: u32,
    pub(crate) result: Option<FileRange>,
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
    fn match_loc(&self, loc: &Loc, services: &dyn ModuleServices) -> bool {
        let r = services.convert_loc_range(loc);
        match &r {
            Some(r) => r.in_range(self.filepath.clone(), self.line, self.col),
            None => false,
        }
    }
}

impl ScopeVisitor for Visitor {
    fn handle_item(&mut self, services: &dyn ModuleServices, _scopes: &Scopes, item: &Item) {
        match item {
            Item::Parameter(var, _) => {
                if self.match_loc(&var.borrow().0, services) {
                    if let Some(t) = services.convert_loc_range(&var.borrow().0) {
                        self.result = Some(t);
                    }
                }
            }
            Item::UseMember(name, item) => {
                if self.match_loc(&name.loc, services) {
                    if let Some(t) = services.convert_loc_range(item.as_ref().def_loc()) {
                        self.result = Some(t);
                    }
                }
            }

            Item::ApplyType(chain, ty) => {
                if self.match_loc(&get_access_chain_name(chain).loc, services) {
                    if let Some(t) =
                        services.convert_loc_range(ty.as_ref().chain_resolve_type_loc())
                    {
                        self.result = Some(t);
                    }
                }
            }
            _ => {}
        }
    }

    fn file_should_visit(&self, p: &PathBuf) -> bool {
        let x = self.filepath == *p;
        println!("xxxxxxxxx {:?} {:?} match:{:?}", p, self.filepath, x);
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
            "goto_definition,file:{:?} line:{} col:{}",
            self.filepath, self.line, self.col
        )
    }
}
