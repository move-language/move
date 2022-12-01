#![allow(dead_code)]
use super::context::*;
use super::item::*;
use super::modules::*;
use crate::utils::path_concat;
use crate::utils::FileRange;
use std::path::PathBuf;

use super::scopes::*;

use lsp_server::*;
use lsp_types::*;

use move_ir_types::location::Loc;

/// Handles go-to-def request of the language server
pub fn on_go_to_def_request(context: &Context, request: &Request) {
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
    let fpath = path_concat(
        PathBuf::from(std::env::current_dir().unwrap()).as_path(),
        fpath.as_path(),
    );
    log::info!(
        "request is goto definition,fpath:{:?}  line:{} col:{}",
        fpath.as_path(),
        line,
        col,
    );
    let mut visitor = Visitor::new(fpath, line, col);
    context.modules.run_visitor(&mut visitor);

    match &visitor.result {
        Some(x) => {
            let range = Range {
                start: Position {
                    line: x.line,
                    character: x.col_start,
                },
                end: Position {
                    line: x.line,
                    character: x.col_end,
                },
            };
            let uri = Url::from_file_path(x.path.as_path()).unwrap();
            let loc = GotoDefinitionResponse::Scalar(Location::new(uri, range));
            log::info!("found location is {:?}", loc);
            let r = Response::new_ok(request.id.clone(), serde_json::to_value(loc).unwrap());
            context
                .connection
                .sender
                .send(Message::Response(r))
                .unwrap();
        }
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
    fn match_loc(&self, loc: &Loc, services: &dyn ConvertLoc) -> bool {
        let r = services.convert_loc_range(loc);
        match &r {
            Some(r) => r.in_range(self.filepath.clone(), self.line, self.col),
            None => false,
        }
    }
}

impl ScopeVisitor for Visitor {
    fn handle_item(&mut self, services: &dyn ConvertLoc, _scopes: &Scopes, item: &ItemOrAccess) {
        match item {
            ItemOrAccess::Item(item) => match item {
                // If Some special add here.
                // Right now default is enough.
                _ => {
                    let loc = item.def_loc();
                    if self.match_loc(&loc, services) {
                        if let Some(t) = services.convert_loc_range(&loc) {
                            self.result = Some(t);
                        }
                    }
                }
            },

            ItemOrAccess::Access(access) => match item {
                _ => {
                    log::trace!("access:{}", access);

                    let locs = access.access_def_loc();
                    if self.match_loc(&locs.0, services) {
                        if let Some(t) = services.convert_loc_range(&locs.1) {
                            self.result = Some(t);
                        }
                    }
                }
            },
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
            "goto_definition,file:{:?} line:{} col:{}",
            self.filepath, self.line, self.col
        )
    }
}
