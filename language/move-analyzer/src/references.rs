use super::context::Context;
use super::goto_definition;
use super::item::*;
use super::modules::*;
use super::utils::*;
use lsp_server::*;
use lsp_types::*;
use move_ir_types::location::Loc;
use std::collections::HashSet;
use std::path::*;

pub fn on_references_request(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<ReferenceParams>(request.params.clone())
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
    let include_decl = parameters.context.include_declaration;

    let fpath = path_concat(
        PathBuf::from(std::env::current_dir().unwrap()).as_path(),
        fpath.as_path(),
    );
    // first find definition.
    let mut goto_definition = goto_definition::Visitor::new(fpath.clone(), line, col);
    context.modules.run_visitor(&mut goto_definition);
    let send_err = || {
        let err = format!("{:?}:{}:{} not found definition.", fpath.clone(), line, col);
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, err);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let result = match goto_definition.result_loc {
        Some(x) => x,
        None => {
            send_err();
            return;
        }
    };
    let mut visitor = Visitor::new(result, include_decl);
    context.modules.run_visitor(&mut visitor);
    let locations = visitor.to_locations(&context.modules);
    let loc = GotoDefinitionResponse::Array(locations);
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(loc).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

struct Visitor {
    def_loc: Loc,
    include_decl: bool,
    results: HashSet<Loc>,
}

impl Visitor {
    pub(crate) fn new(def_loc: Loc, include_decl: bool) -> Self {
        Self {
            def_loc,
            include_decl,
            results: Default::default(),
        }
    }

    pub(crate) fn to_locations(self, convert_loc: &dyn ConvertLoc) -> Vec<Location> {
        let mut file_ranges = Vec::with_capacity(self.results.len() + 1);
        if self.include_decl {
            if let Some(t) = convert_loc.convert_loc_range(&self.def_loc) {
                file_ranges.push(t);
            }
        }
        for x in self.results.iter() {
            if let Some(t) = convert_loc.convert_loc_range(x) {
                file_ranges.push(t);
            }
        }

        let mut ret = Vec::with_capacity(file_ranges.len());
        for xx in file_ranges.iter() {
            ret.push(Location {
                uri: Url::from_file_path(&xx.path).unwrap(),
                range: Range {
                    start: lsp_types::Position {
                        line: xx.line,
                        character: xx.col_start,
                    },
                    end: lsp_types::Position {
                        line: xx.line,
                        character: xx.col_end,
                    },
                },
            })
        }

        ret
    }
}

impl ScopeVisitor for Visitor {
    fn file_should_visit(&self, _p: &PathBuf) -> bool {
        // TODO
        true
    }
    fn handle_item(
        &mut self,
        _services: &dyn HandleItemService,
        _scopes: &crate::scopes::Scopes,
        item: &crate::item::ItemOrAccess,
    ) {
        match item {
            ItemOrAccess::Item(_) => {}

            ItemOrAccess::Access(access) => match item {
                _ => {
                    log::trace!("access:{}", access);
                    if let Some((access, def)) = access.access_module() {
                        if def == self.def_loc {
                            self.results.insert(access);
                            return;
                        }
                    }
                    let (access, def) = access.access_def_loc();
                    if def == self.def_loc {
                        self.results.insert(access);
                        return;
                    }
                }
            },
        }
    }
    fn finished(&self) -> bool {
        false
    }
}

impl std::fmt::Display for Visitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "find references for {:?}", self.def_loc)
    }
}
