use super::context::Context;
use super::goto_definition;
use super::item::*;
use super::modules::*;
use super::utils::*;
use im::HashMap;
use lsp_server::*;
use lsp_types::*;
use move_ir_types::location::Loc;
use std::collections::HashSet;
use std::path::*;

pub fn on_references_request(context: &mut Context, request: &Request) {
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
    let include_declaration = parameters.context.include_declaration;
    let fpath = path_concat(
        PathBuf::from(std::env::current_dir().unwrap()).as_path(),
        fpath.as_path(),
    );
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
    // first find definition.
    let mut goto_definition = goto_definition::Visitor::new(fpath.clone(), line, col);
    context.modules.run_visitor_for_file(
        &mut goto_definition,
        &manifest_dir,
        &fpath,
        layout.clone(),
    );
    let send_err = || {
        let err = format!("{:?}:{}:{} not found definition.", fpath.clone(), line, col);
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, err);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let def_loc = match goto_definition.result_loc {
        Some(x) => x,
        None => {
            send_err();
            return;
        }
    };
    if let Some(x) = context.ref_caches.get(&def_loc) {
        let r = Response::new_ok(
            request.id.clone(),
            serde_json::to_value(Some(x.clone())).unwrap(),
        );
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
        return;
    }
    let def_loc_range = match context.modules.convert_loc_range(&def_loc) {
        Some(x) => x,
        None => {
            send_err();
            return;
        }
    };
    let is_local = goto_definition
        .result_item_or_access
        .as_ref()
        .map(|x| x.is_local())
        .unwrap_or(false);
    let mut visitor = Visitor::new(def_loc, def_loc_range, include_declaration, is_local);
    if is_local {
        context
            .modules
            .run_visitor_for_file(&mut visitor, &manifest_dir, &fpath, layout);
    } else {
        context.modules.run_full_visitor(&mut visitor);
    }

    let locations = visitor.to_locations(&context.modules);
    let loc = Some(locations.clone());
    if !is_local {
        // We only cache global items.
        context.ref_caches.set(def_loc, locations.clone());
    }
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(loc).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

struct Visitor {
    def_loc: Loc,
    def_loc_range: FileRange,
    include_declaration: bool,
    refs: HashSet<Loc>,
    is_local: bool,
}

impl Visitor {
    pub(crate) fn new(
        def_loc: Loc,
        def_loc_range: FileRange,
        include_declaration: bool,
        is_local: bool,
    ) -> Self {
        Self {
            def_loc,
            include_declaration,
            refs: Default::default(),
            def_loc_range,
            is_local,
        }
    }

    pub(crate) fn to_locations(self, convert_loc: &dyn ConvertLoc) -> Vec<Location> {
        let mut file_ranges = Vec::with_capacity(self.refs.len() + 1);
        if self.include_declaration {
            if let Some(t) = convert_loc.convert_loc_range(&self.def_loc) {
                file_ranges.push(t);
            }
        }
        for x in self.refs.iter() {
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

impl GetPosition for Visitor {
    fn get_position(&self) -> (PathBuf, u32 /* line */, u32 /* col */) {
        (
            self.def_loc_range.path.clone(),
            self.def_loc_range.line,
            (self.def_loc_range.col_start + self.def_loc_range.col_end) / 2,
        )
    }
}

impl ScopeVisitor for Visitor {
    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }
    fn function_or_spec_body_should_visit(&self, start: &FileRange, end: &FileRange) -> bool {
        if self.is_local {
            Self::in_range(self, start, end)
        } else {
            true
        }
    }
    fn handle_item_or_access(
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
                            self.refs.insert(access);
                            return;
                        }
                    }
                    let (access, def) = access.access_def_loc();
                    if def == self.def_loc {
                        self.refs.insert(access);
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

#[derive(Default)]
pub struct ReferencesCache {
    caches: HashMap<Loc, Vec<lsp_types::Location>>,
}

impl ReferencesCache {
    pub fn set(&mut self, loc: Loc, v: Vec<lsp_types::Location>) {
        self.caches.insert(loc, v);
    }
    pub fn get(&self, loc: &Loc) -> Option<&Vec<lsp_types::Location>> {
        self.caches.get(loc)
    }
    pub fn clear(&mut self) {
        self.caches = Default::default();
    }
}
