use super::context::Context;
use super::goto_definition;
use super::item::*;

use super::utils::*;
use lsp_server::*;
use lsp_types::*;
use std::path::PathBuf;

/// Handles hover request of the language server
pub fn on_hover_request(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<HoverParams>(request.params.clone())
        .expect("could not deserialize hover request");
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
        "request is hover,fpath:{:?}  line:{} col:{}",
        fpath.as_path(),
        line,
        col,
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
    let mut visitor = goto_definition::Visitor::new(fpath.clone(), line, col);
    context
        .modules
        .run_visitor_for_file(&mut visitor, &manifest_dir, &fpath, layout);
    let item = visitor.result_item_or_access.clone();
    let hover = item.map(|x| hover_on_item_or_access(&x));
    let hover = hover.map(|x| Hover {
        contents: HoverContents::Scalar(MarkedString::String(x)),
        range: None,
    });
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(hover).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

fn hover_on_item_or_access(ia: &ItemOrAccess) -> String {
    let item_hover = |item: &Item| -> String {
        match item {
            Item::MoveBuildInFun(x) => String::from(x.to_notice()),
            Item::SpecBuildInFun(x) => String::from(x.to_notice()),
            _ => {
                // nothing special .
                String::from("")
            }
        }
    };
    match ia {
        ItemOrAccess::Item(item) => item_hover(item),
        ItemOrAccess::Access(access) => match access {
            Access::ApplyType(_, _, ty) => format!("{}", ty),
            Access::ExprVar(_, item) => format!("{}", item.as_ref()),
            Access::ExprAccessChain(_, _, item) => item_hover(item.as_ref()),
            Access::ExprAddressName(_) => String::from(""), // TODO handle this.
            Access::AccessFiled(AccessFiled { to, ty, .. }) => {
                format!("field {}:{}", to.0.value.as_str(), ty)
            }
            Access::KeyWords(x) => format!("keyword {}", *x),
            Access::MacroCall(macro_, _) => format!("macro {}", macro_.to_static_str()),
            Access::Friend(_, _) => String::from(""),
            Access::IncludeSchema(_, _) => String::from(""),
            Access::PragmaProperty(_) => String::from(""),
            Access::SpecFor(_, item) => format!("{}", item.as_ref()),
        },
    }
}
