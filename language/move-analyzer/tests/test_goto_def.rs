// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[cfg(test)]
mod tests {
    use lsp_server::{Connection, Request, Response};
    use move_analyzer::{
        context::{Context, FileDiags, MultiProject},
        goto_definition, symbols,
        utils::*,
        vfs::VirtualFileSystem,
    };
    use move_command_line_common::files::FileHash;
    use move_compiler::shared::*;
    use serde_json::json;
    use std::{
        path::PathBuf,
        sync::{Arc, Mutex},
        time::Duration,
    };
    pub use url::Url;

    fn update_defs(context: &mut Context, fpath: PathBuf, content: &str) {
        use move_analyzer::syntax::parse_file_string;
        let file_hash = FileHash::new(content);
        let mut env = CompilationEnv::new(Flags::testing());
        let defs = parse_file_string(&mut env, file_hash, content);
        let defs = match defs {
            std::result::Result::Ok(x) => x,
            std::result::Result::Err(d) => {
                log::error!("update file failed,err:{:?}", d);
                return;
            }
        };
        let (defs, _) = defs;
        context.projects.update_defs(fpath.clone(), defs);
        context.ref_caches.clear();
        context
            .projects
            .hash_file
            .as_ref()
            .borrow_mut()
            .update(fpath.clone(), file_hash);
        context
            .projects
            .file_line_mapping
            .as_ref()
            .borrow_mut()
            .update(fpath, content);
    }

    #[test]
    fn test_on_go_to_def_request_001() {
        let (connection, _) = Connection::stdio();
        let symbols = Arc::new(Mutex::new(symbols::Symbolicator::empty_symbols()));

        let mut mock_ctx = Context {
            projects: MultiProject::new(),
            connection,
            files: VirtualFileSystem::default(),
            symbols,
            ref_caches: Default::default(),
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/symbols/sources/M1.move").as_path(),
        );

        eprintln!("fpath = {:?}", fpath.to_str());
        let (mani, _) = match discover_manifest_and_kind(&fpath) {
            Some(x) => x,
            None => {
                log::error!("not move project.");
                return;
            }
        };
        match mock_ctx.projects.get_project(&fpath) {
            Some(_) => {
                if let Ok(x) = std::fs::read_to_string(fpath.as_path()) {
                    update_defs(&mut mock_ctx, fpath.clone(), x.as_str());
                };
                return;
            }
            None => {
                eprintln!("project '{:?}' not found try load.", fpath.as_path());
            }
        };
        let p = match mock_ctx.projects.load_project(&mock_ctx.connection, &mani) {
            anyhow::Result::Ok(x) => x,
            anyhow::Result::Err(e) => {
                log::error!("load project failed,err:{:?}", e);
                return;
            }
        };
        mock_ctx.projects.insert_project(p);

        let params_json = json!({
            "position": {
                "line": 25,
                "character": 27
            },
            "textDocument": {
                "uri": "file:///".to_string() + fpath.to_str().unwrap()
            },
        });
        let request = Request {
            id: "go_to_def_request_001".to_string().into(),
            method: String::from("textDocument/definition"),
            params: params_json,
        };

        let actual_r = goto_definition::on_go_to_def_request(&mock_ctx, &request);
        let expect_r = Response::new_ok(
            "go_to_def_request_001".to_string().into(),
            json!([{
                "range":{
                    "end":{
                        "character":32,
                        "line":6
                    },
                    "start":{
                        "character":15,
                        "line":6
                    }
                },
                "uri": ("file:///".to_string() + path_concat(
                            std::env::current_dir().unwrap().as_path(),
                            PathBuf::from("tests/symbols/sources/M2.move").as_path()).to_str().unwrap()
                       ).replace('\\', "/")
            }]),
        );
        std::thread::sleep(Duration::new(1, 0));
        eprintln!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", actual_r);
        eprintln!("\n");
        eprintln!("expect_r = {:?}", expect_r);
        eprintln!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }

    #[test]
    fn test_on_go_to_type_def_request_002() {
        let (connection, _) = Connection::stdio();
        let symbols = Arc::new(Mutex::new(symbols::Symbolicator::empty_symbols()));

        let mut mock_ctx = Context {
            projects: MultiProject::new(),
            connection,
            files: VirtualFileSystem::default(),
            symbols,
            ref_caches: Default::default(),
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/symbols/sources/M1.move").as_path(),
        );

        eprintln!("fpath = {:?}", fpath.to_str());
        let (mani, _) = match discover_manifest_and_kind(&fpath) {
            Some(x) => x,
            None => {
                log::error!("not move project.");
                return;
            }
        };
        match mock_ctx.projects.get_project(&fpath) {
            Some(_) => {
                if let Ok(x) = std::fs::read_to_string(fpath.as_path()) {
                    update_defs(&mut mock_ctx, fpath.clone(), x.as_str());
                };
                return;
            }
            None => {
                eprintln!("project '{:?}' not found try load.", fpath.as_path());
            }
        };
        let p = match mock_ctx.projects.load_project(&mock_ctx.connection, &mani) {
            anyhow::Result::Ok(x) => x,
            anyhow::Result::Err(e) => {
                log::error!("load project failed,err:{:?}", e);
                return;
            }
        };
        mock_ctx.projects.insert_project(p);

        let params_json = json!({
            "position": {
                "line": 24,
                "character": 45
            },
            "textDocument": {
                "uri": "file:///".to_string() + fpath.to_str().unwrap()
            },
        });
        let request = Request {
            id: "go_to_type_def_request_002".to_string().into(),
            method: String::from("textDocument/typeDefinition"),
            params: params_json,
        };

        let actual_r = goto_definition::on_go_to_type_def_request(&mock_ctx, &request);
        let expect_r = Response::new_ok(
            "go_to_type_def_request_002".to_string().into(),
            json!([{
                "range":{
                    "end":{
                        "character":26,
                        "line":2
                    },
                    "start":{
                        "character":11,
                        "line":2
                    }
                },
                "uri": ("file:///".to_string() + path_concat(
                            std::env::current_dir().unwrap().as_path(),
                            PathBuf::from("tests/symbols/sources/M2.move").as_path()).to_str().unwrap()
                       ).replace('\\', "/")
            }]),
        );
        std::thread::sleep(Duration::new(1, 0));
        eprintln!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", actual_r);
        eprintln!("\n");
        eprintln!("expect_r = {:?}", expect_r);
        eprintln!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }
}
