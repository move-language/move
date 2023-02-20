// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

// TODO the memory profiling not working,figure it out.
// Sometimes I want run profiling on my local machine.
#![allow(dead_code)]
use anyhow::Result;
use clap::Parser;
use crossbeam::channel::bounded;
use crossbeam::channel::select;
use crossbeam::channel::Sender;
use log::{Level, Metadata, Record};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    notification::Notification as _, request::Request as _, CompletionOptions,
    HoverProviderCapability, OneOf, SaveOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TypeDefinitionProviderCapability, WorkDoneProgressOptions,
};

#[allow(unused)]
use move_analyzer::modules::Ending;
use move_command_line_common::files::FileHash;
use move_compiler::diagnostics::Diagnostics;
use move_compiler::{shared::*, PASS_TYPING};

use std::sync::{Arc, Mutex};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use move_analyzer::{
    code_lens,
    completion::on_completion_request,
    context::{Context, FileDiags, MultiProject},
    document_symbol, goto_definition, hover,
    modules::ConvertLoc,
    references,
    utils::*,
};

use url::Url;

use jemalloc_ctl::{Access, AsName};
use jemallocator;
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;
const PROF_ACTIVE: &'static [u8] = b"prof.active\0";
const PROF_DUMP: &'static [u8] = b"prof.dump\0";
const PROFILE_OUTPUT: &'static [u8] = b"profile.out\0";

fn set_memory_prof_active(active: bool) {
    let name = PROF_ACTIVE.name();
    name.write(active).expect("Should succeed to set prof");
}

struct SimpleLogger;
impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Error
    }
    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            eprintln!("{} - {}", record.level(), record.args());
        }
    }
    fn flush(&self) {}
}
const LOGGER: SimpleLogger = SimpleLogger;

pub fn init_log() {
    log::set_logger(&LOGGER)
        .map(|()| log::set_max_level(log::LevelFilter::Error))
        .unwrap()
}

#[derive(Parser)]
#[clap(author, version, about)]
struct Options {}

fn main() {
    // cpu_pprof(20);
    // memory_pprof(20);

    // For now, move-analyzer only responds to options built-in to clap,
    // such as `--help` or `--version`.
    Options::parse();
    init_log();
    // stdio is used to communicate Language Server Protocol requests and responses.
    // stderr is used for logging (and, when Visual Studio Code is used to communicate with this
    // server, it captures this output in a dedicated "output channel").
    let exe = std::env::current_exe()
        .unwrap()
        .to_string_lossy()
        .to_string();
    log::error!(
        "Starting language server '{}' communicating via stdio...",
        exe
    );

    let (connection, io_threads) = Connection::stdio();
    let mut context = Context {
        projects: MultiProject::new(),
        connection,
        ref_caches: Default::default(),
        diag_version: FileDiags::new(),
    };

    let (id, _client_response) = context
        .connection
        .initialize_start()
        .expect("could not start connection initialization");

    let capabilities = serde_json::to_value(lsp_types::ServerCapabilities {
        // The server receives notifications from the client as users open, close,
        // and modify documents.
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                // TODO: We request that the language server client send us the entire text of any
                // files that are modified. We ought to use the "incremental" sync kind, which would
                // have clients only send us what has changed and where, thereby requiring far less
                // data be sent "over the wire." However, to do so, our language server would need
                // to be capable of applying deltas to its view of the client's open files. See the
                // 'move_analyzer::vfs' module for details.
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(
                    SaveOptions {
                        include_text: Some(true),
                    }
                    .into(),
                ),
            },
        )),
        selection_range_provider: None,
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        // The server provides completions as a user is typing.
        completion_provider: Some(CompletionOptions {
            resolve_provider: None,
            trigger_characters: Some({
                let mut c = vec![":".to_string(), ".".to_string()];
                for x in 'a'..='z' {
                    c.push(String::from(x as char));
                }
                for x in 'A'..='Z' {
                    c.push(String::from(x as char));
                }
                c.push(String::from("0"));
                c
            }),
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
            completion_item: None,
        }),
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
        references_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        inlay_hint_provider: Some(OneOf::Left(true)),
        code_lens_provider: Some(lsp_types::CodeLensOptions {
            resolve_provider: Some(true),
        }),
        ..Default::default()
    })
    .expect("could not serialize server capabilities");
    context
        .connection
        .initialize_finish(
            id,
            serde_json::json!({
                "capabilities": capabilities,
            }),
        )
        .expect("could not finish connection initialization");
    let (diag_sender, diag_receiver) = bounded::<(PathBuf, Diagnostics)>(1);
    let diag_sender = Arc::new(Mutex::new(diag_sender));

    loop {
        select! {
            recv(diag_receiver) -> message => {
                match message {
                    Ok ((mani ,x)) => {
                        send_diag(&mut context,mani,x);
                    }
                    Err(error) => log::error!("IDE diag message error: {:?}", error),
                }
            }
            recv(context.connection.receiver) -> message => {
                try_reload_projects_has_not_exists(&mut context);
                match message {
                    Ok(Message::Request(request)) => on_request(&mut context, &request),
                    Ok(Message::Response(response)) => on_response(&context, &response),
                    Ok(Message::Notification(notification)) => {
                        match notification.method.as_str() {
                            lsp_types::notification::Exit::METHOD => break,
                            lsp_types::notification::Cancel::METHOD => {
                                // TODO: Currently the server does not implement request cancellation.
                                // It ought to, especially once it begins processing requests that may
                                // take a long time to respond to.
                            }
                            _ => on_notification(&mut context, &notification ,diag_sender.clone()),
                        }
                    }
                    Err(error) => log::error!("IDE lsp client message error: {:?}", error),
                }
            }
        };
    }
    io_threads.join().expect("I/O threads could not finish");
    log::error!("Shut down language server '{}'.", exe);
}

fn try_reload_projects_has_not_exists(context: &mut Context) {
    context.projects.try_reload_projects_has_not_exists();
}
fn on_request(context: &mut Context, request: &Request) {
    log::info!("receive method:{}", request.method.as_str());
    match request.method.as_str() {
        lsp_types::request::Completion::METHOD => on_completion_request(context, request),
        lsp_types::request::GotoDefinition::METHOD => {
            goto_definition::on_go_to_def_request(context, request);
        }
        lsp_types::request::GotoTypeDefinition::METHOD => {
            goto_definition::on_go_to_type_def_request(context, request);
        }
        lsp_types::request::References::METHOD => {
            references::on_references_request(context, request);
        }
        lsp_types::request::HoverRequest::METHOD => {
            hover::on_hover_request(context, request);
        }
        lsp_types::request::DocumentSymbolRequest::METHOD => {
            document_symbol::on_document_symbol_request(context, request);
        }
        lsp_types::request::CodeLensRequest::METHOD => {
            code_lens::move_get_test_code_lens(context, request);
        }
        _ => log::error!("handle request '{}' from client", request.method),
    }
}

fn on_response(_context: &Context, _response: &Response) {
    log::error!("handle response from client");
}

type DiagSender = Arc<Mutex<Sender<(PathBuf, Diagnostics)>>>;

fn on_notification(context: &mut Context, notification: &Notification, diag_sender: DiagSender) {
    fn update_defs(context: &mut Context, fpath: PathBuf, content: &str) {
        use move_analyzer::syntax::parse_file_string;
        let file_hash = FileHash::new(content);
        let mut env = CompilationEnv::new(Flags::testing());
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
            .update(fpath.clone(), content);
        let defs = parse_file_string(&mut env, file_hash, content);
        let defs = match defs {
            std::result::Result::Ok(x) => x,
            std::result::Result::Err(d) => {
                log::error!("update file failed,err:{:?}", d);
                return;
            }
        };
        let (defs, _) = defs;
        let old_defs = context.projects.update_defs(fpath.clone(), defs);
        context
            .projects
            .get_projects_mut(&fpath)
            .into_iter()
            .for_each(|modules| modules.update_defs(&fpath, old_defs.as_ref()));
        context.ref_caches.clear();
    }

    match notification.method.as_str() {
        lsp_types::notification::DidSaveTextDocument::METHOD => {
            use lsp_types::DidSaveTextDocumentParams;
            let parameters =
                serde_json::from_value::<DidSaveTextDocumentParams>(notification.params.clone())
                    .expect("could not deserialize DidSaveTextDocumentParams request");
            let fpath = parameters.text_document.uri.to_file_path().unwrap();
            let fpath = path_concat(&PathBuf::from(std::env::current_dir().unwrap()), &fpath);
            let content = std::fs::read_to_string(fpath.as_path());
            let content = match content {
                Ok(x) => x,
                Err(err) => {
                    log::error!("read file failed,err:{:?}", err);
                    return;
                }
            };
            update_defs(context, fpath.clone(), content.as_str());
            make_diag(context, diag_sender, fpath);
        }

        lsp_types::notification::DidChangeTextDocument::METHOD => {
            use lsp_types::DidChangeTextDocumentParams;
            let parameters =
                serde_json::from_value::<DidChangeTextDocumentParams>(notification.params.clone())
                    .expect("could not deserialize DidChangeTextDocumentParams request");
            let fpath = parameters.text_document.uri.to_file_path().unwrap();
            let fpath = path_concat(&PathBuf::from(std::env::current_dir().unwrap()), &fpath);
            update_defs(
                context,
                fpath,
                parameters.content_changes.last().unwrap().text.as_str(),
            );
        }
        lsp_types::notification::DidOpenTextDocument::METHOD => {
            use lsp_types::DidOpenTextDocumentParams;
            let parameters =
                serde_json::from_value::<DidOpenTextDocumentParams>(notification.params.clone())
                    .expect("could not deserialize DidOpenTextDocumentParams request");
            let fpath = parameters.text_document.uri.to_file_path().unwrap();
            let fpath = path_concat(&PathBuf::from(std::env::current_dir().unwrap()), &fpath);
            let (mani, _) = match discover_manifest_and_kind(&fpath) {
                Some(x) => x,
                None => {
                    log::error!("not move project.");
                    send_not_project_file_error(context, fpath, true);
                    return;
                }
            };
            match context.projects.get_project(&fpath) {
                Some(_) => return,
                None => {
                    log::error!("project '{:?}' not found try load.", fpath.as_path());
                }
            };
            let p = match context.projects.load_project(&context.connection, &mani) {
                anyhow::Result::Ok(x) => x,
                anyhow::Result::Err(e) => {
                    log::error!("load project failed,err:{:?}", e);
                    return;
                }
            };
            context.projects.insert_project(p);
            make_diag(context, diag_sender, fpath);
        }
        lsp_types::notification::DidCloseTextDocument::METHOD => {
            use lsp_types::DidCloseTextDocumentParams;
            let parameters =
                serde_json::from_value::<DidCloseTextDocumentParams>(notification.params.clone())
                    .expect("could not deserialize DidCloseTextDocumentParams request");
            let fpath = parameters.text_document.uri.to_file_path().unwrap();
            let fpath = path_concat(&PathBuf::from(std::env::current_dir().unwrap()), &fpath);
            let (_, _) = match discover_manifest_and_kind(&fpath) {
                Some(x) => x,
                None => {
                    log::error!("not move project.");
                    send_not_project_file_error(context, fpath, false);
                    return;
                }
            };
        }
        _ => log::error!("handle notification '{}' from client", notification.method),
    }
}

fn cpu_pprof(seconds: u64) {
    use std::fs::File;
    use std::time::Duration;
    let guard = pprof::ProfilerGuardBuilder::default()
        .frequency(1000)
        .blocklist(&["libc", "libgcc", "pthread", "vdso"])
        .build()
        .unwrap();
    std::thread::spawn(move || loop {
        std::thread::sleep(Duration::new(seconds, 0));
        match guard.report().build() {
            Result::Ok(report) => {
                let file = File::create("/Users/temp/.move-analyzer/flamegraph.svg").unwrap();
                report.flamegraph(file).unwrap();
            }
            Result::Err(e) => {
                log::error!("build report failed,err:{}", e);
            }
        };
    });
}

fn memory_pprof(seconds: u64) {
    use std::time::Duration;
    std::thread::spawn(move || loop {
        set_memory_prof_active(true);
        std::thread::sleep(Duration::new(seconds, 0));
        set_memory_prof_active(false);
        dump_memory_profile();
    });
}

fn dump_memory_profile() {
    let name = PROF_DUMP.name();
    name.write(PROFILE_OUTPUT)
        .expect("Should succeed to dump profile")
}

fn get_package_compile_diagnostics(
    pkg_path: &Path,
) -> Result<move_compiler::diagnostics::Diagnostics> {
    use anyhow::*;
    use move_package::compilation::build_plan::BuildPlan;
    use tempfile::tempdir;
    let build_config = move_package::BuildConfig {
        test_mode: true,
        install_dir: Some(tempdir().unwrap().path().to_path_buf()),
        skip_fetch_latest_git_deps: true,
        ..Default::default()
    };
    // resolution graph diagnostics are only needed for CLI commands so ignore them by passing a
    // vector as the writer
    let resolution_graph = build_config.resolution_graph_for_package(pkg_path, &mut Vec::new())?;
    let build_plan = BuildPlan::create(resolution_graph)?;
    let mut diagnostics = None;
    build_plan.compile_with_driver(&mut std::io::sink(), |compiler| {
        let (_, compilation_result) = compiler.run::<PASS_TYPING>()?;
        match compilation_result {
            std::result::Result::Ok(_) => {}
            std::result::Result::Err(diags) => {
                diagnostics = Some(diags);
            }
        };
        Ok(Default::default())
    })?;
    match diagnostics {
        Some(x) => Ok(x),
        None => Ok(Default::default()),
    }
}

fn make_diag(context: &Context, diag_sender: DiagSender, fpath: PathBuf) {
    let (mani, _) = match move_analyzer::utils::discover_manifest_and_kind(fpath.as_path()) {
        Some(x) => x,
        None => {
            log::error!("manifest not found.");
            return;
        }
    };
    match context.projects.get_project(&fpath) {
        Some(x) => {
            if !x.load_ok() {
                //
                // let k = url::Url::from_file_path(fpath).unwrap();
                // let ds = lsp_types::PublishDiagnosticsParams::new(
                //     k,
                //     vec![lsp_types::Diagnostic {
                //         range: lsp_types::Range {
                //             start: lsp_types::Position {
                //                 line: 0,
                //                 character: 0,
                //             },
                //             end: lsp_types::Position {
                //                 line: 0,
                //                 character: 0,
                //             },
                //         },
                //         message: format!(
                //             "The project is incomplete,Maybe you can execute a 'sui move build'"
                //         ),
                //         ..Default::default()
                //     }],
                //     None,
                // );
                // context
                //     .connection
                //     .sender
                //     .send(lsp_server::Message::Notification(Notification {
                //         method: format!("{}", lsp_types::notification::PublishDiagnostics::METHOD),
                //         params: serde_json::to_value(ds).unwrap(),
                //     }))
                //     .unwrap();
                return;
            }
        }
        None => return,
    };
    std::thread::spawn(move || {
        let x = match get_package_compile_diagnostics(mani.as_path()) {
            Ok(x) => x,
            Err(err) => {
                log::error!("get_package_compile_diagnostics failed,err:{:?}", err);
                return;
            }
        };
        diag_sender.lock().unwrap().send((mani, x)).unwrap();
    });
}

fn send_not_project_file_error(context: &mut Context, fpath: PathBuf, is_open: bool) {
    let url = url::Url::from_file_path(fpath.as_path()).unwrap();
    let content = std::fs::read_to_string(fpath.as_path()).unwrap_or("".to_string());
    let lines: Vec<_> = content.lines().collect();
    let last_line = lines.len();
    let last_col = lines.last().map(|x| (*x).len()).unwrap_or(1);
    let ds = lsp_types::PublishDiagnosticsParams::new(
        url,
        if is_open {
            vec![lsp_types::Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: last_line as u32,
                        character: last_col as u32,
                    },
                },
                message: "This file doesn't belong to a move project.\nMaybe a build artifact???"
                    .to_string(),
                ..Default::default()
            }]
        } else {
            vec![]
        },
        None,
    );
    context
        .connection
        .sender
        .send(lsp_server::Message::Notification(Notification {
            method: lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
            params: serde_json::to_value(ds).unwrap(),
        }))
        .unwrap();
}
fn send_diag(context: &mut Context, mani: PathBuf, x: Diagnostics) {
    let mut result: HashMap<Url, Vec<lsp_types::Diagnostic>> = HashMap::new();
    for x in x.into_codespan_format() {
        let (s, msg, (loc, m), _, notes) = x;
        match context.projects.convert_loc_range(&loc) {
            Some(r) => {
                let url = url::Url::from_file_path(r.path.as_path()).unwrap();
                let d = lsp_types::Diagnostic {
                    range: r.mk_location().range,
                    severity: Some(match s {
                        codespan_reporting::diagnostic::Severity::Bug => {
                            lsp_types::DiagnosticSeverity::ERROR
                        }
                        codespan_reporting::diagnostic::Severity::Error => {
                            lsp_types::DiagnosticSeverity::ERROR
                        }
                        codespan_reporting::diagnostic::Severity::Warning => {
                            lsp_types::DiagnosticSeverity::WARNING
                        }
                        codespan_reporting::diagnostic::Severity::Note => {
                            lsp_types::DiagnosticSeverity::INFORMATION
                        }
                        codespan_reporting::diagnostic::Severity::Help => {
                            lsp_types::DiagnosticSeverity::HINT
                        }
                    }),
                    message: format!(
                        "{}\n{}{:?}",
                        msg,
                        m,
                        if notes.len() > 0 {
                            format!(" {:?}", notes)
                        } else {
                            "".to_string()
                        }
                    ),
                    ..Default::default()
                };
                if let Some(a) = result.get_mut(&url) {
                    a.push(d);
                } else {
                    result.insert(url, vec![d]);
                };
            }
            None => {
                unreachable!();
            }
        }
    }
    // update version.
    for (k, v) in result.iter() {
        context.diag_version.update(&mani, k, v.len());
    }
    context.diag_version.with_manifest(&mani, |x| {
        for (old, v) in x.iter() {
            if result.contains_key(&old) == false && *v > 0 {
                result.insert(old.clone(), vec![]);
            }
        }
    });
    for (k, x) in result.iter() {
        if x.len() == 0 {
            context.diag_version.update(&mani, k, 0);
        }
    }
    for (k, v) in result.into_iter() {
        let ds = lsp_types::PublishDiagnosticsParams::new(k.clone(), v, None);
        context
            .connection
            .sender
            .send(lsp_server::Message::Notification(Notification {
                method: format!("{}", lsp_types::notification::PublishDiagnostics::METHOD),
                params: serde_json::to_value(ds).unwrap(),
            }))
            .unwrap();
    }
}
