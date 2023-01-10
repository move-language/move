// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

// TODO the memory profiling not working,figure it out.
// Sometimes I want run profiling on my local machine.
#![allow(dead_code)]
use anyhow::Result;
use clap::Parser;
use crossbeam::channel::{bounded, select};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    notification::Notification as _, request::Request as _, CompletionOptions, Diagnostic,
    HoverProviderCapability, OneOf, SaveOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TypeDefinitionProviderCapability, WorkDoneProgressOptions,
};
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use log::{Level, Metadata, Record};
use move_analyzer::{
    completion::on_completion_request, context::Context, goto_definition, hover, modules::Modules,
    references, utils::*,
};
use move_symbol_pool::Symbol;
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
        modules: Modules::new(std::env::current_dir().unwrap()),
        connection,
        ref_caches: Default::default(),
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
                change: Some(TextDocumentSyncKind::Full),
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
            resolve_provider: Some(true),
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
        }),
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
        references_provider: Some(OneOf::Left(true)),
        // document_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .expect("could not serialize server capabilities");

    let (_diag_sender, diag_receiver) = bounded::<Result<BTreeMap<Symbol, Vec<Diagnostic>>>>(0);

    context
        .connection
        .initialize_finish(
            id,
            serde_json::json!({
                "capabilities": capabilities,
            }),
        )
        .expect("could not finish connection initialization");

    loop {
        select! {
            recv(diag_receiver) -> message => {
                match message {
                    Ok(result) => {
                        match result {
                            Ok(diags) => {
                                for (k, v) in diags {
                                    let url = Url::from_file_path(Path::new(&k.to_string())).unwrap();
                                    let params = lsp_types::PublishDiagnosticsParams::new(url, v, None);
                                    let notification = Notification::new(lsp_types::notification::PublishDiagnostics::METHOD.to_string(), params);
                                    if let Err(err) = context
                                        .connection
                                        .sender
                                        .send(lsp_server::Message::Notification(notification)) {
                                            log::error!("could not send diagnostics response: {:?}", err);
                                        };
                                }
                            },
                            Err(err) => {
                                let typ = lsp_types::MessageType::Error;
                                let message = format!("{err}");
                                    // report missing manifest only once to avoid re-generating
                                    // user-visible error in cases when the developer decides to
                                    // keep editing a file that does not belong to a packages
                                    let params = lsp_types::ShowMessageParams { typ, message };
                                let notification = Notification::new(lsp_types::notification::ShowMessage::METHOD.to_string(), params);
                                if let Err(err) = context
                                    .connection
                                    .sender
                                    .send(lsp_server::Message::Notification(notification)) {
                                        log::error!("could not send compiler error response: {:?}", err);
                                    };
                            },
                        }
                    },
                    Err(error) => log::error!("symbolicator message error: {:?}", error),
                }
            },
            recv(context.connection.receiver) -> message => {
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
                            _ => on_notification(&mut context,   &notification),
                        }
                    }
                    Err(error) => log::error!("IDE message error: {:?}", error),
                }
            }
        };
    }
    io_threads.join().expect("I/O threads could not finish");
    log::error!("Shut down language server '{}'.", exe);
}

fn on_request(context: &mut Context, request: &Request) {
    log::info!("receive method:{}", request.method.as_str());
    match request.method.as_str() {
        lsp_types::request::Completion::METHOD => on_completion_request(context, request),
        lsp_types::request::GotoDefinition::METHOD => {
            goto_definition::on_go_to_def_request(context, request);
        }
        lsp_types::request::GotoTypeDefinition::METHOD => {
            goto_definition::on_go_to_def_request(context, request);
        }

        lsp_types::request::References::METHOD => {
            references::on_references_request(context, request);
        }
        lsp_types::request::HoverRequest::METHOD => {
            hover::on_hover_request(context, request);
        }

        // lsp_types::request::DocumentSymbolRequest::METHOD => {
        //     symbols::on_document_symbol_request(context, request, &context.symbols.lock().unwrap());
        // }
        _ => log::error!("handle request '{}' from client", request.method),
    }
}

fn on_response(_context: &Context, _response: &Response) {
    log::error!("handle response from client");
}

fn on_notification(context: &mut Context, notification: &Notification) {
    match notification.method.as_str() {
        lsp_types::notification::DidSaveTextDocument::METHOD => {
            use lsp_types::DidSaveTextDocumentParams;
            let parameters =
                serde_json::from_value::<DidSaveTextDocumentParams>(notification.params.clone())
                    .expect("could not deserialize go-to-def request");
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
            context.modules.update_defs(&fpath, content.as_str());
            context.ref_caches.clear();
        }
        lsp_types::notification::DidChangeTextDocument::METHOD => {
            use lsp_types::DidChangeTextDocumentParams;
            let parameters =
                serde_json::from_value::<DidChangeTextDocumentParams>(notification.params.clone())
                    .expect("could not deserialize go-to-def request");
            let fpath = parameters.text_document.uri.to_file_path().unwrap();
            let fpath = path_concat(&PathBuf::from(std::env::current_dir().unwrap()), &fpath);
            context.modules.update_defs(
                &fpath,
                parameters.content_changes.last().unwrap().text.as_str(),
            );
            context.ref_caches.clear();
        }

        lsp_types::notification::DidOpenTextDocument::METHOD
        | lsp_types::notification::DidCloseTextDocument::METHOD => {
            log::error!("handle notification '{}' from client", notification.method);
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
