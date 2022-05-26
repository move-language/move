// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::utils::get_loc;
use codespan_reporting::{diagnostic::Severity, files::SimpleFiles};
use lsp_types::{Diagnostic, DiagnosticSeverity, Range};
use move_command_line_common::files::FileHash;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::collections::{BTreeMap, HashMap};

fn severity(s: Severity) -> DiagnosticSeverity {
    match s {
        Severity::Bug => DiagnosticSeverity::Error,
        Severity::Error => DiagnosticSeverity::Error,
        Severity::Warning => DiagnosticSeverity::Warning,
        Severity::Note => DiagnosticSeverity::Information,
        Severity::Help => DiagnosticSeverity::Hint,
    }
}

pub fn lsp_diagnostics(
    diagnostics: &Vec<(
        codespan_reporting::diagnostic::Severity,
        &'static str,
        (Loc, String),
        Vec<(Loc, String)>,
        Vec<String>,
    )>,
    files: &SimpleFiles<Symbol, String>,
    file_id_mapping: &HashMap<FileHash, usize>,
    file_name_mapping: &BTreeMap<FileHash, Symbol>,
) -> BTreeMap<Symbol, Vec<Diagnostic>> {
    let mut lsp_diagnostics = BTreeMap::new();
    for (s, _, (loc, msg), _, _) in diagnostics {
        let fpath = file_name_mapping.get(&loc.file_hash()).unwrap();
        if let Some(start) = get_loc(&loc.file_hash(), loc.start(), files, file_id_mapping) {
            if let Some(end) = get_loc(&loc.file_hash(), loc.end(), files, file_id_mapping) {
                let range = Range::new(start, end);
                lsp_diagnostics
                    .entry(*fpath)
                    .or_insert_with(Vec::new)
                    .push(Diagnostic::new(
                        range,
                        Some(severity(*s)),
                        None,
                        None,
                        msg.to_string(),
                        None,
                        None,
                    ));
            }
        }
    }
    lsp_diagnostics
}

pub fn lsp_empty_diagnostics(
    file_name_mapping: &BTreeMap<FileHash, Symbol>,
) -> BTreeMap<Symbol, Vec<Diagnostic>> {
    let mut lsp_diagnostics = BTreeMap::new();
    for n in file_name_mapping.values() {
        lsp_diagnostics.insert(*n, vec![]);
    }
    lsp_diagnostics
}
