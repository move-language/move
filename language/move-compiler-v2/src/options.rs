// Copyright © Aptos Foundation
// Parts of the project are originally copyright © Meta Platforms, Inc.
// SPDX-License-Identifier: Apache-2.0

use crate::{experiments, DefaultValue, EXPERIMENTS};
use clap::Parser;
use codespan_reporting::diagnostic::Severity;
use itertools::Itertools;
use move_command_line_common::env::read_env_var;
use move_compiler::command_line as cli;
use once_cell::sync::Lazy;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
};

/// Defines options for a run of the compiler.
#[derive(Parser, Clone, Debug)]
#[clap(author, version, about)]
pub struct Options {
    /// Directories where to lookup dependencies.
    #[clap(
        short,
        num_args = 0..
    )]
    pub dependencies: Vec<String>,
    /// Named address mapping.
    #[clap(
        short,
        num_args = 0..
    )]
    pub named_address_mapping: Vec<String>,
    /// Output directory.
    #[clap(short, long, default_value = "")]
    pub output_dir: String,
    /// Do not complain about unknown attributes in Move code.
    #[clap(long, default_value = "false")]
    pub skip_attribute_checks: bool,
    /// Known attributes for this dialect of move; if empty, assumes third-party Move.
    /// Only used if skip_attribute_checks is false.
    #[clap(skip)]
    pub known_attributes: BTreeSet<String>,
    /// Whether we generate code for tests. This specifically guarantees stable output
    /// for baseline testing.
    #[clap(long)]
    pub testing: bool,
    /// Active experiments. Experiments alter default behavior of the compiler.
    /// See `Experiment` struct.
    #[clap(short)]
    #[clap(
        long = "experiment",
        num_args = 0..
    )]
    pub experiments: Vec<String>,
    /// A transient cache for memoization of experiment checks.
    #[clap(skip)]
    pub experiment_cache: RefCell<BTreeMap<String, bool>>,
    /// Sources to compile (positional arg, therefore last)
    pub sources: Vec<String>,
    /// Show warnings about unused functions, fields, constants, etc.
    /// Note that the current value of this constant is "Wunused"
    #[clap(long = cli::WARN_UNUSED_FLAG, default_value="false")]
    pub warn_unused: bool,
    /// Whether to compile everything, including dependencies.
    #[clap(long)]
    pub whole_program: bool,
}

impl Default for Options {
    fn default() -> Self {
        Parser::parse_from(std::iter::empty::<String>())
    }
}

impl Options {
    /// Returns the least severity of diagnosis which shall be reported.
    /// This is currently hardwired.
    pub fn report_severity(&self) -> Severity {
        Severity::Warning
    }

    /// Turns an experiment on or off, overriding command line, environment, and defaults.
    pub fn set_experiment(self, name: impl AsRef<str>, on: bool) -> Self {
        let name = name.as_ref().to_string();
        assert!(
            experiments::EXPERIMENTS.contains_key(&name),
            "experiment `{}` not declared",
            name
        );
        self.experiment_cache.borrow_mut().insert(name, on);
        self
    }

    /// Returns true if an experiment is on.
    pub fn experiment_on(&self, name: &str) -> bool {
        self.experiment_on_recursive(name, &mut BTreeSet::new())
    }

    fn experiment_on_recursive(&self, name: &str, visited: &mut BTreeSet<String>) -> bool {
        if !visited.insert(name.to_string()) {
            panic!(
                "cyclic inheritance relation between experiments: `{} -> {}`",
                name,
                visited.iter().clone().join(",")
            )
        }
        if let Some(on) = self.experiment_cache.borrow().get(name).cloned() {
            return on;
        }
        if let Some(exp) = experiments::EXPERIMENTS.get(&name.to_string()) {
            // First we look at experiments provided via the command line, second
            // via the env var, and last we take the configured default.
            let on = if let Some(on) = find_experiment(&self.experiments, name) {
                on
            } else if let Some(on) = find_experiment(&compiler_exp_var(), name) {
                on
            } else {
                match &exp.default {
                    DefaultValue::Given(on) => *on,
                    DefaultValue::Inherited(other_name) => {
                        self.experiment_on_recursive(other_name, visited)
                    },
                }
            };
            self.experiment_cache
                .borrow_mut()
                .insert(name.to_string(), on);
            on
        } else {
            panic!("unknown experiment `{}`", name)
        }
    }
}

/// Finds the experiment in the list of definitions. A definition
/// can be either `<exp_name>` in which case it is on, or
/// `<exp_name>=on/off`.
fn find_experiment(s: &[String], name: &str) -> Option<bool> {
    let mut result = None;
    for e in s {
        let mut parts = e.split('=');
        let exp_name = parts.next().unwrap();
        assert!(
            EXPERIMENTS.contains_key(exp_name),
            "undeclared experiment `{}`",
            exp_name
        );
        if exp_name == name {
            let on = if let Some(value) = parts.next() {
                match value.trim().to_lowercase().as_str() {
                    "on" => true,
                    "off" => false,
                    _ => panic!(
                        "invalid value for experiment `{}`: \
                        found `{}` but must be `on` or `off`",
                        name, value
                    ),
                }
            } else {
                // Default is on
                true
            };
            result = Some(on);
            // continue going to (1) check all experiments
            // in the list are actually declared (2) later
            // entries override earlier ones
        }
    }
    result
}

/// Gets the value of the env var for experiments.
fn compiler_exp_var() -> Vec<String> {
    static EXP_VAR: Lazy<Vec<String>> = Lazy::new(|| {
        for s in ["MVC_EXP", "MOVE_COMPILER_EXP"] {
            let s = read_env_var(s);
            if !s.is_empty() {
                return s.split(',').map(|s| s.to_string()).collect();
            }
        }
        vec![]
    });
    (*EXP_VAR).clone()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_options_ref_cell_clone() {
        let test_key = "foo".to_owned();
        let options1 = Options::default();

        options1
            .experiment_cache
            .borrow_mut()
            .insert(test_key.clone(), false);

        let options2 = options1.clone();

        options1
            .experiment_cache
            .borrow_mut()
            .insert(test_key.clone(), true);

        let x1 = *(options1
            .experiment_cache
            .borrow()
            .get(&test_key)
            .expect("we just set foo"));
        assert!(x1);

        let x2 = *(options2
            .experiment_cache
            .borrow()
            .get(&test_key)
            .expect("we just set foo"));
        assert!(!x2);
    }
}
