// Copyright © Aptos Foundation
// SPDX-License-Identifier: Apache-2.0

//! This module contains a set of transformers and analyzers of global environment.
//! Those can be arranged in a pipeline and executed in sequence.

// TODO: move all other `&mut GlobalEnv` processors into this module.

use log::trace;
use move_model::model::GlobalEnv;
use std::io::Write;

pub mod lambda_lifter;
pub mod rewrite_target;
pub mod spec_checker;
pub mod spec_rewriter;

/// Represents a pipeline of processors working on the global environment.
#[derive(Default)]
pub struct EnvProcessorPipeline<'a> {
    /// A sequence of transformations to run on the model.
    /// For each processor, we store its name and the transformation function.
    processors: Vec<(String, Box<dyn Fn(&mut GlobalEnv) + 'a>)>,
}

impl<'a> EnvProcessorPipeline<'a> {
    /// Adds a processor to the pipeline.
    pub fn add<P>(&mut self, name: &str, processor: P)
    where
        P: Fn(&mut GlobalEnv) + 'a,
    {
        self.processors.push((name.to_owned(), Box::new(processor)))
    }

    /// Runs the pipeline. Running will be ended if any of the steps produces an error.
    /// The function returns true if all steps succeeded without errors.
    pub fn run(&self, env: &mut GlobalEnv) -> bool {
        trace!("before env processor pipeline: {}", env.dump_env());
        for (name, proc) in &self.processors {
            proc(env);
            trace!("after env processor {}", name);
            if env.has_errors() {
                return false;
            }
        }
        true
    }

    /// Runs the pipeline, recording results into the writer. This is intended for testing
    /// only.
    pub fn run_and_record(&self, env: &mut GlobalEnv, w: &mut impl Write) -> anyhow::Result<bool> {
        let msg = format!("before env processor pipeline:\n{}\n", env.dump_env());
        trace!("{}", msg);
        writeln!(w, "// -- Model dump {}", msg)?;
        for (name, proc) in &self.processors {
            proc(env);
            if !env.has_errors() {
                let msg = format!("after env processor {}:\n{}\n", name, env.dump_env());
                trace!("{}", msg);
                writeln!(w, "// -- Model dump {}", msg)?;
            } else {
                return Ok(false);
            }
        }
        Ok(true)
    }
}
