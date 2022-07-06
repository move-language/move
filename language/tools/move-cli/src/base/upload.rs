// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::utils::{credential, credential::TestMode};
use anyhow::bail;
use clap::*;
use move_package::BuildConfig;
use reqwest::blocking::Client;
use std::{fs, path::PathBuf, process::Command};

#[derive(serde::Serialize, Default)]
pub struct UploadRequest {
    github_repo_url: String,
    rev: String,
    total_files: usize,
    token: String,
}

/// Upload the package to Movey.net.
#[derive(Parser)]
#[clap(name = "upload")]
pub struct Upload {
    #[clap(long = "test-path")]
    test_path: Option<String>,
}

impl Upload {
    pub fn execute(self, _path: Option<PathBuf>, config: BuildConfig) -> anyhow::Result<()> {
        let mut upload_request: UploadRequest = Default::default();
        let mut output = Command::new("git")
            .current_dir(".")
            .args(&["remote", "-v"])
            .output()
            .unwrap();
        if !output.status.success() || output.stdout.is_empty() {
            bail!("invalid git repository")
        }

        let lines = String::from_utf8_lossy(output.stdout.as_slice());
        let lines = lines.split("\n");
        for line in lines {
            if line.contains("github.com") {
                let tokens: Vec<&str> = line.split(&['\t', ' '][..]).collect();
                if tokens.len() != 3 {
                    bail!("invalid remote url")
                }
                // convert ssh url to https
                if tokens[1].starts_with("git@github.com") {
                    let https_url = tokens[1]
                        .replace(":", "/")
                        .replace("git@", "https://")
                        .replace(".git", "");
                    upload_request.github_repo_url = https_url;
                    break;
                }
                upload_request.github_repo_url = String::from(tokens[1]);
                break;
            }
        }

        output = Command::new("git")
            .current_dir(".")
            .args(&["rev-parse", "HEAD"])
            .output()
            .unwrap();
        if !output.status.success() {
            bail!("invalid HEAD commit id")
        }
        let revision_num = String::from_utf8_lossy(output.stdout.as_slice());
        upload_request.rev = String::from(revision_num.trim());

        output = Command::new("git")
            .current_dir(".")
            .args(&["ls-files"])
            .output()
            .unwrap();
        let tracked_files = String::from_utf8_lossy(output.stdout.as_slice());
        let tracked_files: Vec<&str> = tracked_files.split("\n").collect();
        let mut total_files = tracked_files.len();
        for file_path in tracked_files {
            if file_path.is_empty() {
                total_files -= 1;
                continue;
            }
        }
        upload_request.total_files = total_files;
        upload_request.token = if config.test_mode {
            let test = if let Some(path) = self.test_path {
                path
            } else {
                String::new()
            };
            let test_mode = TestMode { test_path: test };
            credential::get_registry_api_token(Some(test_mode))?
        } else {
            credential::get_registry_api_token(None)?
        };

        if config.test_mode {
            fs::write(
                "./request-body.txt",
                serde_json::to_string(&upload_request).expect("invalid request body"),
            )
            .expect("unable to write file");
        } else {
            let url: String;
            if cfg!(debug_assertions) {
                url = String::from("https://movey-app-staging.herokuapp.com");
            } else {
                url = String::from("https://www.movey.net");
            }
            let client = Client::new();
            let response = client
                .post(&format!("{}/api/v1/post_package/", url))
                .json(&upload_request)
                .send()
                .unwrap();
            if response.status().as_u16() == 200 {
                println!("Your package has been successfully uploaded to Movey")
            } else {
                println!(
                    "Upload failed. Please check your token (you can find it on {}) \
                        and try again.",
                    url
                );
            }
        }
        Ok(())
    }
}
