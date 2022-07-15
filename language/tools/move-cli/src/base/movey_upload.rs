// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::utils::movey_credential;
use anyhow::bail;
use clap::*;
use move_command_line_common::{env::MOVE_HOME, movey};
use move_package::BuildConfig;
use reqwest::blocking::Client;
use std::{fs, process::Command};

#[derive(serde::Serialize, Default)]
pub struct MoveyUploadRequest {
    github_repo_url: String,
    total_files: usize,
    token: String,
    subdir: String,
}

/// Upload the package to Movey.net.
#[derive(Parser)]
#[clap(name = "movey-upload")]
pub struct MoveyUpload {
    #[clap(long = "test-token")]
    test_token: Option<String>,
}

impl MoveyUpload {
    pub fn execute(self, config: BuildConfig) -> anyhow::Result<()> {
        let mut movey_upload_request: MoveyUploadRequest = Default::default();
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
                    movey_upload_request.github_repo_url = https_url;
                    break;
                }
                movey_upload_request.github_repo_url = String::from(tokens[1]);
                break;
            }
        }

        output = Command::new("git")
            .current_dir(".")
            .args(&["rev-parse", "--show-prefix"])
            .output()
            .unwrap();
        let subdir = String::from_utf8_lossy(output.stdout.as_slice());
        movey_upload_request.subdir = String::from(subdir);

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
        movey_upload_request.total_files = total_files;
        movey_upload_request.token = if config.test_mode {
            // if running in test mode, get token from CLI option or hardcoded fallback instead of reading from movey_api_key.toml
            // to separate MoveyUpload::execute logic from others
            if let Some(token) = self.test_token {
                token
            } else {
                "test-token".to_string()
            }
        } else {
            movey_credential::get_registry_api_token(&MOVE_HOME.clone())?
        };

        if config.test_mode {
            fs::write(
                "./request-body.txt",
                serde_json::to_string(&movey_upload_request).expect("invalid request body"),
            )
            .expect("unable to write file");
        } else {
            let client = Client::new();
            let response = client
                .post(&format!("{}/api/v1/post_package/", movey::MOVEY_URL))
                .json(&movey_upload_request)
                .send();
            match response {
                Ok(response) => {
                    if response.status().is_success() {
                        println!("Your package has been successfully uploaded to Movey")
                    } else if response.status().is_client_error() {
                        println!("Error: {}", response.text()?)
                    } else if response.status().is_server_error() {
                        println!("Error: An unexpected error occurred. Please try again later");
                    }
                }
                Err(_) => {
                    println!("Error: An unexpected error occurred. Please try again later");
                }
            }
        }
        Ok(())
    }
}
