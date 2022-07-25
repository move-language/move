// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{env, fs::File, path::PathBuf, process::Command};

use anyhow::bail;
use clap::*;
use reqwest::blocking::Client;

use move_command_line_common::env::MOVE_HOME;

use crate::utils::movey_credential;

#[derive(serde::Serialize, Default)]
pub struct MoveyUploadRequest {
    github_repo_url: String,
    total_files: usize,
    token: String,
    subdir: String,
}

/// Upload the package metadata to Movey.net.
#[derive(Parser)]
#[clap(name = "movey-upload")]
pub struct MoveyUpload;

impl MoveyUpload {
    pub fn execute(self, path: Option<PathBuf>) -> anyhow::Result<()> {
        if let Some(path) = path {
            if path.exists() && path.is_dir() {
                let _ = env::set_current_dir(&path);
            } else {
                bail!("invalid directory")
            }
        }
        // make sure it's a Move project
        let move_toml = File::open("Move.toml");
        if move_toml.is_err() {
            bail!("Move.toml not found")
        }
        let metadata = move_toml.unwrap().metadata()?;
        if metadata.len() == 0 {
            bail!("Move.toml not found")
        }

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
        movey_upload_request.token = movey_credential::get_registry_api_token(&MOVE_HOME)?;
        let movey_url = movey_credential::get_movey_url(&MOVE_HOME);
        match movey_url {
            Ok(url) => {
                let client = Client::new();
                let response = client
                    .post(&format!("{}/api/v1/post_package/", &url))
                    .json(&movey_upload_request)
                    .send();
                match response {
                    Ok(response) => {
                        if response.status().is_success() {
                            println!("Your package has been successfully uploaded to Movey")
                        } else if response.status().is_client_error() {
                            bail!("Error: {}", response.text()?)
                        } else if response.status().is_server_error() {
                            bail!("Error: An unexpected error occurred. Please try again later");
                        }
                    }
                    Err(_) => {
                        bail!("Error: An unexpected error occurred. Please try again later");
                    }
                }
            }
            Err(_) => bail!("Error: An unexpected error occurred. Please try again later"),
        }
        Ok(())
    }
}
