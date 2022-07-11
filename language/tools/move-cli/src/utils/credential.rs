// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use anyhow::{bail, Context, Result};
use move_package::source_package::manifest_parser::MOVE_HOME;
use std::fs;
use toml_edit::easy::Value;

pub const MOVEY_API_KEY_PATH: &str = "/movey_api_key.toml";

#[derive(Clone)]
pub struct TestMode {
    pub test_path: String,
}

pub fn get_move_home_path(test_mode: Option<TestMode>) -> String {
    let mut move_home;
    if let Some(test_mode) = test_mode {
        move_home = std::env::var("TEST_MOVE_HOME").unwrap();
        if !test_mode.test_path.is_empty() {
            move_home.push_str(&test_mode.test_path);
        }
    } else {
        move_home = MOVE_HOME.clone();
    }
    move_home
}

pub fn get_credential_path(test_mode: Option<TestMode>) -> String {
    get_move_home_path(test_mode) + MOVEY_API_KEY_PATH
}

pub fn get_registry_api_token(test_mode: Option<TestMode>) -> Result<String> {
    if let Ok(content) = get_api_token(test_mode) {
        Ok(content)
    } else {
        bail!(
            "There seems to be an error with your Movey credential. \
            Please run `move login` and follow the instructions."
        )
    }
}

fn get_api_token(test_mode: Option<TestMode>) -> Result<String> {
    let credential_path = get_credential_path(test_mode);

    let contents = fs::read_to_string(&credential_path)?;
    let mut toml: Value = contents.parse()?;
    let registry = toml
        .as_table_mut()
        .context(format!("Error parsing {}", MOVEY_API_KEY_PATH))?
        .get_mut("registry")
        .context(format!("Error parsing {}", MOVEY_API_KEY_PATH))?;
    let token = registry
        .as_table_mut()
        .context("Error parsing token")?
        .get_mut("token")
        .context("Error parsing token")?;
    Ok(token.to_string().replace("\"", ""))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{env, fs::File};

    fn setup_move_home(test_path: &str) -> (String, String) {
        let cwd = env::current_dir().unwrap();
        let mut move_home: String = String::from(cwd.to_string_lossy());
        env::set_var("TEST_MOVE_HOME", &move_home);

        if !test_path.is_empty() {
            move_home.push_str(&test_path);
        }
        let credential_path = move_home.clone() + MOVEY_API_KEY_PATH;
        (move_home, credential_path)
    }

    fn clean_up(move_home: &str) {
        let _ = fs::remove_dir_all(move_home);
    }

    #[test]
    fn get_api_token_works() {
        let test_path = String::from("/get_api_token_works");
        let (move_home, credential_path) = setup_move_home(&test_path);

        let _ = fs::create_dir_all(&move_home);
        File::create(&credential_path).unwrap();

        let content = "[registry]\ntoken = \"a sample token\"";
        fs::write(&credential_path, content).unwrap();

        let test_mode = TestMode { test_path };
        let token = get_api_token(Some(test_mode)).unwrap();
        assert!(token.contains("a sample token"));

        clean_up(&move_home)
    }

    #[test]
    fn get_api_token_fails_if_there_is_no_move_home_directory() {
        let test_path = String::from("/get_api_token_fails_if_there_is_no_move_home_directory");
        let (move_home, _) = setup_move_home(&test_path);
        let _ = fs::remove_dir_all(&move_home);
        let test_mode = TestMode { test_path };
        let token = get_api_token(Some(test_mode));
        assert!(token.is_err());

        clean_up(&move_home)
    }

    #[test]
    fn get_api_token_fails_if_there_is_no_credential_file() {
        let test_path = String::from("/get_api_token_fails_if_there_is_no_credential_file");
        let (move_home, _) = setup_move_home(&test_path);
        let _ = fs::remove_dir_all(&move_home);
        fs::create_dir_all(&move_home).unwrap();
        let test_mode = TestMode { test_path };
        let token = get_api_token(Some(test_mode));
        assert!(token.is_err());

        clean_up(&move_home)
    }

    #[test]
    fn get_api_token_fails_if_credential_file_is_in_wrong_format() {
        let test_path = String::from("/get_api_token_fails_if_credential_file_is_in_wrong_format");
        let (move_home, credential_path) = setup_move_home(&test_path);

        let _ = fs::remove_dir_all(&move_home);
        fs::create_dir_all(&move_home).unwrap();
        File::create(&credential_path).unwrap();

        let content = "[registry]\ntoken = a sample token";
        fs::write(&credential_path, content).unwrap();

        let test_mode = TestMode { test_path };
        let token = get_api_token(Some(test_mode.clone()));
        assert!(token.is_err());

        let content = "[registry]\ntokens = \"a sample token\"";
        fs::write(&credential_path, content).unwrap();

        let token = get_api_token(Some(test_mode));
        assert!(token.is_err());

        clean_up(&move_home)
    }
}
