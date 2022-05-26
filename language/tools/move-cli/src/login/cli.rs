use std::collections::HashMap;
use std::io;
use std::fs::File;
use std::path::PathBuf;
use anyhow::{bail, Result};
use toml_edit::easy::{Value};
use toml_edit::easy::map::Map;

pub fn handle_login_commands() -> Result<()> {
    let url: &str;
    if cfg!(debug_assertions) {
        url = "https://movey-app-staging.herokuapp.com";
    } else {
        url = "https://movey.net";
    }
    println!("please paste the API Token found on {}/settings/tokens below", url);
    let mut line = String::new();
    match io::stdin().read_line(&mut line) {
        Ok(_) => {
            if let Some('\n') = line.chars().next_back() {
                line.pop();
            }
            if let Some('\r') = line.chars().next_back() {
                line.pop();
            }
            print!("{}", line);
        }
        Err(err) => {
            bail!("Error reading input: {}", err);
        }
    }
    save_credential(line)
}

fn save_credential(token: String) -> Result<()> {
    let move_home = std::env::var("MOVE_HOME").unwrap_or_else(|_| {
        format!(
            "{}/.move",
            std::env::var("HOME").expect("env var 'HOME' must be set")
        )
    });
    let credential_path = move_home + "/credential.json";
    let credential_file = PathBuf::from(credential_path.clone());
    if !credential_file.exists() {
        File::create(credential_path)?;
    }
    let mut value = Map::new();
    value.insert(String::from("token"), Value::String(token));
    let mut toml = Value::Table(Map::new());
    toml.as_table_mut().unwrap().insert(String::from("registry"), Value::Table(value));

    let contents = toml.to_string();
    println!("{}", contents);
    Ok(())
}
