use std::fs;
use anyhow::{Result, Context, bail};
use toml_edit::easy::Value;

pub fn get_move_home_path(is_test_mode: bool) -> String {
    let mut home = std::env::var("MOVE_HOME").unwrap_or_else(|_| {
        format!(
            "{}/.move",
            std::env::var("HOME").expect("env var 'HOME' must be set")
        )
    });
    if is_test_mode && !home.contains("/test") {
        home.push_str("/test");
    }
    home
}

pub fn get_credential_path(is_test_mode: bool) -> String {
    get_move_home_path(is_test_mode) + "/credential.toml"
}

pub fn get_registry_api_token(is_test_mode: bool) -> Result<String> {
    match get_api_token(is_test_mode) {
        Ok(content) => Ok(content),
        Err(_) => bail!("There seems to be an error with your Movey credential. \
            Please run `move login` and follow the instructions.")
    }
}

fn get_api_token(is_test_mode: bool) -> Result<String> {
    let move_home = get_move_home_path(is_test_mode);
    let credential_path = move_home.clone() + "/credential.toml";
    
    let contents = fs::read_to_string(&credential_path)?;
    let mut toml: Value = contents.parse()?;
    let registry = toml.as_table_mut()
        .context("None")?
        .get_mut("registry")
        .context("None")?;
    let token = registry.as_table_mut().context("None")?
        .get_mut("token")
        .context("None")?;
    Ok(token.to_string().replace("\"", ""))
}
