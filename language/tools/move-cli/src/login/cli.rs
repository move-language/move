use std::{fs, io};
use std::fs::File;
use std::path::PathBuf;
use anyhow::{bail, Result};
use toml_edit::easy::Value;
use toml_edit::easy::map::Map;

pub fn handle_login_commands(config: move_package::BuildConfig) -> Result<()> {
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
            bail!("Error reading file: {}", err);
        }
    }
    save_credential(line, config.test_mode)
}

pub fn save_credential(token: String, is_test_mode: bool) -> Result<()> {
    let mut move_home = std::env::var("MOVE_HOME").unwrap_or_else(|_| {
        format!(
            "{}/.move",
            std::env::var("HOME").expect("env var 'HOME' must be set")
        )
    });
    if is_test_mode {
        move_home.push_str("/test")
    }
    fs::create_dir_all(&move_home)?;
    let credential_path = move_home + "/credential.toml";
    let credential_file = PathBuf::from(&credential_path.clone());
    if !credential_file.exists() {
        File::create(&credential_path)?;
    }

    let old_contents: String;
    match fs::read_to_string(&credential_path) {
        Ok(contents) => {
            old_contents = contents;
        },
        Err(error) => bail!("Error reading input: {}", error),
    }
    let mut toml: Value = old_contents.parse()
        .map_err(|e| anyhow::Error::from(e).context("could not parse input as TOML"))?;

    if let Some(registry) = toml.as_table_mut().unwrap().get_mut("registry") {
        if let Some(toml_token) = registry.as_table_mut().unwrap().get_mut("token") {
            *toml_token = Value::String(token);
        } else {
            registry.as_table_mut().unwrap()
                .insert(String::from("token"), Value::String(token));
        }
    } else {
        let mut value = Map::new();
        value.insert(String::from("token"), Value::String(token));
        toml.as_table_mut().unwrap().insert(String::from("registry"), Value::Table(value));
    }

    let new_contents = toml.to_string();
    fs::write(credential_file, new_contents).expect("Unable to write file");
    let file = File::open(&credential_path)?;
    set_permissions(&file, 0o600)?;
    Ok(())
}

#[cfg(unix)]
fn set_permissions(file: &File, mode: u32) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;

    let mut perms = file.metadata()?.permissions();
    perms.set_mode(mode);
    file.set_permissions(perms)?;
    Ok(())
}

#[cfg(not(unix))]
#[allow(unused)]
fn set_permissions(file: &File, mode: u32) -> Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::env;
    use home::home_dir;
    use super::*;

    fn setup_home_path() -> String {
        let move_home = env::var("MOVE_HOME").unwrap_or_else(|_| {
            env::var("HOME").unwrap_or_else(|_| {
                let home_dir = home_dir().unwrap().to_string_lossy().to_string();
                env::set_var("HOME", &home_dir);
                home_dir
            })
        });
        let credential_file = move_home + "/.move/test/credential.toml";
        return credential_file;
    }

    #[test]
    fn save_credential_works_if_no_credential_file_exists() {
        let credential_file = setup_home_path();
        let _ = fs::remove_file(&credential_file);

        save_credential(String::from("test_token"), true).unwrap();

        let contents = fs::read_to_string(&credential_file).expect("Unable to read file");
        let mut toml: Value = contents.parse().unwrap();
        let registry = toml.as_table_mut().unwrap().get_mut("registry").unwrap();
        let token = registry.as_table_mut().unwrap().get_mut("token").unwrap();
        assert!(token.to_string().contains("test_token"));
    }

    #[test]
    fn save_credential_works_if_credential_file_is_empty() {
        let credential_file = setup_home_path();

        let _ = fs::remove_file(&credential_file);
        File::create(&credential_file).unwrap();

        save_credential(String::from("test_token"), true).unwrap();

        let contents = fs::read_to_string(&credential_file).expect("Unable to read file");
        let mut toml: Value = contents.parse().unwrap();
        let registry = toml.as_table_mut().unwrap().get_mut("registry").unwrap();
        let token = registry.as_table_mut().unwrap().get_mut("token").unwrap();
        assert!(token.to_string().contains("test_token"));
    }

    #[test]
    fn save_credential_works_with_old_token_field() {
        let credential_file = setup_home_path();
        File::create(&credential_file).unwrap();

        let old_content = String::from("[registry]\ntoken = \"old_test_token\"\n");
        fs::write(&credential_file, old_content).expect("Unable to write file");

        save_credential(String::from("test_token"), true).unwrap();

        let contents = fs::read_to_string(&credential_file).expect("Unable to read file");
        let mut toml: Value = contents.parse().unwrap();
        let registry = toml.as_table_mut().unwrap().get_mut("registry").unwrap();
        let token = registry.as_table_mut().unwrap().get_mut("token").unwrap();
        assert!(token.to_string().contains("test_token"));
        assert!(!token.to_string().contains("old_test_token"));
    }

    #[test]
    fn save_credential_works_with_old_empty_token_field() {
        let credential_file = setup_home_path();
        File::create(&credential_file).unwrap();

        let old_content = String::from("[registry]\ntoken = \"\"\n");
        fs::write(&credential_file, old_content).expect("Unable to write file");

        save_credential(String::from("test_token"), true).unwrap();

        let contents = fs::read_to_string(&credential_file).expect("Unable to read file");
        let mut toml: Value = contents.parse().unwrap();
        let registry = toml.as_table_mut().unwrap().get_mut("registry").unwrap();
        let token = registry.as_table_mut().unwrap().get_mut("token").unwrap();
        assert!(token.to_string().contains("test_token"));
        assert!(!token.to_string().contains("old_test_token"));
    }
}