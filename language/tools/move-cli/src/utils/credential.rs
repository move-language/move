use std::fs;
use anyhow::{Result, Context, bail};
use toml_edit::easy::{Value};

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
    if let Ok(content) = get_api_token(is_test_mode) {
        Ok(content)
    } else {
        bail!("There seems to be an error with your Movey credential. \
            Please run `move login` and follow the instructions.")
    }
}

fn get_api_token(is_test_mode: bool) -> Result<String> {
    let credential_path = get_credential_path(is_test_mode);
    
    let contents = fs::read_to_string(&credential_path)?;
    let mut toml: Value = contents.parse()?;
    let registry = toml.as_table_mut()
        .context("Error parsing credential.toml")?
        .get_mut("registry")
        .context("Error parsing credential.toml")?;
    let token = registry.as_table_mut()
        .context("Error parsing token")?
        .get_mut("token")
        .context("Error parsing token")?;
    Ok(token.to_string().replace("\"", ""))
}

#[cfg(test)]
mod tests {
    use super::*;
    use home::home_dir;
    use std::env;
    use std::fs::File;

    fn setup_move_home() -> (String, String) {
        let mut move_home = env::var("MOVE_HOME").unwrap_or_else(|_| {
            env::var("HOME").unwrap_or_else(|_| {
                let home_dir = home_dir().unwrap().to_string_lossy().to_string();
                env::set_var("HOME", &home_dir);
                home_dir
            })
        });
        move_home.push_str("/.move/test");
        let credential_path = move_home.clone() + "/credential.toml";

        return (move_home, credential_path);
    }

    fn clean_up() {
        let (move_home, _) = setup_move_home();
        let _ = fs::remove_dir_all(move_home);
    }

    #[test]
    fn get_api_token_works() {
        let (move_home, credential_path) = setup_move_home();
        
        let _ = fs::create_dir_all(&move_home);
        File::create(&credential_path).unwrap();

        let content = "[registry]\ntoken = \"a sample token\"";
        fs::write(&credential_path, content).unwrap();
        
        let token = get_api_token(true).unwrap();
        assert!(token.contains("a sample token"));

        clean_up()
    }

    #[test]
    fn get_api_token_fails_if_there_is_no_move_home_directory() {
        let (move_home, _) = setup_move_home();
        let _ = fs::remove_dir_all(&move_home);
        let token = get_api_token(true);
        assert!(token.is_err());

        clean_up()
    }

    #[test]
    fn get_api_token_fails_if_there_is_no_credential_file() {
        let (move_home, _) = setup_move_home();
        let _ = fs::remove_dir_all(&move_home);
        fs::create_dir_all(&move_home).unwrap();
        let token = get_api_token(true);
        assert!(token.is_err());

        clean_up()
    }

    #[test]
    fn get_api_token_fails_if_credential_file_is_in_wrong_format() {
        let (move_home, credential_path) = setup_move_home();

        let _ = fs::remove_dir_all(&move_home);
        fs::create_dir_all(&move_home).unwrap();
        File::create(&credential_path).unwrap();

        let content = "[registry]\ntoken = a sample token";
        fs::write(&credential_path, content).unwrap();

        let token = get_api_token(true);
        assert!(token.is_err());

        let content = "[registry]\ntokens = \"a sample token\"";
        fs::write(&credential_path, content).unwrap();

        let token = get_api_token(true);
        assert!(token.is_err());

        clean_up()
    }
}
