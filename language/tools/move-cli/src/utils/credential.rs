use std::fs;
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

pub fn get_registry_api_token(is_test_mode: bool) -> String {
    let move_home = get_move_home_path(is_test_mode);
    let credential_path = move_home.clone() + "/credential.toml";
    let contents = fs::read_to_string(&credential_path).expect("Unable to read credential.toml");
    let mut toml: Value = contents.parse().expect("Unable to parse credential.toml");
    let registry = toml
        .as_table_mut()
        .unwrap()
        .get_mut("registry")
        .expect("Unable to get [registry] table in credential.toml");
    let token = registry
        .as_table_mut()
        .unwrap()
        .get_mut("token")
        .expect("Unable to get token");
    token.to_string().replace("\"", "")
}
