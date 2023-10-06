use crate::warehouse::AccountData;

/// Trait for a storage engine. This is used by the Move VM to store data. Used for
/// mapping Substrate storage which is typical key-value container.
pub trait Storage {
    /// Returns the data for specified `key`.
    /// `None` if the key cannot be obtained.
    fn get(&self, key: &[u8]) -> Option<AccountData>;

    /// Set a `value` for the specified `key` in the storage.
    /// If the key is non-existent, a new key-value pair is inserted.
    fn set(&self, key: &[u8], value: &AccountData);

    /// Remove `key` and its value from the storage.
    fn remove(&self, key: &[u8]);
}
