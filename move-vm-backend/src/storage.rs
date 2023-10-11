use alloc::vec::Vec;

/// Trait for a storage engine. This is used by the Move VM to store data. Used for
/// mapping Substrate storage which is typical key-value container.
pub trait Storage {
    /// Returns the data for specified `key`.
    /// `None` if the key cannot be obtained.
    fn get(&self, key: &[u8]) -> Option<Vec<u8>>;

    /// Set a `value` for the specified `key` in the storage.
    /// If the key is non-existent, a new key-value pair is inserted.
    // TODO: We could use Vec<u8> for value here to avoid one clone operation. We don't usually use
    // the value after we update it in storage.
    fn set(&self, key: &[u8], value: &[u8]);

    /// Remove `key` and its value from the storage.
    fn remove(&self, key: &[u8]);
}
