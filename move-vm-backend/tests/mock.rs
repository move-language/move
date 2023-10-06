use std::cell::RefCell;
use std::collections::HashMap;

use move_vm_backend::storage::Storage;
use move_vm_backend::warehouse::AccountData;

// Mock storage implementation for testing
#[derive(Clone, Debug)]
pub struct StorageMock {
    pub data: RefCell<HashMap<Vec<u8>, AccountData>>,
}

impl StorageMock {
    pub fn new() -> StorageMock {
        StorageMock {
            data: RefCell::new(Default::default()),
        }
    }
}

impl Default for StorageMock {
    fn default() -> Self {
        StorageMock::new()
    }
}

impl Storage for StorageMock {
    fn get(&self, key: &[u8]) -> Option<AccountData> {
        let data = self.data.borrow();
        data.get(key).map(|blob| blob.clone())
    }

    fn set(&self, key: &[u8], value: &AccountData) {
        let mut data = self.data.borrow_mut();
        data.insert(key.to_owned(), value.clone());
    }

    fn remove(&self, key: &[u8]) {
        let mut data = self.data.borrow_mut();
        data.remove(key);
    }
}
