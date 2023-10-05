use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use move_vm_backend::storage::Storage;

// Mock storage implementation for testing
#[derive(Clone, Debug)]
pub struct StorageMock {
    pub data: RefCell<HashMap<Vec<u8>, Vec<u8>>>,
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
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        let data = self.data.borrow();
        data.get(key).map(|blob| blob.to_owned())
    }

    fn set(&self, key: &[u8], value: &[u8]) {
        let mut data = self.data.borrow_mut();
        data.insert(key.to_owned(), value.to_owned());
    }

    fn remove(&self, key: &[u8]) {
        let mut data = self.data.borrow_mut();
        data.remove(key);
    }
}
