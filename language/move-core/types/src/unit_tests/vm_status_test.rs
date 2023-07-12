// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::vm_status::{StatusCode, VMStatus};

#[test]
fn test_stats_code() {
    let vm_status = VMStatus::error(StatusCode::OUT_OF_GAS, None);
    let code = vm_status.status_code();
    assert_eq!(code, StatusCode::OUT_OF_GAS);
}
