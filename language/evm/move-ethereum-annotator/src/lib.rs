// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

mod events;
mod functions;
mod util;

pub use events::MoveEventAnnotator;
pub use functions::{FunctionCallAnnotator, MoveCall};

const LRU_CACHE_SIZE: usize = 1_000;
