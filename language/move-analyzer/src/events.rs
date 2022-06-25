// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Ord, PartialOrd, PartialEq, Eq, Serialize, Deserialize)]
pub enum AnalyzerEventType {
    SymbolicatorEvent,
}

#[derive(Debug, Clone, Ord, PartialOrd, PartialEq, Eq, Serialize, Deserialize)]
pub struct AnalyzerEvent {
    /// The event type
    pub event_type: AnalyzerEventType,
    pub event_data: BTreeMap<String, String>,
}
