use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

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
