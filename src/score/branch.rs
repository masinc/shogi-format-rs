use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub struct BranchMetadata {
    comment: Option<String>,
    consumed_time: Option<Duration>,
    total_consumed_time: Option<Duration>,
}


#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub struct Branch {
    metadata: BranchMetadata,
    branches: Vec<Branch>,
}
