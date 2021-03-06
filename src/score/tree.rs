use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, time::Duration};

use super::branch::Branch;

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct TreeMetadata {
    start_time: Option<NaiveDateTime>,
    end_time: Option<NaiveDateTime>,

    // location: Option<String>,
    // event: Option<String>,
    comment: Option<String>,

    time_limit: Option<Duration>,
    time_limit_every: Option<Duration>,
    extra_metadata: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct Tree {
    metadata: TreeMetadata,
    branches: Vec<Branch>,
}
