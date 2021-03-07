use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, time::Duration};

use super::{board::BoardType, branch::Branch};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct TreeMetadata {
    initial_board: BoardType,

    start_time: Option<NaiveDateTime>,
    end_time: Option<NaiveDateTime>,

    // location: Option<String>,
    // event: Option<String>,
    comment: Option<String>,

    time_limit: Option<Duration>,
    time_limit_every: Option<Duration>,
    extra_metadata: Option<HashMap<String, String>>,
}

impl TreeMetadata {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for TreeMetadata {
    #[inline]
    fn default() -> Self {
        Self {
            initial_board: BoardType::Even,
            start_time: None,
            end_time: None,
            comment: None,
            time_limit: None,
            time_limit_every: None,
            extra_metadata: None,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct Tree {
    metadata: TreeMetadata,
    branches: Vec<Branch>,
}
