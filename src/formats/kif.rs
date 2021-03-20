use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    io::{self, prelude::*, BufReader, Read},
};

type KifMetadata = HashMap<String, String>;
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Kif {
    metadata: KifMetadata,
    moves: Vec<KifMoves>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct KifMoves {
    move_: usize,
}

impl Kif {
    pub fn from_read(r: impl Read) -> Result<Self, io::Error> {
        let mut kif = Kif::default();

        let mut reader = BufReader::new(r);
        let mut line = String::new();
        while reader.read_line(&mut line).is_ok() {
            line = line.trim().into();
            if line.starts_with('#') {
                continue;
            }
        }

        Ok(kif)
    }
}

impl Default for Kif {
    fn default() -> Self {
        Kif {
            metadata: HashMap::new(),
            moves: vec![],
        }
    }
}
