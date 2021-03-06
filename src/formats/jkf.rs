use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// JKF(Json Kif Format)
/// see: https://github.com/na2hiro/json-kifu-format

type Number = usize;

type JkfHeader = HashMap<String, String>;
type JkfBoard = Vec<Vec<JkfPiece>>;
type JkfHands = HashMap<String, usize>;
type JkfFork = Vec<Vec<JkfMoveFormat>>;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Jkf {
    header: JkfHeader,
    initial: Option<JkfInitial>,
    moves: Vec<JkfMoveFormat>,
    forks: Option<JkfFork>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JkfInitial {
    preset: String,
    data: Option<JkfStateFormat>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JkfStateFormat {
    color: JkfColor,
    board: JkfBoard,
    hands: JkfHands,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JkfPiece {
    color: Option<JkfColor>,
    kind: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum JkfColor {
    Black,
    White,
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JkfMoveFormat {
    comments: Option<String>,
    #[serde(rename = "move")]
    move_to: Option<String>,
    time: Option<JkfMoveTimeFormat>,
    special: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JkfMoveMoveFormat {
    color: JkfColor,
    from: Option<JkfPieceFormat>,
    to: Option<JkfPieceFormat>,
    piece: String,
    same: Option<bool>,
    promote: Option<bool>,
    capture: Option<String>,
    reactive: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JkfPieceFormat {
    x: Number,
    y: Number,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JkfTimeFormat {
    #[serde(rename = "h")]
    hear: Option<Number>,
    #[serde(rename = "m")]
    minute: Option<Number>,
    #[serde(rename = "s")]
    second: Option<Number>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct JkfMoveTimeFormat {
    now: JkfTimeFormat,
    total: JkfTimeFormat,
}

#[cfg(test)]
mod tests {
    #[test]
    fn to_jkf() {}
}
