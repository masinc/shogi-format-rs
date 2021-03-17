use std::{fmt::Display, time::Duration};

use serde::{Deserialize, Serialize};

use super::{
    color::Color,
    piece::Piece,
    square::{Square, ToSquare, TryToSquare},
};

pub trait AfterMovesToString {
    fn after_moves_to_string(&self) -> Option<String>;
}

pub trait BeforeMovesToString {
    fn before_moves_to_string(&self) -> Option<String>;
}

#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub enum BeforeMoves {
    Square(Square),
    Stand,
}

impl BeforeMoves {
    pub fn new_square(square: Square) -> Self {
        Self::Square(square)
    }

    pub fn new_stand() -> Self {
        BeforeMoves::Stand
    }
}

impl TryToSquare for BeforeMoves {
    fn try_square_ptr(&self) -> Option<&Square> {
        match self {
            BeforeMoves::Square(square) => Some(square),
            BeforeMoves::Stand => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub struct AfterMoves {
    square: Square,
    promote: bool,
}

impl AfterMoves {
    pub fn new(square: Square, promote: bool) -> Self {
        Self { square, promote }
    }
}

impl ToSquare for AfterMoves {
    fn square_ptr(&self) -> &Square {
        &self.square
    }
}

#[derive(Debug, Clone, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub enum MovesMetadata {
    Comment(String),
    ConsumeTime(Duration),
    TotalConsumeTime(Duration),
}

#[derive(Debug, Clone, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub struct MoveAction {
    piece: Piece,
    before: Option<BeforeMoves>,
    after: AfterMoves,
}

impl MoveAction {
    pub fn new(piece: Piece, after: AfterMoves, before: Option<BeforeMoves>) -> Self {
        MoveAction {
            piece,
            before,
            after,
        }
    }
}

impl AfterMovesToString for MoveAction {
    fn after_moves_to_string(&self) -> Option<String> {
        let suffix = if self.after.promote {
            "成"
        } else if self.before == Some(BeforeMoves::Stand) {
            "打"
        } else {
            ""
        };

        Some(format!(
            "{}{}{}",
            self.after.square.to_string(),
            self.piece.to_string(),
            suffix
        ))
    }
}

impl BeforeMovesToString for MoveAction {
    fn before_moves_to_string(&self) -> Option<String> {
        match self.before {
            None => None,
            Some(ref before) => match before {
                BeforeMoves::Stand => None,
                BeforeMoves::Square(square) => Some(square.to_string()),
            },
        }
    }
}

#[derive(Debug, Clone, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub enum MoveActionKind {
    Action(MoveAction),
    Resignation,
    IllegalAction,
    RepentitionOfMoves,
    Interruption,
}

#[derive(Debug, Clone, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub struct Moves {
    metadata: Option<Vec<MovesMetadata>>,
    color: Color,
    action: MoveActionKind,
    // branch
}

impl Moves {
    #[inline]
    pub fn resign(color: Color) -> Self {
        Self {
            metadata: None,
            color,
            action: MoveActionKind::Resignation,
        }
    }

    #[inline]
    pub fn interrupt(color: Color) -> Self {
        Self {
            metadata: None,
            color,
            action: MoveActionKind::Interruption,
        }
    }

    #[inline]
    pub fn color(&self) -> Color {
        self.color
    }

    #[inline]
    pub fn has_resign(&self) -> bool {
        self.action == MoveActionKind::Resignation
    }

    pub fn piece(&self) -> Option<&Piece> {
        match &self.action {
            MoveActionKind::Action(action) => Some(&action.piece),
            _ => None,
        }
    }

    pub fn before_move(&self) -> Option<&BeforeMoves> {
        match &self.action {
            MoveActionKind::Action(action) => action.before.as_ref(),
            _ => None,
        }
    }

    pub fn after_move(&self) -> Option<&AfterMoves> {
        match &self.action {
            MoveActionKind::Action(action) => Some(&action.after),
            _ => None,
        }
    }
}

impl BeforeMovesToString for Moves {
    fn before_moves_to_string(&self) -> Option<String> {
        match self.action {
            MoveActionKind::Action(ref action) => match action.before_moves_to_string() {
                Some(s) => Some(format!("{}{}", s, action.piece.to_string())),
                None => None,
            },
            _ => None,
        }
    }
}

impl AfterMovesToString for Moves {
    fn after_moves_to_string(&self) -> Option<String> {
        match self.action {
            MoveActionKind::Action(ref m) => m.after_moves_to_string(),
            _ => None,
        }
    }
}

impl Display for Moves {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.action {
            MoveActionKind::Action(action) => {
                write!(
                    f,
                    "{}{}",
                    action.before_moves_to_string().unwrap(),
                    if let Some(BeforeMoves::Square(square)) = action.before {
                        format!("({})", square.to_string())
                    } else {
                        "".to_owned()
                    }
                )
            }
            MoveActionKind::Resignation => write!(f, "投了"),
            MoveActionKind::IllegalAction => write!(f, "反則"),
            MoveActionKind::RepentitionOfMoves => write!(f, "千日手"),
            MoveActionKind::Interruption => write!(f, "中断"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn before_moves_new_stand() {
        assert_eq!(BeforeMoves::new_stand(), BeforeMoves::Stand);
    }

    #[test]
    fn before_moves_new_square() {
        if let BeforeMoves::Square(ref square) = BeforeMoves::new_square(Square::new(1, 1).unwrap())
        {
            assert_eq!(square.x(), 1);
            assert_eq!(square.y(), 1);
        } else {
            unreachable!()
        }
    }

    #[test]
    fn before_move_try_square_ptr() {
        let bm_square = BeforeMoves::new_square(Square::new(1, 1).unwrap());
        assert_eq!(
            bm_square.try_square_ptr(),
            Some(&Square::new(1, 1).unwrap())
        );

        let bm_stand = BeforeMoves::new_stand();
        assert_eq!(bm_stand.try_square_ptr(), None);
    }
}
