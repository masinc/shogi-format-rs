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
    #[inline]
    pub fn new_square(square: Square) -> Self {
        Self::Square(square)
    }

    #[inline]
    pub fn new_stand() -> Self {
        BeforeMoves::Stand
    }
}

impl From<Square> for BeforeMoves {
    #[inline]
    fn from(value: Square) -> Self {
        BeforeMoves::Square(value)
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
    #[inline]
    pub fn new(square: Square, promote: bool) -> Self {
        Self { square, promote }
    }
}

impl ToSquare for AfterMoves {
    #[inline]
    fn square_ptr(&self) -> &Square {
        &self.square
    }
}

#[derive(Default, Debug, Clone, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub struct MovesMetadata {
    comment: Option<String>,
    consume_time: Option<Duration>,
}

impl MovesMetadata {
    #[inline]
    pub fn builder() -> MovesMetadataBuilder {
        Default::default()
    }
}

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub struct MovesMetadataBuilder {
    comment: Option<String>,
    consume_time: Option<Duration>,
}

impl MovesMetadataBuilder {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn comment(self, value: impl Into<String>) -> Self {
        Self {
            comment: Some(value.into()),
            ..self
        }
    }

    #[inline]
    pub fn consume_time(self, value: Duration) -> Self {
        Self {
            consume_time: Some(value),
            ..self
        }
    }

    #[inline]
    pub fn build(self) -> MovesMetadata {
        MovesMetadata {
            comment: self.comment,
            consume_time: self.consume_time,
        }
    }
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
    metadata: Option<MovesMetadata>,
    color: Color,
    action: MoveActionKind,
}

impl Moves {
    #[inline]
    pub fn resignation(color: Color) -> Self {
        Self {
            metadata: None,
            color,
            action: MoveActionKind::Resignation,
        }
    }

    #[inline]
    pub fn interruption(color: Color) -> Self {
        Self {
            metadata: None,
            color,
            action: MoveActionKind::Interruption,
        }
    }

    #[inline]
    pub fn illegal_action(color: Color) -> Self {
        Self {
            metadata: None,
            color,
            action: MoveActionKind::IllegalAction,
        }
    }

    #[inline]
    pub fn repentition_of_moves(color: Color) -> Self {
        Self {
            metadata: None,
            color,
            action: MoveActionKind::RepentitionOfMoves,
        }
    }

    #[inline]
    pub fn builder() -> MovesBuilder<(), ()> {
        Default::default()
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MovesBuilder<TColor, TMoveActionKind> {
    metadata: Option<MovesMetadata>,
    color: TColor,
    action: TMoveActionKind,
}

impl MovesBuilder<(), ()> {
    #[inline]
    pub fn new() -> Self {
        MovesBuilder {
            metadata: None,
            color: (),
            action: (),
        }
    }
}

impl<TColor, TMoveActionKind> MovesBuilder<TColor, TMoveActionKind> {
    #[inline]
    pub fn metadata(self, value: MovesMetadata) -> Self {
        Self {
            metadata: Some(value),
            ..self
        }
    }
}

impl<TMoveActionKind> MovesBuilder<(), TMoveActionKind> {
    #[inline]
    pub fn color(self, value: Color) -> MovesBuilder<Color, TMoveActionKind> {
        MovesBuilder {
            metadata: self.metadata,
            color: value,
            action: self.action,
        }
    }
}

impl<TColor> MovesBuilder<TColor, ()> {
    #[inline]
    pub fn action(self, value: impl Into<MoveActionKind>) -> MovesBuilder<TColor, MoveActionKind> {
        MovesBuilder {
            metadata: self.metadata,
            color: self.color,
            action: value.into(),
        }
    }
}

impl MovesBuilder<Color, MoveActionKind> {
    #[inline]
    pub fn build(self) -> Moves {
        Moves {
            metadata: self.metadata,
            color: self.color,
            action: self.action,
        }
    }
}

impl Default for MovesBuilder<(), ()> {
    #[inline]
    fn default() -> Self {
        Self::new()
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

    #[test]
    fn moves_metadata_builder() {
        assert_eq!(MovesMetadata::builder(), MovesMetadataBuilder::new());
        assert_eq!(MovesMetadata::builder(), MovesMetadataBuilder::default());
    }

    #[test]
    fn moves_metadata_comment() {
        assert_eq!(
            MovesMetadata::builder().comment("abc").build(),
            MovesMetadata {
                comment: Some("abc".into()),
                ..Default::default()
            }
        )
    }

    #[test]
    fn moves_metadata_consume_time() {
        assert_eq!(
            MovesMetadata::builder()
                .consume_time(Duration::from_secs(10))
                .build(),
            MovesMetadata {
                consume_time: Some(Duration::from_secs(10)),
                ..Default::default()
            }
        )
    }

    #[test]
    fn moves_builder() {
        assert_eq!(Moves::builder(), MovesBuilder::new());
        assert_eq!(Moves::builder(), MovesBuilder::default());
    }

    #[test]
    fn moves_builder_build() {
        let builder = Moves::builder()
            .metadata(MovesMetadataBuilder::new().comment("abc").build())
            .color(Color::White)
            .action(MoveActionKind::Resignation);

        assert_eq!(
            builder.build(),
            Moves {
                metadata: Some(MovesMetadata {
                    comment: Some("abc".into()),
                    ..Default::default()
                }),
                color: Color::White,
                action: MoveActionKind::Resignation,
            }
        )
    }
}
