use super::{
    color::Color,
    piece::{self, Piece, PieceKind, PromotedPiece, UnpromotedPiece},
    square::{self, Square},
};
use piece::{PieceNumber, PieceWithColor};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    convert::TryInto,
    hash::Hash,
    mem,
    ops::Deref,
};

pub type Board = HashSet<BoardValue>;
pub type PieceCount = usize;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BoardValue {
    Field {
        piece: PieceWithColor,
        square: Square,
    },
    Stand {
        piece: PieceWithColor,
        count: PieceCount,
    },
    Outside {
        piece: Piece,
        count: PieceCount,
    },
}

impl BoardValue {
    #[inline]
    pub fn new_field(piece: PieceWithColor, square: Square) -> Self {
        Self::Field { piece, square }
    }

    #[inline]
    pub fn new_stand(piece: PieceWithColor, count: PieceCount) -> Self {
        Self::Stand { piece, count }
    }

    #[inline]
    pub fn new_outside(piece: Piece, count: PieceCount) -> Self {
        Self::Outside { piece, count }
    }

    #[inline]
    pub fn shallow_eq(&self, other: &Self) -> bool {
        ShallowBoardValue(self) == ShallowBoardValue(other)
    }

    #[inline]
    pub fn shallow_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ShallowBoardValue(self).hash(state);
    }
}

impl<'a> From<ShallowBoardValue<'a>> for &'a BoardValue {
    #[inline]
    fn from(value: ShallowBoardValue<'a>) -> Self {
        value.0
    }
}

#[derive(Debug, Eq, Clone)]
pub(crate) struct ShallowBoardValue<'a>(&'a BoardValue);

impl Hash for ShallowBoardValue<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.0 {
            BoardValue::Field { piece: _, square } => {
                mem::discriminant(self.0).hash(state);
                // piece.hash(state);
                square.hash(state);
            }
            BoardValue::Stand { piece, count: _ } => {
                mem::discriminant(self.0).hash(state);
                piece.hash(state);
                // count.hash(state);
            }
            BoardValue::Outside { piece, count: _ } => {
                mem::discriminant(self.0).hash(state);
                piece.hash(state);
                // count.hash(state);
            }
        }
    }
}

impl PartialEq for ShallowBoardValue<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let self_v = mem::discriminant(self.0);
        let other_v = mem::discriminant(other.0);
        if self_v != other_v {
            return false;
        }

        match (&self.0, &other.0) {
            (
                BoardValue::Field { piece: _, square },
                BoardValue::Field {
                    piece: _other_piece,
                    square: other_square,
                },
            ) => square == other_square,
            (
                BoardValue::Stand { piece, count: _ },
                BoardValue::Stand {
                    piece: other_piece,
                    count: _other_count,
                },
            ) => piece == other_piece,
            (
                BoardValue::Outside { piece, count: _ },
                BoardValue::Outside {
                    piece: other_piece,
                    count: _other_count,
                },
            ) => piece == other_piece,
            _ => false,
        }
    }
}

impl<'a> From<&'a BoardValue> for ShallowBoardValue<'a> {
    #[inline]
    fn from(value: &'a BoardValue) -> Self {
        Self(value)
    }
}

impl Deref for ShallowBoardValue<'_> {
    type Target = BoardValue;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

pub trait BoardImpl {
    #[inline]
    fn new_board() -> Board {
        Board::with_capacity(40)
    }

    fn field(&self) -> HashMap<Square, PieceWithColor>;
    fn stand(&self) -> HashMap<PieceWithColor, PieceCount>;
    fn outside(&self) -> HashMap<Piece, PieceCount>;
    fn update(&mut self, value: &BoardValue, update_value: BoardValue) -> bool;
    fn piece_count(&self) -> PieceCount;
    fn find_piece(&self, piece: impl Into<PieceKind>) -> HashSet<BoardValue>;
    fn filter_field(&self) -> HashSet<BoardValue>;
    fn filter_stand(&self) -> HashSet<BoardValue>;
    fn filter_outside(&self) -> HashSet<BoardValue>;
    fn shallow_contains(&self, value: &BoardValue) -> bool;
    fn shallow_get(&self, value: &BoardValue) -> Option<&BoardValue>;
    fn shallow_take(&mut self, value: &BoardValue) -> Option<BoardValue>;
    fn shallow_insert(&mut self, value: BoardValue) -> bool;
    fn shallow_update(&mut self, value: &BoardValue, update_value: BoardValue) -> bool;
}

impl BoardImpl for Board {
    fn field(&self) -> HashMap<Square, PieceWithColor> {
        self.iter()
            .filter_map(|x| match x {
                BoardValue::Field { square, piece } => Some((*square, *piece)),
                _ => None,
            })
            .collect()
    }

    fn stand(&self) -> HashMap<PieceWithColor, PieceCount> {
        self.iter()
            .filter_map(|x| match x {
                BoardValue::Stand { piece, count } => Some((*piece, *count)),
                _ => None,
            })
            .collect()
    }

    fn outside(&self) -> HashMap<Piece, PieceCount> {
        self.iter()
            .filter_map(|x| match x {
                BoardValue::Outside { piece, count } => Some((*piece, *count)),
                _ => None,
            })
            .collect()
    }

    fn update(&mut self, value: &BoardValue, update_value: BoardValue) -> bool {
        if self.contains(value) {
            self.remove(value) && self.insert(update_value)
        } else {
            false
        }
    }

    fn piece_count(&self) -> PieceCount {
        self.iter()
            .map(|x| match x {
                BoardValue::Field {
                    piece: _,
                    square: _,
                } => 1,
                BoardValue::Stand { piece: _, count } => *count,
                BoardValue::Outside { piece: _, count } => *count,
            })
            .sum()
    }

    fn find_piece(&self, piece: impl Into<PieceKind>) -> HashSet<BoardValue> {
        let target_piece: PieceKind = piece.into();
        let target_piece = target_piece.piece();

        self.iter()
            .filter(|x| match x {
                BoardValue::Field { piece, square: _ } => piece.piece() == target_piece,
                BoardValue::Stand { piece, count: _ } => piece.piece() == target_piece,
                BoardValue::Outside { piece, count: _ } => piece == target_piece,
            })
            .cloned()
            .collect()
    }

    fn filter_field(&self) -> HashSet<BoardValue> {
        self.iter()
            .filter(|x| matches!(x, BoardValue::Field { .. }))
            .cloned()
            .collect()
    }

    fn filter_stand(&self) -> HashSet<BoardValue> {
        self.iter()
            .filter(|x| matches!(x, BoardValue::Stand { .. }))
            .cloned()
            .collect()
    }

    fn filter_outside(&self) -> HashSet<BoardValue> {
        self.iter()
            .filter(|x| matches!(x, BoardValue::Outside { .. }))
            .cloned()
            .collect()
    }

    fn shallow_contains(&self, value: &BoardValue) -> bool {
        let value = ShallowBoardValue(value);
        self.iter().any(|x| value == ShallowBoardValue(x))
    }

    fn shallow_get(&self, value: &BoardValue) -> Option<&BoardValue> {
        let value = ShallowBoardValue(value);
        self.iter().find(move |x| value == ShallowBoardValue(x))
    }

    fn shallow_take(&mut self, value: &BoardValue) -> Option<BoardValue> {
        self.replace(value)
        let value = self.shallow_get(value);

        if let Some(value) = value {
            self.take(value)
        } else {
            None
        }
        // let value = {
        //     let shallow = ShallowBoardValue(&value);
        //     self.iter().find(move |x| shallow == ShallowBoardValue(x))
        // };
        // if let Some(value) = value {
        //     self.take(value)
        // } else {
        //     None
        // }
        // value.and_then(move |x| self.take(&x.clone()))
    }

    fn shallow_insert(&mut self, value: BoardValue) -> bool {
        if let Some(target) = self.shallow_take(&value) {
            self.remove(&target) && self.insert(value)
        } else {
            false
        }
    }

    fn shallow_update(&mut self, value: &BoardValue, update_value: BoardValue) -> bool {
        if let Some(value) = self.shallow_take(&value) {
            self.remove(&value) && self.insert(update_value)
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Handicap {
    Rook,
    Bishop,
    LeftLance,
    RookAndLeftLance,
    BishopAndLeftLance,
    TwoPiece,
    FourPiece,
    SixPiece,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BoardType {
    Even,
    Blank,
    Handicap(Handicap),
    CustomBoard(Board),
}

macro_rules! make_board {
    ($(field => {
        $($fcolor:path => {
            $(($fx:expr, $fy:expr) => $fpiece:expr),*
        }),*
    })?
    $(outside => {
        $($opiece:expr => $ocount:expr),*
    })?
    ) => {{
        let mut board = Board::with_capacity(40);
        $( // field
            $($(
                board.insert(BoardValue::Field {
                    square: Square::new_unchecked($fx, $fy),
                    piece: PieceWithColor::new($fpiece, $fcolor),
                });
            )*)*
        )*

        $( // outside
            $(
                board.insert(BoardValue::Outside {
                    piece: $opiece.into(),
                    count: $ocount,
                });
            )*
        )*
        board
    }};
}

impl BoardType {
    fn get_board_even() -> Board {
        make_board!(
            field => {
                Color::Black => {
                    (9, 7) => UnpromotedPiece::Pawn,
                    (8, 7) => UnpromotedPiece::Pawn,
                    (7, 7) => UnpromotedPiece::Pawn,
                    (6, 7) => UnpromotedPiece::Pawn,
                    (5, 7) => UnpromotedPiece::Pawn,
                    (4, 7) => UnpromotedPiece::Pawn,
                    (3, 7) => UnpromotedPiece::Pawn,
                    (2, 7) => UnpromotedPiece::Pawn,
                    (1, 7) => UnpromotedPiece::Pawn,
                    (8, 8) => UnpromotedPiece::Bishop,
                    (2, 8) => UnpromotedPiece::Rook,
                    (9, 9) => UnpromotedPiece::Lance,
                    (8, 9) => UnpromotedPiece::Knight,
                    (7, 9) => UnpromotedPiece::Silver,
                    (6, 9) => UnpromotedPiece::Gold,
                    (5, 9) => UnpromotedPiece::King,
                    (4, 9) => UnpromotedPiece::Gold,
                    (3, 9) => UnpromotedPiece::Silver,
                    (2, 9) => UnpromotedPiece::Knight,
                    (1, 9) => UnpromotedPiece::Lance
                },
                Color::White => {
                    (9, 3) => UnpromotedPiece::Pawn,
                    (8, 3) => UnpromotedPiece::Pawn,
                    (7, 3) => UnpromotedPiece::Pawn,
                    (6, 3) => UnpromotedPiece::Pawn,
                    (5, 3) => UnpromotedPiece::Pawn,
                    (4, 3) => UnpromotedPiece::Pawn,
                    (3, 3) => UnpromotedPiece::Pawn,
                    (2, 3) => UnpromotedPiece::Pawn,
                    (1, 3) => UnpromotedPiece::Pawn,
                    (2, 2) => UnpromotedPiece::Bishop,
                    (8, 2) => UnpromotedPiece::Rook,
                    (9, 1) => UnpromotedPiece::Lance,
                    (8, 1) => UnpromotedPiece::Knight,
                    (7, 1) => UnpromotedPiece::Silver,
                    (6, 1) => UnpromotedPiece::Gold,
                    (5, 1) => UnpromotedPiece::King,
                    (4, 1) => UnpromotedPiece::Gold,
                    (3, 1) => UnpromotedPiece::Silver,
                    (2, 1) => UnpromotedPiece::Knight,
                    (1, 1) => UnpromotedPiece::Lance
                }
            }
        )
    }

    fn get_board_blank() -> Board {
        make_board!(
            outside => {
                UnpromotedPiece::Pawn => 18,
                UnpromotedPiece::Lance => 4,
                UnpromotedPiece::Knight => 4,
                UnpromotedPiece::Silver => 4,
                UnpromotedPiece::Gold => 4,
                UnpromotedPiece::Bishop => 2,
                UnpromotedPiece::Rook => 2,
                UnpromotedPiece::King => 2
            }
        )
    }

    fn get_board_handicap(handicap: &Handicap) -> Board {
        fn to_outside(
            board: &mut Board,
            color: Color,
            piece: impl Into<Piece> + Copy,
            x: PieceNumber,
            y: PieceNumber,
        ) {
            let piece = piece.into();
            let field = &BoardValue::new_field(
                PieceWithColor::new(piece, color),
                Square::new_unchecked(x, y),
            );

            if !board.remove(field) {
                unreachable!();
            }

            if let Some(BoardValue::Outside { piece, count }) =
                board.take(PieceWithPosition::new(piece, PiecePosition::Outside))
            {
                let outside = BoardValue::new_outside(piece, count + 1);
                if !board.insert(outside) {
                    unreachable!()
                }
            } else if !board.insert(BoardValue::new_outside(piece, 1)) {
                unreachable!()
            }
        }

        match handicap {
            Handicap::Rook => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Rook, 8, 2);
                board
            }
            Handicap::Bishop => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Bishop, 2, 2);
                board
            }
            Handicap::LeftLance => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 1, 1);
                board
            }
            Handicap::RookAndLeftLance => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Rook, 8, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 1, 1);
                board
            }
            Handicap::BishopAndLeftLance => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Bishop, 2, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 1, 1);
                board
            }
            Handicap::TwoPiece => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Bishop, 2, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Rook, 8, 2);
                board
            }
            Handicap::FourPiece => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Bishop, 2, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Rook, 8, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 1, 1);
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 9, 1);
                board
            }
            Handicap::SixPiece => {
                let mut board = Self::get_board_even();
                to_outside(&mut board, Color::White, UnpromotedPiece::Bishop, 2, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Rook, 8, 2);
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 1, 1);
                to_outside(&mut board, Color::White, UnpromotedPiece::Knight, 2, 1);
                to_outside(&mut board, Color::White, UnpromotedPiece::Knight, 8, 1);
                to_outside(&mut board, Color::White, UnpromotedPiece::Lance, 9, 1);
                board
            }
        }
    }

    pub fn get_board(&self) -> Board {
        match self {
            BoardType::Even => Self::get_board_even(),
            BoardType::Blank => Self::get_board_blank(),
            BoardType::Handicap(handicap) => Self::get_board_handicap(handicap),
            BoardType::CustomBoard(_) => {
                todo!()
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use std::{collections::hash_map::DefaultHasher, hash::Hasher};

    use super::*;

    #[test]
    fn board_value_new_field() {
        assert_eq!(
            BoardValue::new_field(
                PieceWithColor::new_black(UnpromotedPiece::Pawn),
                Square::new_unchecked(5, 5)
            ),
            BoardValue::Field {
                piece: PieceWithColor::new_black(UnpromotedPiece::Pawn),
                square: Square::new_unchecked(5, 5)
            }
        );
    }

    #[test]
    fn board_value_new_stand() {
        assert_eq!(
            BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 4),
            BoardValue::Stand {
                piece: PieceWithColor::new_black(UnpromotedPiece::Pawn),
                count: 4
            }
        );
    }

    #[test]
    fn board_value_new_outside() {
        assert_eq!(
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 2),
            BoardValue::Outside {
                piece: UnpromotedPiece::Lance.into(),
                count: 2,
            }
        );
    }

    impl BoardValue {
        fn get_shallow_hash(&self) -> u64 {
            let mut hasher = DefaultHasher::new();

            ShallowBoardValue(self).hash(&mut hasher);
            hasher.finish()
        }
    }

    #[test]
    fn shallow_board_value_hash_field() {
        let hasher1 = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(5, 5),
        )
        .get_shallow_hash();

        let hasher2 = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Bishop),
            Square::new_unchecked(5, 5),
        )
        .get_shallow_hash();

        let hasher3 = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(1, 1),
        )
        .get_shallow_hash();

        let hasher4 = BoardValue::new_field(
            PieceWithColor::new_black(PromotedPiece::Rook),
            Square::new_unchecked(8, 9),
        )
        .get_shallow_hash();

        let hasher_stand =
            BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 1)
                .get_shallow_hash();
        let hasher_outside =
            BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 1).get_shallow_hash();

        assert_eq!(hasher1, hasher2);
        assert_ne!(hasher1, hasher3);
        assert_ne!(hasher1, hasher4);
        assert_ne!(hasher1, hasher_stand);
        assert_ne!(hasher1, hasher_outside);

        assert_ne!(hasher2, hasher3);
        assert_ne!(hasher2, hasher4);
        assert_ne!(hasher2, hasher_stand);
        assert_ne!(hasher2, hasher_outside);

        assert_ne!(hasher3, hasher4);
        assert_ne!(hasher3, hasher_stand);
        assert_ne!(hasher3, hasher_outside);

        assert_ne!(hasher4, hasher_stand);
        assert_ne!(hasher4, hasher_outside);
    }

    #[test]
    fn board_value_hash_stand() {
        let hasher1 = BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 1)
            .get_shallow_hash();

        let hasher2 = BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 18)
            .get_shallow_hash();

        let hasher3 = BoardValue::new_stand(PieceWithColor::new_white(UnpromotedPiece::Pawn), 1)
            .get_shallow_hash();

        let hasher4 = BoardValue::new_stand(PieceWithColor::new_white(UnpromotedPiece::Rook), 1)
            .get_shallow_hash();

        let hasher5 = BoardValue::new_stand(PieceWithColor::new_white(PromotedPiece::Pawn), 1)
            .get_shallow_hash();

        let hasher_field = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(5, 5),
        )
        .get_shallow_hash();

        let hasher_outside =
            BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 1).get_shallow_hash();

        assert_eq!(hasher1, hasher2);
        assert_ne!(hasher1, hasher3);
        assert_ne!(hasher1, hasher4);
        assert_ne!(hasher1, hasher5);
        assert_ne!(hasher1, hasher_field);
        assert_ne!(hasher1, hasher_outside);

        assert_ne!(hasher2, hasher3);
        assert_ne!(hasher2, hasher4);
        assert_ne!(hasher2, hasher5);
        assert_ne!(hasher2, hasher_field);
        assert_ne!(hasher2, hasher_outside);

        assert_ne!(hasher3, hasher4);
        assert_ne!(hasher3, hasher5);
        assert_ne!(hasher3, hasher_field);
        assert_ne!(hasher3, hasher_outside);

        assert_ne!(hasher4, hasher5);
        assert_ne!(hasher4, hasher_field);
        assert_ne!(hasher4, hasher_outside);
    }

    #[test]
    fn board_value_hash_outside() {
        let hasher1 = BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 1).get_shallow_hash();
        let hasher2 = BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 8).get_shallow_hash();
        let hasher3 = BoardValue::new_outside(PromotedPiece::Pawn.into(), 1).get_shallow_hash();
        let hasher4 = BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1).get_shallow_hash();

        let hasher_field = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(5, 5),
        )
        .get_shallow_hash();

        let hasher_stand =
            BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 1)
                .get_shallow_hash();

        assert_eq!(hasher1, hasher2);
        assert_ne!(hasher1, hasher3);
        assert_ne!(hasher1, hasher4);
        assert_ne!(hasher1, hasher_field);
        assert_ne!(hasher1, hasher_stand);

        assert_ne!(hasher2, hasher3);
        assert_ne!(hasher2, hasher4);
        assert_ne!(hasher2, hasher_field);
        assert_ne!(hasher2, hasher_stand);

        assert_ne!(hasher3, hasher4);
        assert_ne!(hasher3, hasher_field);
        assert_ne!(hasher3, hasher_stand);

        assert_ne!(hasher4, hasher_field);
        assert_ne!(hasher4, hasher_stand);
    }

    #[test]
    #[rustfmt::skip]
    fn shallow_board_value_eq_field() {
        let value1 = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(5, 5),
        );
        let value2 = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Bishop),
            Square::new_unchecked(5, 5),
        );
        let value3 = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(1, 1),
        );
        let value4 = BoardValue::new_field(
            PieceWithColor::new_black(PromotedPiece::Rook),
            Square::new_unchecked(8, 9),
        );
        let value_stand =
            BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 1);
        let value_outside = BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 1);

        assert_eq!(ShallowBoardValue(&value1), ShallowBoardValue(&value2));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value3));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value_stand));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value_outside));

        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value3));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value_stand));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value_outside));

        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value_stand));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value_outside));

        assert_ne!(ShallowBoardValue(&value4), ShallowBoardValue(&value_stand));
    }

    #[test]
    #[rustfmt::skip]
    fn shallow_board_value_eq_stand() {
        let value1 = BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 1);
        let value2 = BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 18);
        let value3 = BoardValue::new_stand(PieceWithColor::new_white(UnpromotedPiece::Pawn), 1);
        let value4 = BoardValue::new_stand(PieceWithColor::new_white(UnpromotedPiece::Rook), 1);
        let value5 = BoardValue::new_stand(PieceWithColor::new_white(PromotedPiece::Pawn), 1);
        let value_field = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(5, 5),
        );
        let value_outside = BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 1);

        assert_eq!(ShallowBoardValue(&value1), ShallowBoardValue(&value2));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value3));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value5));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value_outside));

        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value3));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value5));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value_outside));

        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value5));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value_outside));

        assert_ne!(ShallowBoardValue(&value4), ShallowBoardValue(&value5));
        assert_ne!(ShallowBoardValue(&value4), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value4), ShallowBoardValue(&value_outside));
    }

    #[test]
    #[rustfmt::skip]
    fn shallow_board_value_eq_outside() {
        let value1 = BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 1);
        let value2 = BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 8);
        let value3 = BoardValue::new_outside(PromotedPiece::Pawn.into(), 1);
        let value4 = BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1);

        let value_field = BoardValue::new_field(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            Square::new_unchecked(5, 5),
        );
        let value_stand =
            BoardValue::new_stand(PieceWithColor::new_black(UnpromotedPiece::Pawn), 1);

        assert_eq!(ShallowBoardValue(&value1), ShallowBoardValue(&value2));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value3));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value1), ShallowBoardValue(&value_stand));

        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value3));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value2), ShallowBoardValue(&value_stand));

        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value4));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value3), ShallowBoardValue(&value_stand));

        assert_ne!(ShallowBoardValue(&value4), ShallowBoardValue(&value_field));
        assert_ne!(ShallowBoardValue(&value4), ShallowBoardValue(&value_stand));
    }

    fn make_field(
        color: Color,
        x: PieceNumber,
        y: PieceNumber,
        piece: UnpromotedPiece,
    ) -> BoardValue {
        BoardValue::Field {
            square: Square::new_unchecked(x, y),
            piece: PieceWithColor::new(piece, color),
        }
    }

    #[test]
    fn board_type_get_board_even() {
        use Color::*;

        let board = BoardType::Even.get_board();
        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            make_field(White, 1, 1, UnpromotedPiece::Lance),
            make_field(White, 2, 2, UnpromotedPiece::Bishop),
            make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_blank() {
        let board = BoardType::Blank.get_board();

        assert_eq!(board.len(), 8);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            BoardValue::new_outside(UnpromotedPiece::Pawn.into(), 18),
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 4),
            BoardValue::new_outside(UnpromotedPiece::Knight.into(), 4),
            BoardValue::new_outside(UnpromotedPiece::Silver.into(), 4),
            BoardValue::new_outside(UnpromotedPiece::Gold.into(), 4),
            BoardValue::new_outside(UnpromotedPiece::Bishop.into(), 2),
            BoardValue::new_outside(UnpromotedPiece::Rook.into(), 2),
            BoardValue::new_outside(UnpromotedPiece::King.into(), 2),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_rook() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::Rook).get_board();

        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            make_field(White, 1, 1, UnpromotedPiece::Lance),
            make_field(White, 2, 2, UnpromotedPiece::Bishop),
            // make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_bishop() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::Bishop).get_board();

        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            make_field(White, 1, 1, UnpromotedPiece::Lance),
            // make_field(White, 2, 2, UnpromotedPiece::Bishop),
            make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Bishop.into(), 1),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_left_lance() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::LeftLance).get_board();

        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            // make_field(White, 1, 1, UnpromotedPiece::Lance),
            make_field(White, 2, 2, UnpromotedPiece::Bishop),
            make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 1),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_rook_and_left_lance() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::RookAndLeftLance).get_board();

        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            // make_field(White, 1, 1, UnpromotedPiece::Lance),
            make_field(White, 2, 2, UnpromotedPiece::Bishop),
            // make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_bishop_and_left_lance() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::BishopAndLeftLance).get_board();

        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            // make_field(White, 1, 1, UnpromotedPiece::Lance),
            // make_field(White, 2, 2, UnpromotedPiece::Bishop),
            make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Bishop.into(), 1),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_two_piece() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::TwoPiece).get_board();

        assert_eq!(board.len(), 40);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            make_field(White, 1, 1, UnpromotedPiece::Lance),
            // make_field(White, 2, 2, UnpromotedPiece::Bishop),
            // make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Bishop.into(), 1),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_four_piece() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::FourPiece).get_board();

        assert_eq!(board.len(), 39);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            // make_field(White, 9, 1, UnpromotedPiece::Lance),
            make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            make_field(White, 2, 1, UnpromotedPiece::Knight),
            // make_field(White, 1, 1, UnpromotedPiece::Lance),
            // make_field(White, 2, 2, UnpromotedPiece::Bishop),
            // make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Bishop.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 2),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }

    #[test]
    fn board_type_get_board_handicap_six_piece() {
        use Color::*;

        let board = BoardType::Handicap(Handicap::SixPiece).get_board();

        assert_eq!(board.len(), 38);
        assert_eq!(board.piece_count(), 40);

        let cases = [
            // make_field(White, 9, 1, UnpromotedPiece::Lance),
            // make_field(White, 8, 1, UnpromotedPiece::Knight),
            make_field(White, 7, 1, UnpromotedPiece::Silver),
            make_field(White, 6, 1, UnpromotedPiece::Gold),
            make_field(White, 5, 1, UnpromotedPiece::King),
            make_field(White, 4, 1, UnpromotedPiece::Gold),
            make_field(White, 3, 1, UnpromotedPiece::Silver),
            // make_field(White, 2, 1, UnpromotedPiece::Knight),
            // make_field(White, 1, 1, UnpromotedPiece::Lance),
            // make_field(White, 2, 2, UnpromotedPiece::Bishop),
            // make_field(White, 8, 2, UnpromotedPiece::Rook),
            make_field(White, 9, 3, UnpromotedPiece::Pawn),
            make_field(White, 8, 3, UnpromotedPiece::Pawn),
            make_field(White, 7, 3, UnpromotedPiece::Pawn),
            make_field(White, 6, 3, UnpromotedPiece::Pawn),
            make_field(White, 5, 3, UnpromotedPiece::Pawn),
            make_field(White, 4, 3, UnpromotedPiece::Pawn),
            make_field(White, 3, 3, UnpromotedPiece::Pawn),
            make_field(White, 2, 3, UnpromotedPiece::Pawn),
            make_field(White, 1, 3, UnpromotedPiece::Pawn),
            make_field(Black, 9, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 9, UnpromotedPiece::Knight),
            make_field(Black, 7, 9, UnpromotedPiece::Silver),
            make_field(Black, 6, 9, UnpromotedPiece::Gold),
            make_field(Black, 5, 9, UnpromotedPiece::King),
            make_field(Black, 4, 9, UnpromotedPiece::Gold),
            make_field(Black, 3, 9, UnpromotedPiece::Silver),
            make_field(Black, 2, 9, UnpromotedPiece::Knight),
            make_field(Black, 1, 9, UnpromotedPiece::Lance),
            make_field(Black, 8, 8, UnpromotedPiece::Bishop),
            make_field(Black, 2, 8, UnpromotedPiece::Rook),
            make_field(Black, 9, 7, UnpromotedPiece::Pawn),
            make_field(Black, 8, 7, UnpromotedPiece::Pawn),
            make_field(Black, 7, 7, UnpromotedPiece::Pawn),
            make_field(Black, 6, 7, UnpromotedPiece::Pawn),
            make_field(Black, 5, 7, UnpromotedPiece::Pawn),
            make_field(Black, 4, 7, UnpromotedPiece::Pawn),
            make_field(Black, 3, 7, UnpromotedPiece::Pawn),
            make_field(Black, 2, 7, UnpromotedPiece::Pawn),
            make_field(Black, 1, 7, UnpromotedPiece::Pawn),
            BoardValue::new_outside(UnpromotedPiece::Rook.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Bishop.into(), 1),
            BoardValue::new_outside(UnpromotedPiece::Lance.into(), 2),
            BoardValue::new_outside(UnpromotedPiece::Knight.into(), 2),
        ];

        for case in cases.iter() {
            assert!(board.contains(case), "{:?}", case);
        }
    }
}
