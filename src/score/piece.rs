use super::color::Color;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

pub type PieceNumber = u8;

#[derive(Debug, Clone, Copy, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub enum UnpromotedPiece {
    King,
    Rook,
    Bishop,
    Gold,
    Silver,
    Knight,
    Lance,
    Pawn,
}

impl Display for UnpromotedPiece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnpromotedPiece::King => "王",
            UnpromotedPiece::Rook => "飛",
            UnpromotedPiece::Bishop => "角",
            UnpromotedPiece::Gold => "金",
            UnpromotedPiece::Silver => "銀",
            UnpromotedPiece::Knight => "桂",
            UnpromotedPiece::Lance => "香",
            UnpromotedPiece::Pawn => "歩",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub enum PromotedPiece {
    Rook,
    Bishop,
    Silver,
    Knight,
    Lance,
    Pawn,
}

impl Display for PromotedPiece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            PromotedPiece::Rook => "龍",
            PromotedPiece::Bishop => "馬",
            PromotedPiece::Silver => "全",
            PromotedPiece::Knight => "圭",
            PromotedPiece::Lance => "杏",
            PromotedPiece::Pawn => "と",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub enum Piece {
    Unpromoted(UnpromotedPiece),
    Promoted(PromotedPiece),
}

impl Piece {
    #[inline]
    pub fn new(piece: impl Into<Piece>) -> Self {
        piece.into()
    }

    #[inline]
    pub fn new_unpromoted(piece: UnpromotedPiece) -> Self {
        Self::Unpromoted(piece)
    }

    #[inline]
    pub fn new_promoted(piece: PromotedPiece) -> Self {
        Self::Promoted(piece)
    }

    pub fn promote(&self) -> Option<Piece> {
        match self {
            Piece::Unpromoted(x) => match x {
                UnpromotedPiece::King => None,
                UnpromotedPiece::Rook => Some(Piece::new_promoted(PromotedPiece::Rook)),
                UnpromotedPiece::Bishop => Some(Piece::new_promoted(PromotedPiece::Bishop)),
                UnpromotedPiece::Gold => None,
                UnpromotedPiece::Silver => Some(Piece::new_promoted(PromotedPiece::Silver)),
                UnpromotedPiece::Knight => Some(Piece::new_promoted(PromotedPiece::Knight)),
                UnpromotedPiece::Lance => Some(Piece::new_promoted(PromotedPiece::Lance)),
                UnpromotedPiece::Pawn => Some(Piece::new_promoted(PromotedPiece::Pawn)),
            },
            Piece::Promoted(_) => Some(*self),
        }
    }

    pub fn unpromote(&self) -> Option<Piece> {
        match self {
            Piece::Unpromoted(_) => Some(*self),
            Piece::Promoted(x) => match x {
                PromotedPiece::Rook => Some(Piece::new_unpromoted(UnpromotedPiece::Rook)),
                PromotedPiece::Bishop => Some(Piece::new_unpromoted(UnpromotedPiece::Bishop)),
                PromotedPiece::Silver => Some(Piece::new_unpromoted(UnpromotedPiece::Silver)),
                PromotedPiece::Knight => Some(Piece::new_unpromoted(UnpromotedPiece::Knight)),
                PromotedPiece::Lance => Some(Piece::new_unpromoted(UnpromotedPiece::Lance)),
                PromotedPiece::Pawn => Some(Piece::new_unpromoted(UnpromotedPiece::Pawn)),
            },
        }
    }

    #[inline]
    pub fn flip(&mut self) -> Option<Piece> {
        match self {
            Piece::Unpromoted(_) => self.promote(),
            Piece::Promoted(_) => self.unpromote(),
        }
    }

    #[inline]
    pub fn has_unpromoted(&self) -> bool {
        match self {
            Piece::Unpromoted(_) => true,
            Piece::Promoted(_) => false,
        }
    }

    #[inline]
    pub fn has_promoted(&self) -> bool {
        match self {
            Piece::Unpromoted(_) => false,
            Piece::Promoted(_) => true,
        }
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Piece::Unpromoted(x) => x.to_string(),
            Piece::Promoted(x) => x.to_string(),
        };

        write!(f, "{}", s)
    }
}

impl From<UnpromotedPiece> for Piece {
    #[inline]
    fn from(piece: UnpromotedPiece) -> Self {
        Self::Unpromoted(piece)
    }
}

impl From<PromotedPiece> for Piece {
    #[inline]
    fn from(piece: PromotedPiece) -> Self {
        Self::Promoted(piece)
    }
}

impl From<PieceWithColor> for Piece {
    #[inline]
    fn from(piece: PieceWithColor) -> Self {
        piece.piece
    }
}

#[derive(Debug, Clone, Copy, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub struct PieceWithColor {
    piece: Piece,
    color: Color,
}

impl PieceWithColor {
    #[inline]
    pub fn new(piece: impl Into<Piece>, color: Color) -> Self {
        Self {
            piece: piece.into(),
            color,
        }
    }

    #[inline]
    pub fn new_black(piece: impl Into<Piece>) -> Self {
        Self {
            piece: piece.into(),
            color: Color::Black,
        }
    }

    #[inline]
    pub fn new_white(piece: impl Into<Piece>) -> Self {
        Self {
            piece: piece.into(),
            color: Color::White,
        }
    }

    #[inline]
    pub fn color(&self) -> &Color {
        &self.color
    }

    #[inline]
    pub fn color_mut(&mut self) -> &mut Color {
        &mut self.color
    }

    #[inline]
    pub fn set_color(&mut self, color: Color) {
        self.color = color
    }

    #[inline]
    pub fn piece(&self) -> &Piece {
        &self.piece
    }

    #[inline]
    pub fn piece_mut(&mut self) -> &mut Piece {
        &mut self.piece
    }

    #[inline]
    pub fn set_piece(&mut self, piece: impl Into<Piece>) {
        self.piece = piece.into()
    }
}

#[derive(Debug, Clone, Copy, Hash, Deserialize, Serialize, PartialEq, Eq)]
pub enum PieceKind {
    WithoutColor(Piece),
    WithColor(PieceWithColor),
}

impl PieceKind {
    #[inline]
    pub fn new(piece: impl Into<PieceKind>) -> Self {
        piece.into()
    }

    #[inline]
    pub fn new_white(piece: impl Into<Piece>) -> Self {
        Self::WithColor(PieceWithColor::new_white(piece))
    }

    #[inline]
    pub fn new_black(piece: impl Into<Piece>) -> Self {
        Self::WithColor(PieceWithColor::new_black(piece))
    }

    pub fn piece(&self) -> &Piece {
        match self {
            PieceKind::WithoutColor(piece) => piece,
            PieceKind::WithColor(piece) => &piece.piece,
        }
    }

    pub fn piece_mut(&mut self) -> &mut Piece {
        match self {
            PieceKind::WithoutColor(piece) => piece,
            PieceKind::WithColor(piece) => &mut piece.piece,
        }
    }

    pub fn color(&self) -> Option<&Piece> {
        match self {
            PieceKind::WithoutColor(_) => None,
            PieceKind::WithColor(piece) => Some(&piece.piece),
        }
    }

    pub fn color_mut(&mut self) -> Option<&mut Piece> {
        match self {
            PieceKind::WithoutColor(_) => None,
            PieceKind::WithColor(piece) => Some(&mut piece.piece),
        }
    }
}

impl From<Piece> for PieceKind {
    #[inline]
    fn from(piece: Piece) -> Self {
        Self::WithoutColor(piece)
    }
}

impl From<PieceWithColor> for PieceKind {
    #[inline]
    fn from(piece: PieceWithColor) -> Self {
        Self::WithColor(piece)
    }
}

impl From<UnpromotedPiece> for PieceKind {
    #[inline]
    fn from(piece: UnpromotedPiece) -> Self {
        Self::WithoutColor(piece.into())
    }
}

impl From<PromotedPiece> for PieceKind {
    #[inline]
    fn from(piece: PromotedPiece) -> Self {
        Self::WithoutColor(piece.into())
    }
}

impl AsRef<Piece> for PieceKind {
    fn as_ref(&self) -> &Piece {
        match self {
            PieceKind::WithoutColor(piece) => piece,
            PieceKind::WithColor(piece_with_color) => &piece_with_color.piece,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unpromoted_piece_to_string() {
        assert_eq!(UnpromotedPiece::King.to_string(), "王");
        assert_eq!(UnpromotedPiece::Rook.to_string(), "飛");
        assert_eq!(UnpromotedPiece::Bishop.to_string(), "角");
        assert_eq!(UnpromotedPiece::Gold.to_string(), "金");
        assert_eq!(UnpromotedPiece::Silver.to_string(), "銀");
        assert_eq!(UnpromotedPiece::Knight.to_string(), "桂");
        assert_eq!(UnpromotedPiece::Lance.to_string(), "香");
        assert_eq!(UnpromotedPiece::Pawn.to_string(), "歩");
    }

    #[test]
    fn promoted_piece_to_string() {
        assert_eq!(PromotedPiece::Rook.to_string(), "龍");
        assert_eq!(PromotedPiece::Bishop.to_string(), "馬");
        assert_eq!(PromotedPiece::Silver.to_string(), "全");
        assert_eq!(PromotedPiece::Knight.to_string(), "圭");
        assert_eq!(PromotedPiece::Lance.to_string(), "杏");
        assert_eq!(PromotedPiece::Pawn.to_string(), "と");
    }

    #[test]
    #[rustfmt::skip]
    fn piece_to_string() {
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::King).to_string(), "王");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Rook).to_string(), "飛");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Bishop).to_string(), "角");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Gold).to_string(), "金");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Silver).to_string(), "銀");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Knight).to_string(), "桂");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Lance).to_string(), "香");
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Pawn).to_string(), "歩");
        assert_eq!(Piece::new_promoted(PromotedPiece::Rook).to_string(), "龍");
        assert_eq!(Piece::new_promoted(PromotedPiece::Bishop).to_string(), "馬");
        assert_eq!(Piece::new_promoted(PromotedPiece::Silver).to_string(), "全");
        assert_eq!(Piece::new_promoted(PromotedPiece::Knight).to_string(), "圭");
        assert_eq!(Piece::new_promoted(PromotedPiece::Lance).to_string(), "杏");
        assert_eq!(Piece::new_promoted(PromotedPiece::Pawn).to_string(), "と");
    }

    #[test]
    #[rustfmt::skip]
    fn piece_has_promoted() {
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::King).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Rook).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Bishop).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Gold).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Silver).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Knight).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Lance).has_promoted(), false);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Pawn).has_promoted(), false);
        assert_eq!(Piece::new_promoted(PromotedPiece::Rook).has_promoted(), true);
        assert_eq!(Piece::new_promoted(PromotedPiece::Bishop).has_promoted(), true);
        assert_eq!(Piece::new_promoted(PromotedPiece::Silver).has_promoted(), true);
        assert_eq!(Piece::new_promoted(PromotedPiece::Knight).has_promoted(), true);
        assert_eq!(Piece::new_promoted(PromotedPiece::Lance).has_promoted(), true);
        assert_eq!(Piece::new_promoted(PromotedPiece::Pawn).has_promoted(), true);
    }

    #[test]
    #[rustfmt::skip]
    fn piece_has_unpromoted() {
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::King).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Rook).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Bishop).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Gold).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Silver).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Knight).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Lance).has_unpromoted(), true);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Pawn).has_unpromoted(), true);
        assert_eq!(Piece::new_promoted(PromotedPiece::Rook).has_unpromoted(), false);
        assert_eq!(Piece::new_promoted(PromotedPiece::Bishop).has_unpromoted(), false);
        assert_eq!(Piece::new_promoted(PromotedPiece::Silver).has_unpromoted(), false);
        assert_eq!(Piece::new_promoted(PromotedPiece::Knight).has_unpromoted(), false);
        assert_eq!(Piece::new_promoted(PromotedPiece::Lance).has_unpromoted(), false);
        assert_eq!(Piece::new_promoted(PromotedPiece::Pawn).has_unpromoted(), false);
    }

    #[test]
    #[rustfmt::skip]
    fn piece_promote() {
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::King).promote(), None);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Rook).promote(), Some(Piece::new_promoted(PromotedPiece::Rook)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Bishop).promote(), Some(Piece::new_promoted(PromotedPiece::Bishop)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Gold).promote(), None);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Silver).promote(), Some(Piece::new_promoted(PromotedPiece::Silver)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Knight).promote(), Some(Piece::new_promoted(PromotedPiece::Knight)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Lance).promote(), Some(Piece::new_promoted(PromotedPiece::Lance)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Pawn).promote(), Some(Piece::new_promoted(PromotedPiece::Pawn)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Rook).promote(), Some(Piece::new_promoted(PromotedPiece::Rook)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Bishop).promote(), Some(Piece::new_promoted(PromotedPiece::Bishop)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Silver).promote(), Some(Piece::new_promoted(PromotedPiece::Silver)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Knight).promote(), Some(Piece::new_promoted(PromotedPiece::Knight)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Lance).promote(), Some(Piece::new_promoted(PromotedPiece::Lance)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Pawn).promote(), Some(Piece::new_promoted(PromotedPiece::Pawn)));
    }

    #[test]
    #[rustfmt::skip]
    fn piece_unpromote() {
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::King).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::King)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Rook).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Rook)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Bishop).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Bishop)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Gold).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Gold)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Silver).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Silver)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Knight).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Knight)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Lance).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Lance)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Pawn).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Pawn)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Rook).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Rook)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Bishop).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Bishop)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Silver).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Silver)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Knight).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Knight)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Lance).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Lance)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Pawn).unpromote(), Some(Piece::new_unpromoted(UnpromotedPiece::Pawn)));
    }

    #[test]
    #[rustfmt::skip]
    fn piece_flip() {
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::King).flip(), None);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Rook).flip(), Some(Piece::new_promoted(PromotedPiece::Rook)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Bishop).flip(), Some(Piece::new_promoted(PromotedPiece::Bishop)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Gold).flip(), None);
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Silver).flip(), Some(Piece::new_promoted(PromotedPiece::Silver)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Knight).flip(), Some(Piece::new_promoted(PromotedPiece::Knight)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Lance).flip(), Some(Piece::new_promoted(PromotedPiece::Lance)));
        assert_eq!(Piece::new_unpromoted(UnpromotedPiece::Pawn).flip(), Some(Piece::new_promoted(PromotedPiece::Pawn)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Rook).flip(), Some(Piece::new_unpromoted(UnpromotedPiece::Rook)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Bishop).flip(), Some(Piece::new_unpromoted(UnpromotedPiece::Bishop)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Silver).flip(), Some(Piece::new_unpromoted(UnpromotedPiece::Silver)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Knight).flip(), Some(Piece::new_unpromoted(UnpromotedPiece::Knight)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Lance).flip(), Some(Piece::new_unpromoted(UnpromotedPiece::Lance)));
        assert_eq!(Piece::new_promoted(PromotedPiece::Pawn).flip(), Some(Piece::new_unpromoted(UnpromotedPiece::Pawn)));
    }

    #[test]
    fn piece_from() {
        assert_eq!(
            Piece::from(UnpromotedPiece::Pawn),
            Piece::Unpromoted(UnpromotedPiece::Pawn)
        );

        assert_eq!(
            Piece::from(PromotedPiece::Pawn),
            Piece::Promoted(PromotedPiece::Pawn)
        );

        assert_eq!(
            Piece::from(PieceWithColor::new_black(UnpromotedPiece::Bishop)),
            Piece::Unpromoted(UnpromotedPiece::Bishop)
        );
    }

    #[test]
    fn piece_with_color_new() {
        assert_eq!(
            PieceWithColor::new(UnpromotedPiece::Pawn, Color::Black),
            PieceWithColor {
                piece: Piece::Unpromoted(UnpromotedPiece::Pawn),
                color: Color::Black
            }
        );

        assert_eq!(
            PieceWithColor::new(UnpromotedPiece::Pawn, Color::White),
            PieceWithColor {
                piece: Piece::Unpromoted(UnpromotedPiece::Pawn),
                color: Color::White
            }
        );
    }

    #[test]
    fn piece_with_color_new_black() {
        assert_eq!(
            PieceWithColor::new_black(UnpromotedPiece::Pawn),
            PieceWithColor {
                piece: Piece::Unpromoted(UnpromotedPiece::Pawn),
                color: Color::Black
            }
        );
    }

    #[test]
    fn piece_with_color_new_white() {
        assert_eq!(
            PieceWithColor::new_white(UnpromotedPiece::Pawn),
            PieceWithColor {
                piece: Piece::Unpromoted(UnpromotedPiece::Pawn),
                color: Color::White
            }
        );
    }

    #[test]
    fn piece_with_color_color() {
        let piece = PieceWithColor::new_white(UnpromotedPiece::Pawn);
        assert_eq!(piece.color(), &Color::White);

        let piece = PieceWithColor::new_black(UnpromotedPiece::Pawn);
        assert_eq!(piece.color(), &Color::Black);
    }

    #[test]
    fn piece_with_color_color_mut() {
        let mut piece = PieceWithColor::new_white(UnpromotedPiece::Pawn);
        assert_eq!(piece.color_mut(), &Color::White);
        *piece.color_mut() = Color::Black;
        assert_eq!(piece.color(), &Color::Black);

        let mut piece = PieceWithColor::new_black(UnpromotedPiece::Pawn);
        assert_eq!(piece.color(), &Color::Black);
        *piece.color_mut() = Color::White;
        assert_eq!(piece.color(), &Color::White);
    }

    #[test]
    fn piece_with_color_set_color() {
        let mut piece = PieceWithColor::new_white(UnpromotedPiece::Pawn);
        assert_eq!(piece.color_mut(), &Color::White);
        piece.set_color(Color::Black);
        assert_eq!(piece.color(), &Color::Black);

        let mut piece = PieceWithColor::new_black(UnpromotedPiece::Pawn);
        assert_eq!(piece.color(), &Color::Black);
        piece.set_color(Color::White);
        assert_eq!(piece.color(), &Color::White);
    }

    #[test]
    fn piece_with_color_piece() {
        let piece = PieceWithColor::new_white(UnpromotedPiece::Pawn);
        assert_eq!(piece.piece(), &Piece::Unpromoted(UnpromotedPiece::Pawn));

        let piece = PieceWithColor::new_black(PromotedPiece::Pawn);
        assert_eq!(piece.piece(), &Piece::Promoted(PromotedPiece::Pawn));
    }

    #[test]
    fn piece_with_color_piece_mut() {
        let mut piece = PieceWithColor::new_white(UnpromotedPiece::Pawn);
        assert_eq!(piece.piece_mut(), &Piece::Unpromoted(UnpromotedPiece::Pawn));
        *piece.piece_mut() = PromotedPiece::Bishop.into();
        assert_eq!(piece.piece(), &Piece::Promoted(PromotedPiece::Bishop));

        let mut piece = PieceWithColor::new_black(PromotedPiece::Pawn);
        assert_eq!(piece.piece_mut(), &Piece::Promoted(PromotedPiece::Pawn));
        *piece.piece_mut() = UnpromotedPiece::Bishop.into();
        assert_eq!(piece.piece(), &Piece::Unpromoted(UnpromotedPiece::Bishop));
    }

    #[test]
    fn piece_with_color_set_piece() {
        let mut piece = PieceWithColor::new_white(UnpromotedPiece::Pawn);
        assert_eq!(piece.piece(), &Piece::Unpromoted(UnpromotedPiece::Pawn));
        piece.set_piece(PromotedPiece::Bishop);
        assert_eq!(piece.piece(), &Piece::Promoted(PromotedPiece::Bishop));

        let mut piece = PieceWithColor::new_black(PromotedPiece::Pawn);
        assert_eq!(piece.piece(), &Piece::Promoted(PromotedPiece::Pawn));
        piece.set_piece(UnpromotedPiece::Bishop);
        assert_eq!(piece.piece(), &Piece::Unpromoted(UnpromotedPiece::Bishop));
    }

    #[test]
    fn piece_kind_new() {
        let kind = PieceKind::new(PromotedPiece::Pawn);
        assert_eq!(
            kind,
            PieceKind::WithoutColor(Piece::Promoted(PromotedPiece::Pawn))
        );

        let kind = PieceKind::new(PieceWithColor::new(PromotedPiece::Pawn, Color::Black));
        assert_eq!(
            kind,
            PieceKind::WithColor(PieceWithColor {
                color: Color::Black,
                piece: PromotedPiece::Pawn.into()
            })
        );
    }

    #[test]
    fn piece_kind_new_white() {
        let kind = PieceKind::new_white(PromotedPiece::Pawn);
        assert_eq!(
            kind,
            PieceKind::WithColor(PieceWithColor {
                color: Color::White,
                piece: PromotedPiece::Pawn.into()
            })
        );
    }

    #[test]
    fn piece_kind_new_black() {
        let kind = PieceKind::new_black(PromotedPiece::Pawn);
        assert_eq!(
            kind,
            PieceKind::WithColor(PieceWithColor {
                color: Color::Black,
                piece: PromotedPiece::Pawn.into()
            })
        );
    }

    #[test]
    fn piece_kind_from() {
        assert_eq!(
            PieceKind::from(Piece::new_promoted(PromotedPiece::Pawn)),
            PieceKind::WithoutColor(Piece::Promoted(PromotedPiece::Pawn))
        );

        assert_eq!(
            PieceKind::from(PieceWithColor::new_black(PromotedPiece::Pawn)),
            PieceKind::WithColor(PieceWithColor {
                color: Color::Black,
                piece: PromotedPiece::Pawn.into()
            })
        );

        assert_eq!(
            PieceKind::from(UnpromotedPiece::Pawn),
            PieceKind::WithoutColor(Piece::Unpromoted(UnpromotedPiece::Pawn))
        );

        assert_eq!(
            PieceKind::from(PromotedPiece::Pawn),
            PieceKind::WithoutColor(Piece::Promoted(PromotedPiece::Pawn))
        );
    }
}
