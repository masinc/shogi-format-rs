use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub enum Color {
    Black,
    White,
}

impl Color {
    #[inline]
    pub fn is_black(&self) -> bool {
        self == &Color::Black
    }

    #[inline]
    pub fn is_white(&self) -> bool {
        self == &Color::White
    }

    #[inline]
    pub fn flip(&mut self) {
        *self = self.to_flip()
    }

    pub fn to_flip(&self) -> Self {
        match self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }

    pub fn to_symbol_string(&self) -> String {
        match self {
            Color::Black => "▲",
            Color::White => "△",
        }
        .to_owned()
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Color::Black => "先手",
            Color::White => "後手",
        };

        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn color_is_black() {
        assert!(Color::Black.is_black());
        assert!(!Color::White.is_black());
    }

    #[test]
    fn color_is_white() {
        assert!(!Color::Black.is_white());
        assert!(Color::White.is_white());
    }

    #[test]
    fn color_to_string() {
        assert_eq!(Color::Black.to_string(), "先手");
        assert_eq!(Color::White.to_string(), "後手");
    }

    #[test]
    fn color_to_symbol_string() {
        assert_eq!(Color::Black.to_symbol_string(), "▲");
        assert_eq!(Color::White.to_symbol_string(), "△");
    }

    #[test]
    fn color_flip() {
        let mut b = Color::Black;
        b.flip();

        assert_eq!(b, Color::White);

        let mut c = Color::White;
        c.flip();

        assert_eq!(c, Color::Black);
    }

    #[test]
    fn color_to_flip() {
        assert_eq!(Color::Black.to_flip(), Color::White);
        assert_eq!(Color::White.to_flip(), Color::Black);
    }
}
