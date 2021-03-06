use std::{convert::TryFrom, error::Error, fmt::Display};

use serde::{Deserialize, Serialize};

pub type SquareNumber = u8;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct SquareOutOfRangeError {
    name: String,
    value: SquareNumber,
}

impl SquareOutOfRangeError {
    #[inline]
    pub(crate) fn new(name: String, value: SquareNumber) -> Self {
        Self { name, value }
    }
}

impl Display for SquareOutOfRangeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}({}) must be between 1 and 9.",
            stringify!(SquareOutOfRangeError),
            self.name,
            self.value
        )
    }
}

impl Error for SquareOutOfRangeError {}

#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub struct Square {
    x: SquareNumber,
    y: SquareNumber,
}

impl Square {
    const Y1: &'static str = "一";
    const Y2: &'static str = "二";
    const Y3: &'static str = "三";
    const Y4: &'static str = "四";
    const Y5: &'static str = "五";
    const Y6: &'static str = "六";
    const Y7: &'static str = "七";
    const Y8: &'static str = "八";
    const Y9: &'static str = "九";

    pub fn new(x: SquareNumber, y: SquareNumber) -> Result<Square, SquareOutOfRangeError> {
        let x = match x {
            1..=9 => Ok(x),
            _ => Err(SquareOutOfRangeError::new("x".into(), x)),
        }?;

        let y = match y {
            1..=9 => Ok(y),
            _ => Err(SquareOutOfRangeError::new("y".into(), y)),
        }?;

        Ok(Square { x, y })
    }

    #[inline]
    pub fn new_unchecked(x: SquareNumber, y: SquareNumber) -> Square {
        Self::new(x, y).unwrap()
    }

    #[inline]
    pub fn x(&self) -> SquareNumber {
        self.x
    }

    #[inline]
    pub fn y(&self) -> SquareNumber {
        self.y
    }
}

impl Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = match self.x {
            1..=9 => self.x,
            _ => unreachable!(),
        };

        let y = match self.y {
            1 => Square::Y1,
            2 => Square::Y2,
            3 => Square::Y3,
            4 => Square::Y4,
            5 => Square::Y5,
            6 => Square::Y6,
            7 => Square::Y7,
            8 => Square::Y8,
            9 => Square::Y9,
            _ => unreachable!(),
        };

        write!(f, "{}{}", x, y)
    }
}

impl TryFrom<(SquareNumber, SquareNumber)> for Square {
    type Error = SquareOutOfRangeError;

    #[inline]
    fn try_from(value: (SquareNumber, SquareNumber)) -> Result<Self, Self::Error> {
        Square::new(value.0, value.1)
    }
}

pub trait ToSquare {
    #[inline]
    fn square(&self) -> Square {
        *self.square_ptr()
    }

    fn square_ptr(&self) -> &Square;
}

pub trait TryToSquare {
    #[inline]
    fn try_square(&self) -> Option<Square> {
        self.try_square_ptr().copied()
    }
    fn try_square_ptr(&self) -> Option<&Square>;
}

impl<T: ToSquare + ?Sized> TryToSquare for T {
    #[inline]
    fn try_square_ptr(&self) -> Option<&Square> {
        Some(self.square_ptr())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn square_new() {
        for x in 1..=9 {
            assert!(Square::new(x, x).is_ok());
        }

        assert_eq!(Square::new(0, 1), Err(SquareOutOfRangeError::new("x".into(), 0)));
        assert_eq!(Square::new(1, 0), Err(SquareOutOfRangeError::new("y".into(), 0)));
        assert_eq!(Square::new(10, 1), Err(SquareOutOfRangeError::new("x".into(), 10)));
        assert_eq!(Square::new(1, 10), Err(SquareOutOfRangeError::new("y".into(), 10)));
    }

    #[test]
    fn squarea_new_unchecked() {
        for x in 1..=9 {
            Square::new_unchecked(x, x);
        }
    }

    #[test]
    #[should_panic]
    fn squere_new_unckecked_error() {
        Square::new_unchecked(0, 0);
    }

    #[test]
    fn square_display() {
        assert_eq!(Square::new(1, 1).unwrap().to_string(), "1一");
        assert_eq!(Square::new(2, 2).unwrap().to_string(), "2二");
        assert_eq!(Square::new(3, 3).unwrap().to_string(), "3三");
        assert_eq!(Square::new(4, 4).unwrap().to_string(), "4四");
        assert_eq!(Square::new(5, 5).unwrap().to_string(), "5五");
        assert_eq!(Square::new(6, 6).unwrap().to_string(), "6六");
        assert_eq!(Square::new(7, 7).unwrap().to_string(), "7七");
        assert_eq!(Square::new(8, 8).unwrap().to_string(), "8八");
        assert_eq!(Square::new(9, 9).unwrap().to_string(), "9九");

        assert_eq!(Square::new(2, 1).unwrap().to_string(), "2一");
        assert_eq!(Square::new(1, 2).unwrap().to_string(), "1二");
        assert_eq!(Square::new(4, 2).unwrap().to_string(), "4二");
        assert_eq!(Square::new(2, 4).unwrap().to_string(), "2四");
        assert_eq!(Square::new(9, 1).unwrap().to_string(), "9一");
        assert_eq!(Square::new(1, 9).unwrap().to_string(), "1九");
    }
}
