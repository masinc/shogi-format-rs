use serde::{Deserialize, Serialize};
use std::time::Duration;

use super::moves::Moves;

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub struct BranchMetadata {
    comment: Option<String>,
    total_time: Option<Duration>,
    byoyomi: Option<Duration>,
}

impl BranchMetadata {
    #[inline]
    pub fn builder() -> BranchMetadataBuilder {
        BranchMetadataBuilder::new()
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub struct Branch {
    metadata: BranchMetadata,
    moves: Moves,
    branches: Option<Vec<Branch>>,
}

impl Branch {
    pub fn builder() -> BranchBuilder<()> {
        BranchBuilder::new()
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct BranchMetadataBuilder {
    comment: Option<String>,
    byoyomi: Option<Duration>,
    total_time: Option<Duration>,
}

impl BranchMetadataBuilder {
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
    pub fn byoyomi(self, value: Duration) -> Self {
        Self {
            byoyomi: Some(value),
            ..self
        }
    }

    #[inline]
    pub fn total_time(self, value: Duration) -> Self {
        Self {
            total_time: Some(value),
            ..self
        }
    }

    #[inline]
    pub fn build(self) -> BranchMetadata {
        BranchMetadata {
            comment: self.comment,
            byoyomi: self.byoyomi,
            total_time: self.total_time,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BranchBuilder<TMoves> {
    metadata: Option<BranchMetadata>,
    moves: TMoves,
    branches: Option<Vec<Branch>>,
}

impl BranchBuilder<()> {
    #[inline]
    pub fn new() -> Self {
        BranchBuilder {
            metadata: None,
            moves: (),
            branches: None,
        }
    }

    #[inline]
    pub fn moves(self, value: Moves) -> BranchBuilder<Moves> {
        BranchBuilder {
            metadata: self.metadata,
            moves: value,
            branches: self.branches,
        }
    }
}

impl Default for BranchBuilder<()> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<TMoves> BranchBuilder<TMoves> {
    #[inline]
    pub fn metadata(self, value: BranchMetadata) -> Self {
        Self {
            metadata: Some(value),
            ..self
        }
    }

    #[inline]
    pub fn branch(self, value: Branch) -> Self {
        let branches = match self.branches {
            Some(mut branches) => {
                branches.push(value);
                branches
            }
            None => {
                vec![value]
            }
        };

        Self {
            branches: Some(branches),
            ..self
        }
    }

    #[inline]
    pub fn branches(self, value: Vec<Branch>) -> Self {
        let bracnhes = match self.branches {
            Some(mut branches) => {
                branches.extend(value);
                branches
            }
            None => value,
        };

        Self {
            branches: Some(bracnhes),
            ..self
        }
    }
}

impl BranchBuilder<Moves> {
    #[inline]
    pub fn build(self) -> Branch {
        Branch {
            moves: self.moves,
            metadata: self.metadata.unwrap_or_default(),
            branches: self.branches,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::color::Color;
    use super::*;

    #[test]
    fn branch_metadata_builder() {
        assert_eq!(BranchMetadata::builder(), BranchMetadataBuilder::new());
        assert_eq!(BranchMetadata::builder(), BranchMetadataBuilder::default());
    }

    #[test]
    fn branch_metadata_comment() {
        let builder = BranchMetadata::builder().comment("abc");
        assert_eq!(
            builder.build(),
            BranchMetadata {
                comment: Some("abc".into()),
                ..Default::default()
            }
        );
    }

    #[test]
    fn branch_metadata_total_time() {
        let builder = BranchMetadata::builder().total_time(Duration::from_secs(30 * 60));
        assert_eq!(
            builder.build(),
            BranchMetadata {
                total_time: Some(Duration::from_secs(30 * 60)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn bracnh_metadata_byoyomi() {
        let builder = BranchMetadata::builder().byoyomi(Duration::from_secs(60));
        assert_eq!(
            builder.build(),
            BranchMetadata {
                byoyomi: Some(Duration::from_secs(60)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn branch_builder() {
        assert_eq!(Branch::builder(), BranchBuilder::new());
        assert_eq!(Branch::builder(), BranchBuilder::default());
    }

    #[test]
    fn branch_move_action() {
        assert_eq!(
            Branch::builder()
                .moves(Moves::interruption(Color::White))
                .build(),
            Branch {
                moves: Moves::interruption(Color::White),
                branches: None,
                metadata: Default::default(),
            }
        );

        assert_eq!(
            Branch::builder()
                .moves(Moves::illegal_action(Color::White))
                .build(),
            Branch {
                moves: Moves::illegal_action(Color::White),
                branches: None,
                metadata: Default::default(),
            }
        );

        assert_eq!(
            Branch::builder()
                .moves(Moves::repentition_of_moves(Color::White))
                .build(),
            Branch {
                moves: Moves::repentition_of_moves(Color::White),
                branches: None,
                metadata: Default::default(),
            }
        );

        assert_eq!(
            Branch::builder()
                .moves(Moves::resignation(Color::White))
                .build(),
            Branch {
                moves: Moves::resignation(Color::White),
                branches: None,
                metadata: Default::default(),
            }
        );
    }

    #[test]
    fn branch_metadata() {
        let builder = Branch::builder()
            .metadata(
                BranchMetadata::builder()
                    .byoyomi(Duration::from_secs(30))
                    .comment("test comment")
                    .build(),
            )
            .moves(Moves::interruption(Color::White));

        assert_eq!(
            builder.build(),
            Branch {
                metadata: BranchMetadata {
                    comment: Some("test comment".into()),
                    total_time: None,
                    byoyomi: Some(Duration::from_secs(30)),
                },
                moves: Moves::interruption(Color::White),
                branches: None,
            }
        );
    }
}
