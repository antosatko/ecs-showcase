use std::{fmt::Display, ops::Range};

use bevy_ecs::component::Component;
use rand::random_range;

pub const BIG_NUMBER: usize = 10_000_000;
pub const ANYDATA_SIZE: usize = 64;

#[derive(Debug)]
pub struct Float2(pub f32, pub f32);

#[derive(Component)]
pub struct AnyData([u8; ANYDATA_SIZE]);

impl Display for AnyData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AnyData(...)")
    }
}

impl Default for AnyData {
    fn default() -> Self {
        Self([0; ANYDATA_SIZE])
    }
}

impl Float2 {
    pub fn random(range: Range<f32>) -> Self {
        Self(random_range(range.clone()), random_range(range))
    }
}
