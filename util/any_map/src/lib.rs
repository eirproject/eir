#![feature(allocator_api)]

mod any_map;
pub use crate::any_map::{AnyMap, DefaultBuildHasher};

mod any_any_map;
pub use crate::any_any_map::{AnyAnyMap, AnyKey};
