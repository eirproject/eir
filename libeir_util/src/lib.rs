#![feature(core_intrinsics)]
#![feature(dropck_eyepatch)]
#![feature(raw_vec_internals)]
#![feature(test)]

extern crate alloc;

#[cfg(any(test, bench))]
extern crate test;

pub mod hashmap_stack;
pub mod graph;
pub mod map;
pub mod pooled_entity_set;
pub mod arena;

#[cfg(test)]
mod tests {
}
