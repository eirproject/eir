//! Convernient builder API for HTML tables.

use std::alloc::Alloc;

pub enum Cell<A: Alloc, T> {
    Terminal(T),
    Horizontal(Box<Cell>, Box<Cell>)
}
