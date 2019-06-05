use std::collections::HashMap;
use std::marker::PhantomData;

pub struct PrimaryMap<K, V> {
    data: Vec<V>,
    key: PhantomData<K>,
}

impl<K: EntityRef, V> PrimaryMap<K, V> {

    pub fn new() -> Self {
        PrimaryMap {
            data: Vec::new(),
            key: PhantomData,
        }
    }

}

struct SecondaryMap<K, V> {
    data: HashMap<K, V>,
}

pub trait EntityRef {

    fn index(&self) -> usize;

}
