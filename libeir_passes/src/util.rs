use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug)]
pub struct EdgeSet<T: Copy + Ord>(pub BTreeMap<T, T>);
impl<T: Copy + Ord> EdgeSet<T> {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
    pub fn insert(&mut self, key: T, val: T) {
        self.0.insert(key, val);
    }
    pub fn propagate_edges(&mut self) {
        let froms: Vec<_> = self.0.keys().cloned().collect();
        loop {
            let mut change = false;

            for from in froms.iter() {
                let first = *self.0.get(from).unwrap();
                if let Some(second) = self.0.get(&first).cloned() {
                    self.0.insert(*from, second);
                    change = true;
                }
            }

            if !change { break; }
        }
    }
}

pub struct Walker<T> {
    pub walked: BTreeSet<T>,
    pub to_walk: Vec<T>,
}
impl<T> Walker<T>
where
    T: Ord,
{
    pub fn new() -> Self {
        Walker::with(Vec::new())
    }
    pub fn with(to_walk: Vec<T>) -> Self {
        Self {
            walked: BTreeSet::new(),
            to_walk,
        }
    }
    pub fn clear(&mut self) {
        self.walked.clear();
        self.to_walk.clear();
    }
    pub fn put(&mut self, t: T) {
        if !self.walked.contains(&t) {
            self.to_walk.push(t);
        }
    }
    pub fn next(&mut self) -> Option<T> {
        while let Some(t) = self.to_walk.pop() {
            if self.walked.contains(&t) { continue; }
            return Some(t);
        }
        None
    }
}
