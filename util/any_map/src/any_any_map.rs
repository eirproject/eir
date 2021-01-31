use crate::{AnyMap, DefaultBuildHasher};
use std::alloc::{Allocator, Global};
use std::any::Any;
use std::hash::{BuildHasher, Hash};

pub trait AnyKey: Clone + Hash + Eq + 'static {
    type Value: Clone + 'static;
}

trait AnyValueTrait {
    fn clone(&self) -> Box<dyn AnyValueTrait>;
}
impl<T> AnyValueTrait for T
where
    T: Clone + Any,
{
    fn clone(&self) -> Box<dyn AnyValueTrait> {
        Box::new(Clone::clone(self))
    }
}

pub struct AnyAnyMap<S = DefaultBuildHasher, A = Global>
where
    A: Allocator + Clone,
{
    inner: AnyMap<Box<dyn Any>, S, A>,
}

impl<S, A> Default for AnyAnyMap<S, A>
where
    S: Default,
    A: Default + Allocator + Clone,
{
    fn default() -> Self {
        AnyAnyMap {
            inner: Default::default(),
        }
    }
}

impl AnyAnyMap {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<S, A> AnyAnyMap<S, A>
where
    A: Allocator + Clone,
    S: BuildHasher,
{
    pub fn insert<K: AnyKey>(&mut self, k: K, v: K::Value) -> Option<K::Value>
    where
        K: AnyKey,
    {
        let ret = self.inner.insert(k, Box::new(v));
        ret.map(|v| *v.downcast().unwrap())
    }

    pub fn get<K: AnyKey>(&self, k: &K) -> Option<&K::Value> {
        self.inner.get(k).map(|v| v.downcast_ref().unwrap())
    }

    pub fn get_mut<K: AnyKey>(&mut self, k: &K) -> Option<&mut K::Value> {
        self.inner.get_mut(k).map(|v| v.downcast_mut().unwrap())
    }
}
