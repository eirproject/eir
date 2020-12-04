use std::borrow::Borrow;
use std::collections::hash_map::RandomState;
use std::fmt::Debug;
use std::hash::{BuildHasher, Hasher};
use std::marker::PhantomData;

use crate::aux_traits::{AuxEq, AuxHash};

//use cranelift_entity::{ListPool, EntityList, EntityRef};
//use cranelift_entity::packed_option::ReservedValue;

use hashbrown::raw::{AllocRef, Global, RawIter, RawTable};

//impl<C, T> AuxHash<C> for T where T: Hash {
//    fn aux_hash<H: Hasher>(&self, state: &mut H, _container: &C) {
//        self.hash(state)
//    }
//}
//impl<E> AuxHash<ListPool<E>> for EntityList<E>
//where
//    E: EntityRef + ReservedValue + Hash
//{
//    fn aux_hash<H: Hasher>(&self, state: &mut H, container: &ListPool<E>) {
//        self.as_slice(container).hash(state)
//    }
//}

//impl<C, T: ?Sized> AuxEq<C> for T where T: Eq {
//    fn aux_eq(&self, other: &Self, _container: &C) -> bool {
//        self.eq(other)
//    }
//    fn aux_ne(&self, other: &Self, _container: &C) -> bool {
//        self.ne(other)
//    }
//}
//impl<E> AuxEq<ListPool<E>> for EntityList<E>
//where
//    E: EntityRef + ReservedValue + Eq
//{
//    fn aux_eq(&self, other: &Self, container: &ListPool<E>) -> bool {
//        self.as_slice(container) == other.as_slice(container)
//    }
//}

/// Implementation of a HashMap where the hash/equality functions
/// requires additional information.
#[derive(Clone)]
pub struct AuxHashMap<K, V, C, S = RandomState, A = Global>
where
    A: Clone + AllocRef,
{
    hash_builder: S,
    table: RawTable<(K, V), A>,
    aux: PhantomData<C>,
}

impl<K, V, C> AuxHashMap<K, V, C, RandomState> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<K, V, C, S> AuxHashMap<K, V, C, S> {
    #[inline]
    pub fn with_hasher(hash_builder: S) -> Self {
        Self {
            hash_builder,
            table: RawTable::new_in(Global),
            aux: PhantomData,
        }
    }

    #[inline]
    pub fn iter(&self) -> Iter<'_, K, V> {
        unsafe {
            Iter {
                inner: self.table.iter(),
                marker: PhantomData,
            }
        }
    }
}

#[inline]
fn make_hash<C, K: AuxHash<C> + ?Sized>(
    hash_builder: &impl BuildHasher,
    container: &C,
    val: &K,
) -> u64 {
    let mut state = hash_builder.build_hasher();
    val.aux_hash(&mut state, container);
    state.finish()
}

impl<K, V, C, S> AuxHashMap<K, V, C, S>
where
    K: AuxEq<C> + AuxHash<C>,
    S: BuildHasher,
{
    #[inline]
    pub fn try_insert(&mut self, k: K, v: V, container: &C) -> Result<(), ()> {
        let hash = make_hash(&self.hash_builder, container, &k);
        if let Some(_item) = self
            .table
            .find(hash, |x| k.aux_eq(&x.0, container, container))
        {
            Err(())
        } else {
            let hash_builder = &self.hash_builder;
            self.table
                .insert(hash, (k, v), |x| make_hash(hash_builder, container, &x.0));
            Ok(())
        }
    }

    #[inline]
    pub fn get<Q: ?Sized>(&self, k: &Q, container: &C) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: AuxHash<C> + AuxEq<C>,
    {
        self.get_key_value(k, container).map(|(_, v)| v)
    }

    #[inline]
    pub fn get_key_value<Q: ?Sized>(&self, k: &Q, container: &C) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: AuxHash<C> + AuxEq<C>,
    {
        let hash = make_hash(&self.hash_builder, container, k);
        self.table
            .find(hash, |x| k.aux_eq(x.0.borrow(), container, container))
            .map(|item| unsafe {
                let &(ref key, ref value) = item.as_ref();
                (key, value)
            })
    }
}

impl<K, V, C, S> Default for AuxHashMap<K, V, C, S>
where
    S: BuildHasher + Default,
{
    #[inline]
    fn default() -> Self {
        Self::with_hasher(Default::default())
    }
}

impl<K, V, C, S> Debug for AuxHashMap<K, V, C, S>
where
    K: Debug,
    V: Debug,
    S: BuildHasher,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

#[derive(Clone)]
pub struct Iter<'a, K, V> {
    inner: RawIter<(K, V)>,
    marker: PhantomData<(&'a K, &'a V)>,
}
impl<'a, K, V> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    #[inline]
    fn next(&mut self) -> Option<(&'a K, &'a V)> {
        self.inner.next().map(|x| unsafe {
            let r = x.as_ref();
            (&r.0, &r.1)
        })
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

#[cfg(test)]
mod tests {

    //use super::{ AuxHashMap, AuxHash };
    //use std::hash::{ Hash, Hasher };

    //#[test]
    //fn test_deduped_entity() {

    //    #[derive(PartialEq, Eq)]
    //    struct Key(usize);
    //    impl AuxHash<AuxData> for Key {
    //        fn aux_hash<H: Hasher>(&self, state: &mut H, container: &AuxData) {
    //            assert!(container.0 == 11);
    //            self.0.hash(state)
    //        }
    //    }

    //    struct AuxData(usize);

    //    let mut map: AuxHashMap<Key, usize, AuxData> = AuxHashMap::new();
    //    let aux = AuxData(11);

    //    map.try_insert(Key(1), 1, &aux).unwrap();
    //    map.try_insert(Key(1), 1, &aux).err().unwrap();

    //    map.try_insert(Key(2), 2, &aux).unwrap();
    //    map.try_insert(Key(2), 2, &aux).err().unwrap();

    //    map.try_insert(Key(1), 1, &aux).err().unwrap();

    //    assert!(map.get(&Key(1), &aux) == Some(&1));
    //    assert!(map.get(&Key(2), &aux) == Some(&2));

    //}
}
