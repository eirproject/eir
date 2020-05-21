use std::marker::PhantomData;
use std::hash::{Hash, Hasher};
use std::fmt::{Debug, Formatter};

use cranelift_entity::EntityRef;
use cranelift_entity::ListPool;
use cranelift_entity::EntityList;
use cranelift_entity::packed_option::ReservedValue;

use crate::aux_traits::{HasAux, AuxDebug, AuxHash, AuxEq};

#[derive(Debug, Clone)]
pub struct EntitySetPool<E> {
    typ: PhantomData<E>,
    pool: ListPool<PooledSetValue>,
}
impl<E> EntitySetPool<E> {

    pub fn new() -> Self {
        EntitySetPool {
            typ: PhantomData,
            pool: ListPool::new(),
        }
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct PooledSetValue(u64);

impl ReservedValue for PooledSetValue {
    fn reserved_value() -> Self {
        PooledSetValue(std::u64::MAX)
    }
}

impl EntityRef for PooledSetValue {
    fn index(self) -> usize {
        self.0 as usize
    }
    fn new(n: usize) -> Self {
        debug_assert!(n < std::usize::MAX);
        PooledSetValue(n as u64)
    }
}

#[derive(Debug, Clone)]
pub struct EntitySet<K>
where
    K: EntityRef
{
    list: EntityList<PooledSetValue>,
    unused: PhantomData<K>,
}

impl<C: HasAux<EntitySetPool<K>>, K: EntityRef + Debug> AuxDebug<C> for EntitySet<K> {
    fn aux_fmt(&self, f: &mut std::fmt::Formatter, aux: &C) -> std::fmt::Result {
        let mut b = f.debug_set();
        b.entries(self.iter(aux.get_aux()));
        b.finish()
    }
}
impl<C: HasAux<EntitySetPool<K>>, K: EntityRef> AuxHash<C> for EntitySet<K> {
    fn aux_hash<H: Hasher>(&self, hasher: &mut H, aux: &C) {
        let mut n = 0;
        for key in self.iter(aux.get_aux()) {
            key.index().hash(hasher);
            n += 1;
        }
        n.hash(hasher);
    }
}
impl<C: HasAux<EntitySetPool<K>>, K: EntityRef> AuxEq<C> for EntitySet<K> {
    fn aux_eq(&self, other: &Self, self_aux: &C, other_aux: &C) -> bool {
        self.eq_other(other, self_aux.get_aux(), other_aux.get_aux())
    }
}

impl<T: EntityRef> Default for EntitySet<T> {
    fn default() -> Self {
        Self {
            list: EntityList::new(),
            unused: PhantomData,
        }
    }
}

impl<K> EntitySet<K>
where
    K: EntityRef
{

    pub fn new() -> Self {
        Self {
            list: EntityList::new(),
            unused: PhantomData,
        }
    }

    pub fn contains(&self, k: K, pool: &EntitySetPool<K>) -> bool {
        let index = k.index();

        let list_len = self.list.len(&pool.pool);
        let list_len_entries = list_len * 63;

        if index < list_len_entries {
            let list_val = self.list.get(index / 63, &pool.pool).unwrap();
            list_val.0 & (1 << (index % 63)) != 0
        } else {
            false
        }
    }

    pub fn size(&self, pool: &EntitySetPool<K>) -> usize {
        let mut len = 0;
        for n in self.list.as_slice(&pool.pool) {
            debug_assert!(n.0 != std::u64::MAX);
            len += n.0.count_ones() as usize;
        }
        len
    }

    pub fn clear(&mut self, pool: &mut EntitySetPool<K>) {
        self.list.clear(&mut pool.pool)
    }

    //pub fn keys(&self) -> Keys<K>

    fn grow_to_pages(&mut self, pages: usize, pool: &mut EntitySetPool<K>) {
        let len = self.list.len(&pool.pool);
        if len < pages {
            // TODO: grow in one go
            for _ in 0..(pages - len) {
                self.list.push(PooledSetValue(0), &mut pool.pool);
            }
        }
    }

    pub fn insert(&mut self, k: K, pool: &mut EntitySetPool<K>) -> bool {
        let index = k.index();

        let list_len = self.list.len(&pool.pool);
        let needed_list_len = (index / 63) + 1; //(index + 62) / 63;

        if list_len < needed_list_len {
            self.grow_to_pages(needed_list_len, pool);
        }

        let result = self.contains(k, pool);
        let val = self.list.get_mut(index / 63, &mut pool.pool).unwrap();

        val.0 |= 1 << (index % 63);

        result
    }

    pub fn remove(&mut self, k: K, pool: &mut EntitySetPool<K>) -> bool {
        let index = k.index();

        let list_len = self.list.len(&mut pool.pool);
        let list_len_entries = list_len * 63;

        if index < list_len_entries {
            let list_val = self.list.get_mut(index / 63, &mut pool.pool).unwrap();
            let bit_idx = 1 << (index % 63);
            let ret = list_val.0 & bit_idx != 0;
            list_val.0 &= !bit_idx;
            ret
        } else {
            false
        }
    }

    pub fn make_copy(&self, pool: &mut EntitySetPool<K>) -> EntitySet<K> {
        let mut new_list = EntityList::new();

        let len = self.list.len(&pool.pool);
        for n in 0..len {
            let item = self.list.get(n, &pool.pool).unwrap();
            new_list.push(item, &mut pool.pool);
        }

        EntitySet {
            list: new_list,
            unused: PhantomData,
        }
    }

    pub fn union(&mut self, other: &EntitySet<K>,
                 pool: &mut EntitySetPool<K>) {
        let other_list = &other.list;
        let other_pages_len = other_list.len(&pool.pool);
        self.grow_to_pages(other_pages_len, pool);
        for n in 0..other_pages_len {
            let other_val = other_list.get(n, &pool.pool).unwrap();
            let val_mut = self.list.get_mut(n, &mut pool.pool).unwrap();
            debug_assert!(val_mut.0 != std::u64::MAX);
            debug_assert!(other_val.0 != std::u64::MAX);
            val_mut.0 |= other_val.0;
        }
    }

    pub fn iter<'a>(&self, pool: &'a EntitySetPool<K>) -> EntitySetIter<'a, K> {
        EntitySetIter {
            pool: &pool.pool,
            list: self.list.clone(),
            unused: PhantomData,
            current_data: None,
            current: 0,
            finished: false,
        }
    }

    pub fn eq(&self, other: &EntitySet<K>, pool: &EntitySetPool<K>) -> bool {
        self.eq_other(other, pool, pool)
    }

    pub fn eq_other(&self, other: &EntitySet<K>, pool: &EntitySetPool<K>, other_pool: &EntitySetPool<K>) -> bool {
        let self_list = self.list.as_slice(&pool.pool);
        let other_list = other.list.as_slice(&other_pool.pool);

        // Check common head
        for (a, b) in self_list.iter().zip(other_list) {
            debug_assert!(a.0 != std::u64::MAX);
            debug_assert!(b.0 != std::u64::MAX);
            if a != b {
                return false;
            }
        }

        // If the length differs, validate that the tail is empty
        match (self_list.len(), other_list.len()) {
            (a, b) if a > b => {
                for n in &self_list[b..] {
                    debug_assert!(n.0 != std::u64::MAX);
                    if n.0 != 0 {
                        return false;
                    }
                }
            }
            (a, b) if a < b => {
                for n in &other_list[a..] {
                    debug_assert!(n.0 != std::u64::MAX);
                    if n.0 != 0 {
                        return false;
                    }
                }
            }
            _ => (),
        }

        true
    }

    pub fn bind<'a>(&self, pool: &'a EntitySetPool<K>) -> BoundEntitySet<'a, K> {
        BoundEntitySet {
            set: self.clone(),
            pool,
        }
    }

}

#[derive(Clone)]
pub struct BoundEntitySet<'a, K: EntityRef> {
    set: EntitySet<K>,
    pool: &'a EntitySetPool<K>,
}
impl<'a, K: EntityRef> BoundEntitySet<'a, K> {

    pub fn contains(&self, k: K) -> bool {
        self.set.contains(k, self.pool)
    }

    pub fn size(&self) -> usize {
        self.set.size(self.pool)
    }

    pub fn iter(&self) -> EntitySetIter<'_, K> {
        self.set.iter(self.pool)
    }

}
impl<'a, K: EntityRef> IntoIterator for &'a BoundEntitySet<'a, K> {
    type Item = K;
    type IntoIter = EntitySetIter<'a, K>;
    fn into_iter(self) -> EntitySetIter<'a, K> {
        BoundEntitySet::iter(self)
    }
}
impl<'a, K> Debug for BoundEntitySet<'a, K> where K: EntityRef + Debug {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_set().entries(self.set.iter(self.pool)).finish()
    }
}
impl<'a, K: EntityRef> PartialEq for BoundEntitySet<'a, K> {
    fn eq(&self, rhs: &BoundEntitySet<K>) -> bool {
        self.set.eq_other(&rhs.set, self.pool, &rhs.pool)
    }
}
impl<'a, K: EntityRef> Eq for BoundEntitySet<'a, K> {}

pub struct EntitySetIter<'a, T> where T: EntityRef {
    pool: &'a ListPool<PooledSetValue>,
    list: EntityList<PooledSetValue>,
    unused: PhantomData<T>,
    current_data: Option<PooledSetValue>,
    current: usize,
    finished: bool,
}
impl<'a, T> Iterator for EntitySetIter<'a, T> where T: EntityRef {
    type Item = T;
    #[inline]
    fn next(&mut self) -> Option<T> {
        loop {
            if self.finished { return None; }
            let rem = self.current % 63;
            if rem == 0 {
                self.current_data = self.list.get(self.current / 63, self.pool);
                if let Some(current_data) = self.current_data {
                    if current_data.0 == 0 {
                        self.current += 63;
                        continue;
                    }
                } else {
                    self.finished = true;
                    return None;
                }
            }

            if (self.current_data.unwrap().0 & (1 << rem)) != 0 {
                let ret = Some(T::new(self.current));
                self.current += 1;
                return ret;
            } else {
                self.current += 1;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::EntitySetPool;
    use super::EntitySet;

    use ::cranelift_entity::entity_impl;

    /// Basic block in function
    #[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    #[test]
    fn test_pooled_set() {
        let mut pool = EntitySetPool::new();

        let mut set1: EntitySet<TestEntity> = EntitySet::new();

        assert!(!set1.contains(TestEntity(0), &mut pool));
        assert!(!set1.contains(TestEntity(1), &mut pool));
        assert!(!set1.contains(TestEntity(2), &mut pool));
        assert!(!set1.contains(TestEntity(9999), &mut pool));

        set1.insert(TestEntity(2), &mut pool);
        assert!(!set1.contains(TestEntity(0), &mut pool));
        assert!(!set1.contains(TestEntity(1), &mut pool));
        assert!(set1.contains(TestEntity(2), &mut pool));
        assert!(!set1.contains(TestEntity(3), &mut pool));

        set1.insert(TestEntity(62), &mut pool);
        set1.insert(TestEntity(63), &mut pool);

        set1.insert(TestEntity(200), &mut pool);
        assert!(!set1.contains(TestEntity(0), &mut pool));
        assert!(set1.contains(TestEntity(2), &mut pool));
        assert!(!set1.contains(TestEntity(199), &mut pool));
        assert!(set1.contains(TestEntity(200), &mut pool));
        assert!(!set1.contains(TestEntity(201), &mut pool));

        set1.insert(TestEntity(10), &mut pool);
        assert!(set1.contains(TestEntity(200), &mut pool));

        set1.remove(TestEntity(200), &mut pool);
        assert!(!set1.contains(TestEntity(200), &mut pool));
        assert!(set1.contains(TestEntity(10), &mut pool));
        assert!(!set1.contains(TestEntity(9), &mut pool));
        assert!(set1.contains(TestEntity(2), &mut pool));
        assert!(!set1.contains(TestEntity(200), &mut pool));

        set1.remove(TestEntity(2), &mut pool);
        assert!(!set1.contains(TestEntity(2), &mut pool));
        assert!(set1.contains(TestEntity(10), &mut pool));

        let mut set2: EntitySet<TestEntity> = EntitySet::new();

        for i in 2..200 {
            println!("{}", i);
            assert!(!set2.contains(TestEntity(i-1), &mut pool));
            assert!(!set2.contains(TestEntity(i), &mut pool));
            assert!(!set2.contains(TestEntity(i+1), &mut pool));
            set2.insert(TestEntity(i), &mut pool);
            assert!(!set2.contains(TestEntity(i-1), &mut pool));
            assert!(set2.contains(TestEntity(i), &mut pool));
            assert!(!set2.contains(TestEntity(i+1), &mut pool));
            set2.remove(TestEntity(i), &mut pool);
            assert!(!set2.contains(TestEntity(i-1), &mut pool));
            assert!(!set2.contains(TestEntity(i), &mut pool));
            assert!(!set2.contains(TestEntity(i+1), &mut pool));
        }

    }

    #[test]
    fn test_pooled_set_remove() {
        let mut pool = EntitySetPool::new();

        let mut set1: EntitySet<TestEntity> = EntitySet::new();

        assert!(!set1.remove(TestEntity(2), &mut pool));
        assert!(!set1.remove(TestEntity(100), &mut pool));
        set1.insert(TestEntity(2), &mut pool);
        assert!(set1.remove(TestEntity(2), &mut pool));
        assert!(!set1.remove(TestEntity(2), &mut pool));
        set1.insert(TestEntity(100), &mut pool);
        assert!(!set1.remove(TestEntity(2), &mut pool));
        assert!(set1.remove(TestEntity(100), &mut pool));
        assert!(!set1.remove(TestEntity(99), &mut pool));
        assert!(!set1.remove(TestEntity(2), &mut pool));
    }

    #[test]
    fn test_pooled_set_eq() {
        let mut pool = EntitySetPool::new();

        let mut set1: EntitySet<TestEntity> = EntitySet::new();
        let mut set2: EntitySet<TestEntity> = EntitySet::new();

        assert!(set1.eq(&set2, &pool));

        set1.insert(TestEntity(2), &mut pool);
        assert!(!set1.eq(&set2, &pool));
        assert!(!set2.eq(&set1, &pool));

        set2.insert(TestEntity(2), &mut pool);
        assert!(set1.eq(&set2, &pool));
        assert!(set2.eq(&set1, &pool));

        set2.insert(TestEntity(100), &mut pool);
        assert!(!set1.eq(&set2, &pool));
        assert!(!set2.eq(&set1, &pool));

        set1.insert(TestEntity(100), &mut pool);
        assert!(set1.eq(&set2, &pool));
        assert!(set2.eq(&set1, &pool));

    }


    #[test]
    fn test_iterator() {
        let mut pool = EntitySetPool::new();
        let mut set1: EntitySet<TestEntity> = EntitySet::new();

        set1.insert(TestEntity(5), &mut pool);
        set1.insert(TestEntity(62), &mut pool);
        set1.insert(TestEntity(63), &mut pool);
        set1.insert(TestEntity(64), &mut pool);
        set1.insert(TestEntity(500), &mut pool);
        set1.insert(TestEntity(501), &mut pool);
        set1.insert(TestEntity(502), &mut pool);
        set1.insert(TestEntity(503), &mut pool);
        set1.insert(TestEntity(504), &mut pool);
        set1.insert(TestEntity(505), &mut pool);
        set1.insert(TestEntity(506), &mut pool);
        set1.insert(TestEntity(507), &mut pool);
        set1.insert(TestEntity(508), &mut pool);
        set1.insert(TestEntity(509), &mut pool);
        set1.insert(TestEntity(510), &mut pool);
        set1.insert(TestEntity(511), &mut pool);
        set1.insert(TestEntity(512), &mut pool);

        let mut iter = set1.iter(&pool);
        assert!(iter.next() == Some(TestEntity(5)));
        assert!(iter.next() == Some(TestEntity(62)));
        assert!(iter.next() == Some(TestEntity(63)));
        assert!(iter.next() == Some(TestEntity(64)));
        assert!(iter.next() == Some(TestEntity(500)));
        assert!(iter.next() == Some(TestEntity(501)));
        assert!(iter.next() == Some(TestEntity(502)));
        assert!(iter.next() == Some(TestEntity(503)));
        assert!(iter.next() == Some(TestEntity(504)));
        assert!(iter.next() == Some(TestEntity(505)));
        assert!(iter.next() == Some(TestEntity(506)));
        assert!(iter.next() == Some(TestEntity(507)));
        assert!(iter.next() == Some(TestEntity(508)));
        assert!(iter.next() == Some(TestEntity(509)));
        assert!(iter.next() == Some(TestEntity(510)));
        assert!(iter.next() == Some(TestEntity(511)));
        assert!(iter.next() == Some(TestEntity(512)));
        assert!(iter.next() == None);
        assert!(iter.next() == None);
    }


}
