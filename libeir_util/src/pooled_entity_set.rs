use std::marker::PhantomData;
use ::cranelift_entity::EntityRef;
use ::cranelift_entity::ListPool;
use ::cranelift_entity::EntityList;
use ::cranelift_entity::packed_option::ReservedValue;

pub type EntitySetPool = ListPool<PooledSetValue>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PooledSetValue(u64);

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
pub struct PooledEntitySet<K>
where
    K: EntityRef
{
    list: EntityList<PooledSetValue>,
    unused: PhantomData<K>,
}

impl<T: EntityRef> Default for PooledEntitySet<T> {
    fn default() -> Self {
        Self {
            list: EntityList::new(),
            unused: PhantomData,
        }
    }
}

impl<K> PooledEntitySet<K>
where
    K: EntityRef
{

    pub fn new() -> Self {
        Self {
            list: EntityList::new(),
            unused: PhantomData,
        }
    }

    pub fn contains(&self, k: K, pool: &EntitySetPool) -> bool {
        let index = k.index();

        let list_len = self.list.len(pool);
        let list_len_entries = list_len * 63;

        if index < list_len_entries {
            let list_val = self.list.get(index / 63, pool).unwrap();
            list_val.0 & (1 << (index % 63)) != 0
        } else {
            false
        }
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn clear(&mut self, pool: &mut EntitySetPool) {
        self.list.clear(pool)
    }

    //pub fn keys(&self) -> Keys<K>

    fn grow_to_pages(&mut self, pages: usize, pool: &mut EntitySetPool) {
        let len = self.list.len(pool);
        if len < pages {
            // TODO: grow in one go
            for _ in 0..(pages - len) {
                self.list.push(PooledSetValue(0), pool);
            }
        }
    }

    pub fn insert(&mut self, k: K, pool: &mut EntitySetPool) -> bool {
        let index = k.index();

        let list_len = self.list.len(pool);
        let needed_list_len = (index / 63) + 1; //(index + 62) / 63;

        if list_len < needed_list_len {
            self.grow_to_pages(needed_list_len, pool);
        }

        let result = self.contains(k, pool);
        let val = self.list.get_mut(index / 63, pool).unwrap();

        val.0 |= 1 << (index % 63);

        result
    }

    pub fn remove(&mut self, k: K, pool: &mut EntitySetPool) -> bool {
        let index = k.index();

        let list_len = self.list.len(pool);
        let list_len_entries = list_len * 63;

        if index < list_len_entries {
            let list_val = self.list.get_mut(index / 63, pool).unwrap();
            let bit_idx = 1 << (index % 63);
            let ret = list_val.0 & bit_idx != 0;
            list_val.0 &= !bit_idx;
            ret
        } else {
            false
        }
    }

    pub fn make_copy(&self, pool: &mut EntitySetPool) -> PooledEntitySet<K> {
        let mut new_list = EntityList::new();

        let len = self.list.len(pool);
        for n in 0..len {
            let item = self.list.get(n, pool).unwrap();
            new_list.push(item, pool);
        }

        PooledEntitySet {
            list: new_list,
            unused: PhantomData,
        }
    }

    pub fn internal_list<'a>(&'a self) -> &EntityList<PooledSetValue> {
        &self.list
    }

    pub fn union(&mut self, other: &PooledEntitySet<K>,
                 pool: &mut EntitySetPool) {
        let other_list = other.internal_list();
        let other_pages_len = other_list.len(pool);
        self.grow_to_pages(other_pages_len, pool);
        for n in 0..other_pages_len {
            let other_val = other_list.get(n, pool).unwrap();
            let val_mut = self.list.get_mut(n, pool).unwrap();
            debug_assert!(val_mut.0 != std::u64::MAX);
            debug_assert!(other_val.0 != std::u64::MAX);
            val_mut.0 |= other_val.0;
        }
    }

    pub fn iter<'a>(&self, pool: &'a ListPool<PooledSetValue>) -> PooledEntitySetIter<'a, K> {
        PooledEntitySetIter {
            pool: pool,
            list: self.list.clone(),
            unused: PhantomData,
            current_data: None,
            current: 0,
            finished: false,
        }
    }

}

pub struct PooledEntitySetIter<'a, T> where T: EntityRef {
    pool: &'a ListPool<PooledSetValue>,
    list: EntityList<PooledSetValue>,
    unused: PhantomData<T>,
    current_data: Option<PooledSetValue>,
    current: usize,
    finished: bool,
}
impl<'a, T> Iterator for PooledEntitySetIter<'a, T> where T: EntityRef {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        loop {
            if self.finished { return None; }
            let rem = self.current % 63;
            if rem == 0 {
                self.current_data = self.list.get(self.current / 63, self.pool);
                if self.current_data.is_none() {
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
    use super::ListPool;
    use super::PooledSetValue;
    use super::PooledEntitySet;

    use ::cranelift_entity::entity_impl;

    /// Basic block in function
    #[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct TestEntity(u32);
    entity_impl!(TestEntity, "test_entity");

    #[test]
    fn test_pooled_set() {
        let mut pool: ListPool<PooledSetValue> = ListPool::new();

        let mut set1: PooledEntitySet<TestEntity> = PooledEntitySet::new();

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

        let mut set2: PooledEntitySet<TestEntity> = PooledEntitySet::new();

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
    fn test_iterator() {
        let mut pool: ListPool<PooledSetValue> = ListPool::new();
        let mut set1: PooledEntitySet<TestEntity> = PooledEntitySet::new();

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
