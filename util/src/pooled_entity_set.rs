use std::marker::PhantomData;
use ::cranelift_entity::EntityRef;
use ::cranelift_entity::ListPool;
use ::cranelift_entity::EntityList;
use ::cranelift_entity::packed_option::ReservedValue;

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

    pub fn contains(&self, k: K, pool: &ListPool<PooledSetValue>) -> bool {
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

    pub fn clear(&mut self, pool: &mut ListPool<PooledSetValue>) {
        self.list.clear(pool)
    }

    //pub fn keys(&self) -> Keys<K>

    pub fn insert(&mut self, k: K, pool: &mut ListPool<PooledSetValue>) -> bool {
        let index = k.index();

        let list_len = self.list.len(pool);
        let needed_list_len = (index / 63) + 1; //(index + 62) / 63;

        if list_len < needed_list_len {
            //self.list.grow_at(list_len, needed_list_len - list_len, pool);
            // TODO: grow in one go
            for _ in 0..(needed_list_len - list_len) {
                self.list.push(PooledSetValue(0), pool);
            }
        }

        let result = self.contains(k, pool);
        let val = self.list.get_mut(index / 63, pool).unwrap();

        val.0 |= 1 << (index % 63);

        result
    }

    pub fn remove(&mut self, k: K, pool: &mut ListPool<PooledSetValue>) -> bool {
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

    pub fn make_copy(&self, pool: &mut ListPool<PooledSetValue>) -> PooledEntitySet<K> {
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


}
