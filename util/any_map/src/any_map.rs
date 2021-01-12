use fnv::FnvBuildHasher;
use hashbrown::raw::{AllocRef, Global, RawTable};
use std::any::{Any, TypeId};
use std::hash::{BuildHasher, Hash, Hasher};
use std::mem;

pub type DefaultBuildHasher = FnvBuildHasher;

trait AnyKeyTrait: 'static {
    fn clone(&self) -> Box<dyn AnyKeyTrait>;
    fn type_id(&self) -> TypeId;
}
impl<T> AnyKeyTrait for T
where
    T: Clone + Any,
{
    fn clone(&self) -> Box<dyn AnyKeyTrait> {
        Box::new(Clone::clone(self))
    }
    fn type_id(&self) -> TypeId {
        Any::type_id(self)
    }
}
impl dyn AnyKeyTrait {
    fn is<T: Any>(&self) -> bool {
        let t = TypeId::of::<T>();
        let st = self.type_id();
        t == st
    }
    fn downcast_ref<'a, T: Any>(&'a self) -> Option<&'a T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const dyn AnyKeyTrait as *const T)) }
        } else {
            None
        }
    }
}

fn make_hash<K: Hash + ?Sized>(hash_builder: &impl BuildHasher, val: &K) -> u64 {
    let mut state = hash_builder.build_hasher();
    val.hash(&mut state);
    state.finish()
}

fn make_eq<Q, V>(k: &Q) -> impl Fn(&(AnyKey, V)) -> bool + '_
where
    Q: Eq + 'static,
{
    move |x| x.0.eq_k(k)
}

struct AnyKey {
    key: Box<dyn AnyKeyTrait>,
    hash: u64,
}
impl Clone for AnyKey {
    fn clone(&self) -> Self {
        AnyKey {
            key: AnyKeyTrait::clone(&*self.key),
            hash: self.hash,
        }
    }
}

impl AnyKey {
    pub fn new<K, S>(k: K, hb: &S) -> Self
    where
        K: AnyKeyTrait + Hash + 'static,
        S: BuildHasher,
    {
        AnyKey {
            hash: make_hash(hb, &k),
            key: Box::new(k),
        }
    }

    pub fn eq_k<K>(&self, k: &K) -> bool
    where
        K: Eq + 'static,
    {
        if let Some(inner) = self.key.downcast_ref::<K>() {
            inner == k
        } else {
            false
        }
    }
}

pub struct AnyMap<V, S = DefaultBuildHasher, A = Global>
where
    A: AllocRef + Clone,
{
    hash_builder: S,
    table: RawTable<(AnyKey, V), A>,
}

impl<V, S, A> Default for AnyMap<V, S, A>
where
    S: Default,
    A: Default + AllocRef + Clone,
{
    fn default() -> Self {
        AnyMap {
            hash_builder: S::default(),
            table: RawTable::new_in(A::default()),
        }
    }
}

fn get_hash<V>(data: &(AnyKey, V)) -> u64 {
    data.0.hash
}

impl<V> AnyMap<V> {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<V, S, A> AnyMap<V, S, A>
where
    S: BuildHasher,
    A: AllocRef + Clone,
{
    pub fn insert<K>(&mut self, k: K, v: V) -> Option<V>
    where
        K: Clone + Eq + Hash + 'static,
    {
        let hash = make_hash(&self.hash_builder, &k);
        if let Some((_, item)) = self.table.get_mut(hash, make_eq(&k)) {
            Some(mem::replace(item, v))
        } else {
            let key = AnyKey::new(k, &self.hash_builder);
            self.table.insert(hash, (key, v), get_hash);
            None
        }
    }

    pub fn get<K>(&self, k: &K) -> Option<&V>
    where
        K: Eq + Hash + 'static,
    {
        let hash = make_hash(&self.hash_builder, k);
        match self.table.get(hash, make_eq(k)) {
            Some(&(_, ref v)) => Some(v),
            None => None,
        }
    }

    pub fn get_mut<K>(&mut self, k: &K) -> Option<&mut V>
    where
        K: Eq + Hash + 'static,
    {
        let hash = make_hash(&self.hash_builder, k);
        match self.table.get_mut(hash, make_eq(k)) {
            Some(&mut (_, ref mut v)) => Some(v),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::AnyMap;

    #[test]
    fn simple_usage() {
        let mut map = AnyMap::new();

        map.insert(12, 5);
        assert!(*map.get(&12).unwrap() == 5);

        #[derive(Clone, Hash, PartialEq, Eq)]
        struct MyKey;
        map.insert(MyKey, 55);
        assert!(*map.get(&MyKey).unwrap() == 55);

        assert!(*map.get(&12).unwrap() == 5);

        for n in 0..1000 {
            map.insert(n, n);
        }
        for n in 0..1000 {
            assert!(*map.get(&n).unwrap() == n);
        }

        assert!(*map.get(&MyKey).unwrap() == 55);

        #[derive(Clone, Hash, PartialEq, Eq)]
        struct MyKeyI(usize);

        map.insert(MyKeyI(12), 15);
        assert!(*map.get(&MyKeyI(12)).unwrap() == 15);
        assert!(*map.get(&12).unwrap() == 12);

        assert!(map.get(&&12).is_none());
    }
}
