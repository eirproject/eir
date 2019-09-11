use std::ops::Index;

use cranelift_entity::{ PrimaryMap, EntityRef };
use crate::aux_hash_map::{ AuxHashMap, AuxHash, AuxEq };

pub type DedupPrimaryMap<K, V> = DedupAuxPrimaryMap<K, V, ()>;

#[derive(Debug, Clone)]
pub struct DedupAuxPrimaryMap<K, V, C>
where
    K: EntityRef,
    V: AuxHash<C> + AuxEq<C>,
{
    forward: PrimaryMap<K, V>,
    backward: AuxHashMap<V, K, C>,
}

impl<K, V, C> DedupAuxPrimaryMap<K, V, C>
where
    K: EntityRef,
    V: AuxHash<C> + AuxEq<C>,
{

    pub fn new() -> Self {
        DedupAuxPrimaryMap {
            forward: PrimaryMap::new(),
            backward: AuxHashMap::new(),
        }
    }

    pub fn push(&mut self, v: V, c: &C) -> K where V: Clone {
        if let Some(k) = self.backward.get(&v, c) {
            *k
        } else {
            let k = self.forward.push(v.clone());
            self.backward.try_insert(v, k, c).unwrap();
            k
        }
    }

}

impl<K, V, C> Index<K> for DedupAuxPrimaryMap<K, V, C>
where
    K: EntityRef,
    V: AuxHash<C> + AuxEq<C>,
{
    type Output = V;
    fn index(&self, key: K) -> &V {
        &self.forward[key]
    }
}
