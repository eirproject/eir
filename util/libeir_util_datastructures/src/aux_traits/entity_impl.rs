use super::{AuxDebug, AuxImpl, HasAux};
use cranelift_entity::packed_option::ReservedValue;
use cranelift_entity::{EntityList, EntityRef, ListPool, PrimaryMap, SecondaryMap};
use std::fmt::{Debug, Formatter, Result as FmtResult};

impl<C: HasAux<ListPool<E>>, E: EntityRef + ReservedValue + Debug> AuxDebug<C> for EntityList<E> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
        let mut b = f.debug_list();
        b.entries(self.as_slice(aux.get_aux()));
        b.finish()
    }
}

impl<K: EntityRef + Debug, V: AuxDebug<C>, C> AuxDebug<C> for PrimaryMap<K, V> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
        let mut b = f.debug_map();
        b.entries(self.iter().map(|(k, v)| (k, AuxImpl(v, aux))));
        b.finish()
    }
}

impl<K: EntityRef + Debug, V: AuxDebug<C> + Clone, C> AuxDebug<C> for SecondaryMap<K, V> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
        let mut b = f.debug_map();
        b.entries(self.iter().map(|(k, v)| (k, AuxImpl(v, aux))));
        b.finish()
    }
}
