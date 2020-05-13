use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::Hasher;

pub trait AuxDebug<C> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult;
}

pub struct AuxDebugImpl<'a, V: AuxDebug<C>, C>(pub &'a V, pub &'a C);
impl<'a, V: AuxDebug<C>, C> Debug for AuxDebugImpl<'a, V, C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.0.aux_fmt(f, self.1)
    }
}


/// Hashes a data type with access to auxiliary data
pub trait AuxHash<C> {
    fn aux_hash<H: Hasher>(&self, state: &mut H, container: &C);
}

/// Tests equality with access to auxiliary data
pub trait AuxEq<C> {
    fn aux_eq(&self, other: &Self, container: &C) -> bool;
    fn aux_ne(&self, other: &Self, container: &C) -> bool {
        !self.aux_eq(other, container)
    }
}

mod entity_impl {
    use std::fmt::{Debug, Formatter, Result as FmtResult};
    use cranelift_entity::{EntityRef, EntityList, ListPool, PrimaryMap, SecondaryMap};
    use cranelift_entity::packed_option::ReservedValue;
    use super::{AuxDebug, AuxDebugImpl};

    impl<E: EntityRef + ReservedValue + Debug> AuxDebug<ListPool<E>> for EntityList<E> {
        fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &ListPool<E>) -> FmtResult {
            let mut b = f.debug_list();
            b.entries(self.as_slice(aux));
            b.finish()
        }
    }

    impl<K: EntityRef + Debug, V: AuxDebug<C>, C> AuxDebug<C> for PrimaryMap<K, V> {
        fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
            let mut b = f.debug_map();
            b.entries(self.iter().map(|(k, v)| (k, AuxDebugImpl(v, aux))));
            b.finish()
        }
    }
    impl<K: EntityRef + Debug, V: AuxDebug<C> + Clone, C> AuxDebug<C> for SecondaryMap<K, V> {
        fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
            let mut b = f.debug_map();
            b.entries(self.iter().map(|(k, v)| (k, AuxDebugImpl(v, aux))));
            b.finish()
        }
    }

    impl<E: AuxDebug<C>, C> AuxDebug<C> for Option<E> {
        fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
            match self {
                Some(inner) => {
                    f.debug_tuple("Some")
                     .field(&AuxDebugImpl(inner, aux))
                     .finish()
                }
                None => {
                    f.debug_tuple("None")
                     .finish()
                }
            }
        }
    }

}
