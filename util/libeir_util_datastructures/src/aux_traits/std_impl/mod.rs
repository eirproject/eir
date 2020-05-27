use super::{AuxDebug, AuxEq, AuxHash, AuxImpl, HasAux};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};

impl<T: AuxDebug<C>, C> AuxDebug<C> for Option<T> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
        self.as_ref().map(|v| AuxImpl(v, aux)).fmt(f)
    }
}
impl<T: AuxHash<Aux>, Aux> AuxHash<Aux> for Option<T> {
    fn aux_hash<H: Hasher>(&self, state: &mut H, aux: &Aux) {
        self.as_ref().map(|v| AuxImpl(v, aux)).hash(state)
    }
}
impl<T: AuxEq<Aux>, Aux> AuxEq<Aux> for Option<T> {
    fn aux_eq(&self, other: &Self, self_aux: &Aux, other_aux: &Aux) -> bool {
        let l = self.as_ref().map(|v| AuxImpl(v, self_aux));
        let r = other.as_ref().map(|v| AuxImpl(v, other_aux));
        l.eq(&r)
    }
}

impl<C, K: AuxDebug<C>, V: AuxDebug<C>> AuxDebug<C> for HashMap<K, V> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
        let mut b = f.debug_map();
        b.entries(
            self.iter()
                .map(|(k, v)| (AuxImpl(k, aux.get_aux()), AuxImpl(v, aux.get_aux()))),
        );
        b.finish()
    }
}

macro_rules! impl_regular {
    ($typ:ty) => {
        impl<C> AuxDebug<C> for $typ {
            fn aux_fmt(&self, f: &mut Formatter<'_>, _aux: &C) -> FmtResult {
                Debug::fmt(self, f)
            }
        }
        impl<C> AuxHash<C> for $typ {
            fn aux_hash<H: Hasher>(&self, f: &mut H, _aux: &C) {
                Hash::hash(self, f)
            }
        }
        impl<C> AuxEq<C> for $typ {
            fn aux_eq(&self, rhs: &$typ, _aux: &C, _rhs_aux: &C) -> bool {
                self.eq(rhs)
            }
        }
    };
}

impl_regular!(());
impl_regular!(u8);
impl_regular!(i8);
impl_regular!(u16);
impl_regular!(i16);
impl_regular!(u32);
impl_regular!(i32);
impl_regular!(u64);
impl_regular!(i64);
impl_regular!(usize);
impl_regular!(isize);
impl_regular!(char);
