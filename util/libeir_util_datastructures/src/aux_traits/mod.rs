use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};

mod bforest_impl;
mod entity_impl;
mod std_impl;

pub trait HasAux<Inner> {
    fn get_aux(&self) -> &Inner;
}
impl<T> HasAux<T> for T {
    fn get_aux(&self) -> &Self {
        self
    }
}
impl<T> HasAux<T> for &T {
    fn get_aux(&self) -> &T {
        *self
    }
}

pub trait AuxDebug<Aux> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, container: &Aux) -> FmtResult;
}

/// Hashes a data type with access to auxiliary data
pub trait AuxHash<Aux> {
    fn aux_hash<H: Hasher>(&self, state: &mut H, container: &Aux);
}

/// Tests equality with access to auxiliary data
pub trait AuxEq<Aux> {
    fn aux_eq(&self, other: &Self, self_container: &Aux, other_container: &Aux) -> bool;
    fn aux_ne(&self, other: &Self, self_container: &Aux, other_container: &Aux) -> bool {
        !self.aux_eq(other, self_container, other_container)
    }
}

pub struct AuxImpl<'a, V, Aux>(pub &'a V, pub &'a Aux);
impl<'a, V: AuxDebug<Aux>, Aux> Debug for AuxImpl<'a, V, Aux> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.0.aux_fmt(f, self.1)
    }
}
impl<'a, V: AuxHash<Aux>, Aux> Hash for AuxImpl<'a, V, Aux> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.aux_hash(state, self.1)
    }
}
impl<'a, V: AuxEq<Aux>, Aux> PartialEq for AuxImpl<'a, V, Aux> {
    fn eq(&self, rhs: &Self) -> bool {
        self.0.aux_eq(rhs.0, self.1, rhs.1)
    }
}
impl<'a, V: AuxEq<Aux>, Aux> Eq for AuxImpl<'a, V, Aux> {}

#[macro_export]
macro_rules! aux_container_impl {
    ($typ:ty) => {
        impl std::fmt::Debug for $typ {
            fn debug(&self, formatter: std::fmt::Formatter) -> std::fmt::Result {
                $crate::aux_traits::AuxDebug::fmt(self, formatter, self)
            }
        }
        impl std::hash::Hash for $typ {
            fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
                $crate::aux_traits::AuxHash::fmt(self, hasher, self)
            }
        }
        impl PartialEq for $typ {
            fn eq(&self, other: &Self) {
                $crate::aux_traits::AuxEq::eq(self, other, self, other)
            }
        }
        impl Eq for $typ {}
    };
}
