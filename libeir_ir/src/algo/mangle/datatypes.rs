#![allow(dead_code)]
use crate::Function;
use crate::{Value, Block};

use super::MangleReceiver;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FromT<T>(pub T);
impl<T> FromT<T> {

    pub(super) fn map_fun<'a, 'b, R, F, V>(self, recv: &'a R, mut f: F) -> FromT<V>
    where R: MangleReceiver<'b>,
          F: FnMut(&Function, T) -> V + 'a
    {
        let fun = recv.from();
        let val = f(fun, self.inner());
        FromT(val)
    }

    pub fn inner(self) -> T {
        self.0
    }

}
impl<T> From<T> for FromT<T> {
    fn from(f: T) -> Self {
        FromT(f)
    }
}
impl<T> std::fmt::Display for FromT<T> where T: std::fmt::Display {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "From({})", self.0)
    }
}
pub(super) type FromValue = FromT<Value>;
pub(super) type FromBlock = FromT<Block>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ToT<T>(pub T);
impl<T> ToT<T> {

    pub(super) fn map_fun<'a, 'b, R, F, V>(self, recv: &'a R, mut f: F) -> ToT<V>
    where R: MangleReceiver<'b>,
          F: FnMut(&Function, T) -> V + 'a
    {
        let fun = recv.to_fun();
        let val = f(fun, self.inner());
        ToT(val)
    }

    pub fn inner(self) -> T {
        self.0
    }

}
impl<T> From<T> for ToT<T> {
    fn from(f: T) -> Self {
        ToT(f)
    }
}
impl<T> std::fmt::Display for ToT<T> where T: std::fmt::Display {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "To({})", self.0)
    }
}
pub type ToValue = ToT<Value>;
pub type ToBlock = ToT<Block>;

/// Because the mangler code supports operating across function containers,
/// we need a mechanism to keep track of when a value is referenced in the
/// old container or when it is referenced in the new one.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MangleTarget<T> {
    From(FromT<T>),
    To(ToT<T>),
}
impl<T> std::fmt::Display for MangleTarget<T> where T: std::fmt::Display {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "To({})", self.inner_ref())
    }
}
impl<T> MangleTarget<T> {

    pub fn inner(self) -> T {
        match self {
            MangleTarget::From(i) => i.0,
            MangleTarget::To(i) => i.0,
        }
    }
    pub fn inner_ref(&self) -> &T {
        match self {
            MangleTarget::From(i) => &i.0,
            MangleTarget::To(i) => &i.0,
        }
    }

    pub(super) fn fun<'a, 'b, R>(&self, recv: &'a R) -> &'a Function
    where R: MangleReceiver<'b>,
    {
        match self {
            MangleTarget::From(_) => recv.from(),
            MangleTarget::To(_) => recv.to_fun(),
        }
    }

    pub fn map<F, V>(self, mut f: F) -> MangleTarget<V>
    where F: FnMut(T) -> V
    {
        match self {
            MangleTarget::From(i) => {
                let val = f(i.inner());
                MangleTarget::From(val.into())
            },
            MangleTarget::To(i) => {
                let val = f(i.inner());
                MangleTarget::To(val.into())
            },
        }
    }

    pub(super) fn map_fun<'a, 'b, R, F, V>(self, recv: &'a R, mut f: F) -> MangleTarget<V>
    where R: MangleReceiver<'b>,
          F: FnMut(&Function, T) -> V + 'a
    {
        let fun = self.fun(recv);
        match self {
            MangleTarget::From(i) => {
                let val = f(fun, i.inner());
                MangleTarget::From(val.into())
            },
            MangleTarget::To(i) => {
                let val = f(fun, i.inner());
                MangleTarget::To(val.into())
            },
        }
    }

    pub fn new_with<N>(&self, new: N) -> MangleTarget<N> {
        match self {
            MangleTarget::From(_) => MangleTarget::From(FromT(new)),
            MangleTarget::To(_) => MangleTarget::To(ToT(new)),
        }
    }

    pub fn from(self) -> Option<FromT<T>> {
        match self {
            MangleTarget::From(inner) => Some(inner),
            MangleTarget::To(_) => None,
        }
    }
    pub fn to(self) -> Option<ToT<T>> {
        match self {
            MangleTarget::From(_) => None,
            MangleTarget::To(inner) => Some(inner),
        }
    }

}
impl<I> MangleTarget<Option<I>> {
    pub fn transpose_opt(self) -> Option<MangleTarget<I>> {
        match self {
            MangleTarget::From(FromT(None)) => None,
            MangleTarget::To(ToT(None)) => None,
            MangleTarget::From(FromT(Some(v))) => Some(MangleTarget::From(v.into())),
            MangleTarget::To(ToT(Some(v))) => Some(MangleTarget::To(v.into())),
        }
    }
}
impl<T> MangleTarget<&T> where T: Clone {
    pub fn cloned(&self) -> MangleTarget<T> {
        self.map(|v| v.clone())
    }
}
impl<I, V> MangleTarget<I> where I: IntoIterator<Item = V> {
    pub fn transpose_iter(self) -> impl Iterator<Item = MangleTarget<V>> {
        let constructor = match self {
            MangleTarget::From(_) => {
                fn a<V>(v: V) -> MangleTarget<V> {
                    MangleTarget::From(FromT(v))
                }
                a
            },
            MangleTarget::To(_) => {
                fn a<V>(v: V) -> MangleTarget<V> {
                    MangleTarget::To(ToT(v))
                }
                a
            },
        };
        self.inner().into_iter().map(constructor)
    }
}
impl<I> From<FromT<I>> for MangleTarget<I> {
    fn from(i: FromT<I>) -> MangleTarget<I> {
        MangleTarget::From(i)
    }
}
impl<I> From<ToT<I>> for MangleTarget<I> {
    fn from(i: ToT<I>) -> MangleTarget<I> {
        MangleTarget::To(i)
    }
}
pub(super) type MangleValue = MangleTarget<Value>;
pub(super) type MangleBlock = MangleTarget<Block>;
