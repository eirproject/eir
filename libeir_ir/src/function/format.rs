#[macro_export]
macro_rules! function_format {
    ($fun:expr, $str:expr, $($arg:expr),*) => {
        format!($str, $($crate::ContainerDebugAdapter { container: $fun, value: $arg }),*)
    };
}

use std::fmt::{Formatter, Result, Debug};
use std::hash::Hash;

use std::collections::{HashMap, BTreeMap};
use std::collections::{HashSet, BTreeSet};

use super::{Function, Value, ValueKind};

pub trait ContainerDebug<C>: Debug {
    fn fmt(&self, container: &C, f: &mut Formatter) -> Result;
}

pub struct ContainerDebugAdapter<'a, C, V> {
    pub container: &'a C,
    pub value: &'a V,
}
impl<C, V> Debug for ContainerDebugAdapter<'_, C, V>
where V: ContainerDebug<C>,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        ContainerDebug::fmt(self.value, self.container, f)
    }
}

default impl<C, V> ContainerDebug<C> for V {
    fn fmt(&self, _con: &C, f: &mut Formatter) -> Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl ContainerDebug<Function> for Value {
    fn fmt(&self, fun: &Function, f: &mut Formatter) -> Result {
        match fun.value_kind(*self) {
            ValueKind::PrimOp(prim) =>
                write!(f, "{}#{}", self, prim),
            ValueKind::Block(block) =>
                write!(f, "{}#{}", self, block),
            ValueKind::Argument(block, num) =>
                write!(f, "{}#{}[{}]", self, block, num),
            ValueKind::Const(cons) =>
                write!(f, "{}#{}", self, cons),
        }
    }
}

impl ContainerDebug<Function> for &[Value] {
    fn fmt(&self, fun: &Function, f: &mut Formatter) -> Result {
        let values = self.as_ref();
        let mut builder = f.debug_list();

        builder.entries(values.iter().map(|v| {
            ContainerDebugAdapter {
                container: fun,
                value: v,
            }
        })).finish()
    }
}

impl<C, K, V> ContainerDebug<C> for HashMap<K, V>
where
    K: ContainerDebug<C> + Hash + Eq,
    V: ContainerDebug<C>,
{
    fn fmt(&self, con: &C, f: &mut Formatter) -> Result {
        let mut builder = f.debug_map();
        builder.entries(self.iter().map(|(k, v)| {
            (
                ContainerDebugAdapter { container: con, value: k },
                ContainerDebugAdapter { container: con, value: v },
            )
        })).finish()
    }
}
impl<C, K, V> ContainerDebug<C> for BTreeMap<K, V>
where
    K: ContainerDebug<C> + Hash,
    V: ContainerDebug<C>,
{
    fn fmt(&self, con: &C, f: &mut Formatter) -> Result {
        let mut builder = f.debug_map();
        builder.entries(self.iter().map(|(k, v)| {
            (
                ContainerDebugAdapter { container: con, value: k },
                ContainerDebugAdapter { container: con, value: v },
            )
        })).finish()
    }
}

impl<C, V> ContainerDebug<C> for HashSet<V>
where
    V: ContainerDebug<C> + Hash + Eq,
{
    fn fmt(&self, con: &C, f: &mut Formatter) -> Result {
        let mut builder = f.debug_set();
        builder.entries(self.iter().map(|v| {
            ContainerDebugAdapter { container: con, value: v }
        })).finish()
    }
}
impl<C, V> ContainerDebug<C> for BTreeSet<V>
where
    V: ContainerDebug<C> + Hash + Eq,
{
    fn fmt(&self, con: &C, f: &mut Formatter) -> Result {
        let mut builder = f.debug_set();
        builder.entries(self.iter().map(|v| {
            ContainerDebugAdapter { container: con, value: v }
        })).finish()
    }
}

impl<C> ContainerDebug<C> for bool {
    fn fmt(&self, _con: &C, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}

macro_rules! impl_tuple {
    ($(($typ:ident, $n:tt)),*) => {
        #[allow(unused_variables)]
        impl<Container, $($typ),*> ContainerDebug<Container> for ($($typ,)*)
        where $($typ: ContainerDebug<Container>,)*
        {
            fn fmt(&self, con: &Container, f: &mut Formatter) -> Result {
                let mut builder = f.debug_tuple("");
                $(
                    builder.field(&ContainerDebugAdapter { container: con, value: &self.$n });
                )*
                builder.finish()
            }
        }

    }
}

impl_tuple!();
impl_tuple!((A, 0));
impl_tuple!((A, 0), (B, 1));
impl_tuple!((A, 0), (B, 1), (C, 2));
impl_tuple!((A, 0), (B, 1), (C, 2), (D, 3));
impl_tuple!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4));
impl_tuple!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5));
impl_tuple!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6));
impl_tuple!((A, 0), (B, 1), (C, 2), (D, 3), (E, 4), (F, 5), (G, 6), (H, 7));
