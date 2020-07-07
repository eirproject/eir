use std::ops::Deref;

use std::any::TypeId;
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::raw::TraitObject;

use meta_table::MetaEntry;
use stack_dst::Value;

macro_rules! impl_op {
    ($typ:ident, $name:expr) => {
        impl Op for $typ {
            fn name(&self) -> &str {
                $name
            }
            fn dyn_clone(&self) -> DynOp {
                DynOp::new(self.clone())
            }
            fn type_id(&self) -> TypeId {
                TypeId::of::<Self>()
            }
            fn meta_entry(&self) -> &dyn MetaEntry {
                self
            }
        }
    };
}

pub mod binary_construct;
pub mod case;
pub mod receive;

pub trait Op: MetaEntry {
    fn name(&self) -> &str;

    fn dyn_clone(&self) -> DynOp;

    fn type_id(&self) -> TypeId;

    fn meta_entry(&self) -> &dyn MetaEntry;

    /// Tests for semantic equality between the two operations.
    /// The default implementation assumes no inner state, and simply compares
    /// TypeIds.
    fn op_eq(&self, other: &dyn Op) -> bool {
        self.type_id() == other.type_id()
    }

    fn debug_fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "Op[{}]", self.name())
    }
}

impl dyn Op {
    pub fn try_cast<T: Op>(&self) -> Option<&T> {
        unimplemented!()
    }

    /// Returns some reference to the boxed value if it is of type `T`, or
    /// `None` if it isn't.
    #[inline]
    pub fn downcast_ref<T: Op>(&self) -> Option<&T> {
        // TODO: `type_id` is implemented by the user, if the user returns the
        // wrong TypeId, this is unsafe.
        if self.type_id() == TypeId::of::<T>() {
            unsafe { Some(self.downcast_ref_unchecked()) }
        } else {
            None
        }
    }

    /// Returns a reference to the boxed value, blindly assuming it to be of type `T`.
    /// If you are not *absolutely certain* of `T`, you *must not* call this.
    #[inline]
    pub unsafe fn downcast_ref_unchecked<T: Op>(&self) -> &T {
        let trait_object: TraitObject = ::std::mem::transmute(self);
        std::mem::transmute(trait_object.data)
    }
}

pub trait OpBuild: Op {
    /// In order to allow the operation implementation to have control over
    /// invariants for the operaion in the IR, each buildable operation
    /// has a token that must be provided to the function builder when adding
    /// it to the IR.
    /// The operation implementation has control over how this token type is
    /// created, and may restrict it in order to ensure that only it is able
    /// to build itself.
    type Token;
}

pub struct DynOp(Value<dyn Op>);

impl DynOp {
    pub fn new<T: Op>(value: T) -> Self {
        DynOp(Value::new_stable(value, |v| v as _).ok().unwrap())
    }
}

impl Deref for DynOp {
    type Target = dyn Op;
    fn deref(&self) -> &dyn Op {
        &*self.0
    }
}

impl Clone for DynOp {
    fn clone(&self) -> Self {
        self.0.dyn_clone()
    }
}

impl Debug for DynOp {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        self.0.debug_fmt(formatter)
    }
}
