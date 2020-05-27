use std::{
    any::{Any, TypeId},
    marker::PhantomData,
};

use hashbrown::HashMap;

/// This implements `Send` and `Sync` unconditionally.
/// (the trait itself doesn't need to have these bounds and the
/// resources are already guaranteed to fulfill it).
struct Invariant<T: ?Sized>(*mut T);

unsafe impl<T> Send for Invariant<T> where T: ?Sized {}

unsafe impl<T> Sync for Invariant<T> where T: ?Sized {}

pub trait MetaEntry: 'static {
    fn get_type_id(&self) -> TypeId;
}
#[macro_export]
macro_rules! impl_meta_entry {
    ($typ:ty) => {
        impl $crate::MetaEntry for $typ {
            fn get_type_id(&self) -> std::any::TypeId {
                std::any::TypeId::of::<$typ>()
            }
        }
    };
}

/// Helper trait for the `MetaTable`.
/// This trait is required to be implemented for a trait to be compatible with
/// the meta table.
///
/// # Memory safety
///
/// Not casting `self` but e.g. a field to the trait object can result in severe
/// memory safety issues.
///
/// # Examples
///
/// ```
/// use meta_table::CastFrom;
///
/// trait Foo {
///     fn foo1(&self);
///     fn foo2(&mut self, x: i32) -> i32;
/// }
///
/// unsafe impl<T> CastFrom<T> for dyn Foo
/// where
///     T: Foo + 'static,
/// {
///     fn cast(t: &T) -> &(dyn Foo + 'static) {
///         t
///     }
///
///     fn cast_mut(t: &mut T) -> &mut (dyn Foo + 'static) {
///         t
///     }
/// }
/// ```
pub unsafe trait CastFrom<T> {
    /// Casts an immutable `T` reference to a trait object.
    fn cast(t: &T) -> &Self;

    /// Casts a mutable `T` reference to a trait object.
    fn cast_mut(t: &mut T) -> &mut Self;
}
#[macro_export]
macro_rules! impl_cast_from {
    ($typ:tt) => {
        unsafe impl<T> $crate::CastFrom<T> for dyn $typ
        where
            T: $typ + 'static,
        {
            fn cast(t: &T) -> &(dyn $typ + 'static) {
                t
            }

            fn cast_mut(t: &mut T) -> &mut (dyn $typ + 'static) {
                t
            }
        }
    };
}

struct Fat(usize);

impl Fat {
    pub unsafe fn from_ptr<T: ?Sized>(t: &T) -> Self {
        use std::ptr::read;

        assert_unsized::<T>();

        let fat_ptr = &t as *const &T as *const usize;
        // Memory layout:
        // [object pointer, vtable pointer]
        //  ^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^
        //  8 bytes       | 8 bytes
        // (on 32-bit both have 4 bytes)
        let vtable = read::<usize>(fat_ptr.offset(1));

        Fat(vtable)
    }

    pub unsafe fn create_ptr<T: ?Sized>(&self, ptr: *const ()) -> *const T {
        let fat_ptr: (*const (), usize) = (ptr, self.0);

        *(&fat_ptr as *const (*const (), usize) as *const *const T)
    }
}

/// The `MetaTable` which allows to store object-safe trait implementations for
/// resources.
///
/// For example, you have a trait `Foo` that is implemented by several
/// resources. You can register all the implementors using
/// `MetaTable::register`. Later on, you can iterate over all resources that
/// implement `Foo` without knowing their specific type.
///
/// # Examples
///
/// ```
/// use meta_table::{MetaTable, impl_cast_from, impl_meta_entry};
///
/// trait Object {
///     fn method1(&self) -> i32;
///
///     fn method2(&mut self, x: i32);
/// }
/// impl_cast_from!(Object);
///
/// struct ImplementorA(i32);
/// impl_meta_entry!(ImplementorA);
///
/// impl Object for ImplementorA {
///     fn method1(&self) -> i32 {
///         self.0
///     }
///
///     fn method2(&mut self, x: i32) {
///         self.0 += x;
///     }
/// }
///
/// struct ImplementorB(i32);
/// impl_meta_entry!(ImplementorB);
///
/// impl Object for ImplementorB {
///     fn method1(&self) -> i32 {
///         self.0
///     }
///
///     fn method2(&mut self, x: i32) {
///         self.0 *= x;
///     }
/// }
///
/// let mut table = MetaTable::<dyn Object>::new();
/// table.register(&ImplementorA(31415)); // Can just be some instance of type `&ImplementorA`.
/// table.register(&ImplementorB(27182));
///
/// assert!(table.get(&ImplementorA(12)).unwrap().method1() == 12);
/// assert!(table.get(&ImplementorB(12)).unwrap().method1() == 12);
/// ```
pub struct MetaTable<T: ?Sized> {
    fat: Vec<Fat>,
    indices: HashMap<TypeId, usize>,
    tys: Vec<TypeId>,
    // `MetaTable` is invariant over `T`
    marker: PhantomData<Invariant<T>>,
}

impl<T: ?Sized> MetaTable<T> {
    /// Creates a new `MetaTable`.
    pub fn new() -> Self {
        assert_unsized::<T>();

        Default::default()
    }

    /// Registers a resource `R` that implements the trait `T`.
    /// This just needs some instance of type `R` to retrieve the vtable.
    /// It doesn't have to be the same object you're calling `get` with later.
    pub fn register<R>(&mut self, r: &R)
    where
        R: MetaEntry,
        T: CastFrom<R> + 'static,
    {
        use hashbrown::hash_map::Entry;

        let thin_ptr = r as *const R as usize;
        let casted_ptr = <T as CastFrom<R>>::cast(r);
        let thin_casted_ptr = casted_ptr as *const T as *const () as usize;

        assert_eq!(
            thin_ptr, thin_casted_ptr,
            "Bug: `CastFrom` did not cast `self`"
        );

        let fat = unsafe { Fat::from_ptr(casted_ptr) };

        let ty_id = r.get_type_id();

        // Important: ensure no entry exists twice!
        let len = self.indices.len();
        match self.indices.entry(ty_id) {
            Entry::Occupied(occ) => {
                let ind = *occ.get();

                self.fat[ind] = fat;
            }
            Entry::Vacant(vac) => {
                vac.insert(len);

                self.fat.push(fat);
                self.tys.push(ty_id);
            }
        }
    }

    /// Tries to convert `world` to a trait object of type `&T`.
    /// If `world` doesn't have an implementation for `T` (or it wasn't
    /// registered), this will return `None`.
    pub fn get<'a>(&self, res: &'a dyn MetaEntry) -> Option<&'a T> {
        unsafe {
            self.indices
                .get(&res.get_type_id())
                .map(move |&ind| &*self.fat[ind].create_ptr(res as *const _ as *const ()))
        }
    }

    /// Tries to convert `world` to a trait object of type `&mut T`.
    /// If `world` doesn't have an implementation for `T` (or it wasn't
    /// registered), this will return `None`.
    pub fn get_mut<'a>(&self, res: &'a dyn MetaEntry) -> Option<&'a mut T> {
        unsafe {
            self.indices.get(&res.get_type_id()).map(move |&ind| {
                &mut *(self.fat[ind].create_ptr::<T>(res as *const _ as *const ()) as *mut T)
            })
        }
    }
}

impl<T> Default for MetaTable<T>
where
    T: ?Sized,
{
    fn default() -> Self {
        MetaTable {
            fat: Default::default(),
            indices: Default::default(),
            tys: Default::default(),
            marker: Default::default(),
        }
    }
}

fn assert_unsized<T: ?Sized>() {
    use std::mem::size_of;

    assert_eq!(size_of::<&T>(), 2 * size_of::<usize>());
}

#[cfg(test)]
mod tests {
    use super::*;

    trait Object: MetaEntry {
        fn method1(&self) -> i32;

        fn method2(&mut self, x: i32);
    }

    unsafe impl<T> CastFrom<T> for dyn Object
    where
        T: Object + 'static,
    {
        fn cast(t: &T) -> &Self {
            t
        }

        fn cast_mut(t: &mut T) -> &mut Self {
            t
        }
    }

    struct ImplementorA(i32);

    impl_meta_entry!(ImplementorA);
    impl Object for ImplementorA {
        fn method1(&self) -> i32 {
            self.0
        }

        fn method2(&mut self, x: i32) {
            self.0 += x;
        }
    }

    struct ImplementorB(i32);

    impl_meta_entry!(ImplementorB);
    impl Object for ImplementorB {
        fn method1(&self) -> i32 {
            self.0
        }

        fn method2(&mut self, x: i32) {
            self.0 *= x;
        }
    }

    //#[test]
    //fn test_iter_all() {
    //    let mut world = World::empty();

    //    world.insert(ImplementorA(3));
    //    world.insert(ImplementorB(1));

    //    let mut table = MetaTable::<dyn Object>::new();
    //    table.register(&ImplementorA(125));
    //    table.register(&ImplementorB(111_111));

    //    {
    //        let mut iter = table.iter(&world);
    //        assert_eq!(iter.next().unwrap().method1(), 3);
    //        assert_eq!(iter.next().unwrap().method1(), 1);
    //    }

    //    {
    //        let mut iter_mut = table.iter_mut(&world);
    //        let obj = iter_mut.next().unwrap();
    //        obj.method2(3);
    //        assert_eq!(obj.method1(), 6);
    //        let obj = iter_mut.next().unwrap();
    //        obj.method2(4);
    //        assert_eq!(obj.method1(), 4);
    //    }
    //}

    //#[test]
    //fn test_iter_all_after_removal() {
    //    let mut world = World::empty();

    //    world.insert(ImplementorA(3));
    //    world.insert(ImplementorB(1));

    //    let mut table = MetaTable::<dyn Object>::new();
    //    table.register(&ImplementorA(125));
    //    table.register(&ImplementorB(111_111));

    //    {
    //        let mut iter = table.iter(&world);
    //        assert_eq!(iter.next().unwrap().method1(), 3);
    //        assert_eq!(iter.next().unwrap().method1(), 1);
    //    }

    //    world.remove::<ImplementorA>().unwrap();

    //    {
    //        let mut iter = table.iter(&world);
    //        assert_eq!(iter.next().unwrap().method1(), 1);
    //    }

    //    world.remove::<ImplementorB>().unwrap();
    //}

    struct ImplementorC;

    impl_meta_entry!(ImplementorC);
    impl Object for ImplementorC {
        fn method1(&self) -> i32 {
            33
        }

        fn method2(&mut self, _x: i32) {
            unimplemented!()
        }
    }

    struct ImplementorD;

    impl_meta_entry!(ImplementorD);
    impl Object for ImplementorD {
        fn method1(&self) -> i32 {
            42
        }

        fn method2(&mut self, _x: i32) {
            unimplemented!()
        }
    }

    #[test]
    fn get() {
        let mut table = MetaTable::<dyn Object>::new();
        table.register(&ImplementorC);
        table.register(&ImplementorD);

        let t1 = ImplementorC;
        let t1d: &dyn MetaEntry = &t1;

        let t2 = ImplementorD;
        let t2d: &dyn MetaEntry = &t2;

        assert_eq!(table.get(t1d).unwrap().method1(), 33);
        assert_eq!(table.get(t2d).unwrap().method1(), 42);
    }
}
