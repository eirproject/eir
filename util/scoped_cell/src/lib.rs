use std::alloc::{Layout, alloc, dealloc};
use std::marker::PhantomData;
use std::ptr::NonNull;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BorrowMutError;

struct ScopedCellGuard<'a, T: ?Sized>(&'a ScopedCell<T>);
impl<'a, T: ?Sized> Drop for ScopedCellGuard<'a, T> {
    fn drop(&mut self) {
        unsafe {
            let inner = self.0.inner_mut();
            debug_assert!(inner.active);
            inner.active = false;
        }
    }
}

pub struct ScopedCell<T: ?Sized> {
    ptr: NonNull<ScopedCellInner<T>>,
    //phantom: PhantomData<ScopedCellInner<T>>,
}


impl<T: ?Sized> ScopedCell<T> {

    pub fn borrow_mut<F, R>(&self, fun: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        unsafe {
            let inner_ptr;
            {
                let inner = self.inner_mut();
                if inner.active {
                    panic!("ScopedCell already borrowed");
                }
                if inner.inner.is_none() {
                    panic!("Tried to borrow ScopedCell outside of creating scope");
                }
                inner.active = true;
                inner_ptr = inner.inner.unwrap();
            }

            // This guard will take care of setting active to false when we
            // leave the inner scope.
            let _guard = ScopedCellGuard(self);

            fun(&mut *inner_ptr.as_ptr())
        }
    }

    pub fn try_borrow_mut<F, R>(&self, fun: F) -> Result<R, BorrowMutError>
    where
        F: FnOnce(&mut T) -> R,
    {
        unsafe {
            let inner_ptr;
            {
                let inner = self.inner_mut();
                if inner.active {
                    return Err(BorrowMutError);
                }
                if inner.inner.is_none() {
                    return Err(BorrowMutError);
                }
                inner.active = true;
                inner_ptr = inner.inner.unwrap();
            }

            // This guard will take care of setting active to false when we
            // leave the inner scope.
            let _guard = ScopedCellGuard(self);

            Ok(fun(&mut *inner_ptr.as_ptr()))
        }
    }

    /// # Safety
    /// This may only be called by internal functions.
    /// A reference to this may only be held while control flow is controlled by the scoped cell.
    /// Only one reference to inner may be obtained at the time.
    unsafe fn inner_mut(&self) -> &mut ScopedCellInner<T> {
        &mut *(self.ptr.as_ptr() as *mut _)
    }

}

impl<T: ?Sized> Clone for ScopedCell<T> {
    fn clone(&self) -> Self {
        unsafe {
            let inner = self.inner_mut();
            inner.references += 1;
        }
        ScopedCell {
            ptr: self.ptr,
            //phantom: PhantomData,
        }
    }
}
impl<T: ?Sized> Drop for ScopedCell<T> {
    fn drop(&mut self) {
        unsafe {
            let is_zero;
            let active;
            {
                let inner = self.inner_mut();
                inner.references -= 1;
                is_zero = inner.references == 0;
                active = inner.active;
            }

            if is_zero {
                debug_assert!(!active);
                dealloc(self.ptr.as_ptr().cast(), Layout::for_value(self.ptr.as_ref()))
            }
        }
    }
}

pub struct ScopedCellInner<T: ?Sized> {
    /// Number of `ScopedCell`s that exist at any given time.
    /// This is incremented on creation/cloning of a new `ScopedCell`, and
    /// decremented in the drop implementation.
    /// When this reaches 0, this struct is deallocated.
    references: usize,
    /// When this is true, a `&mut T` exists
    active: bool,
    /// Pointer to the thing this ScopedCell references.
    /// If this is `Some`, the creator guard of this scoped cell is still in
    /// scope.
    /// If this is `None`, the creator guard has been dropped and any future
    /// borrow attempts will fail.
    inner: Option<NonNull<T>>,
}

/// The guard must not be dropped while a borrow of a related cell is in
/// progress. If this happens, the whole process will be aborted.
pub fn new<'a, T: ?Sized + 'a>(value: &'a mut T) -> ScopedCellCreatorGuard<'a, T>  {
    // Because we are using the value reference in a `PhantomData`, the
    // reference will be concidered used, while not actually existing in a
    // usable form until this guard stuct is dropped.
    //
    // This should make things sound when we create a mutable reference from
    // the pointer we have stored in `ScopedCellInner`, because that can only
    // be done while the actual value reference is tied up in the PhantomData
    // of this guard.

    unsafe {
        let inner_layout = Layout::new::<ScopedCellInner<T>>();
        let inner_ptr_u8 = alloc(inner_layout);

        let inner_ptr = NonNull::new(inner_ptr_u8 as *mut _).unwrap();
        std::ptr::write(inner_ptr.as_ptr(), ScopedCellInner {
            references: 1,
            active: false,
            inner: Some(NonNull::new(value as *mut _).unwrap()),
        });

        let cell = ScopedCell {
            ptr: inner_ptr,
            //phantom: PhantomData,
        };

        ScopedCellCreatorGuard {
            cell,
            life: PhantomData,
        }
    }
}

pub struct ScopedCellCreatorGuard<'a, T: ?Sized> {
    cell: ScopedCell<T>,
    life: PhantomData<&'a mut T>,
}
impl<'a, T: ?Sized> Drop for ScopedCellCreatorGuard<'a, T> {
    fn drop(&mut self) {
        unsafe {
            let inner = self.cell.inner_mut();
            if inner.active {
                println!("FATAL: ScopedCell borrow active while ScopedCellCreatorGuard was dropped");
                std::process::abort();
            }
            inner.inner = None;
        }
    }
}
impl<'a, T: ?Sized> ScopedCellCreatorGuard<'a, T> {
    pub fn clone_cell(&self) -> ScopedCell<T> {
        self.cell.clone()
    }
}

pub fn scoped_cell<T: ?Sized, F, R>(value: &mut T, fun: F) -> R
where
    F: FnOnce(ScopedCell<T>) -> R,
{
    let guard = new(value);

    let cell = guard.clone_cell();
    fun(cell)
}

#[cfg(test)]
mod tests {
    use super::{new, scoped_cell};

    #[test]
    fn creation() {
        let mut a: u32 = 0;
        scoped_cell(&mut a, |_ac| {
        });
    }

    #[test]
    fn basic_usage() {
        let mut a: u32 = 0;

        fn inner(a: &mut u32) {
            scoped_cell(a, |ac| {
                let ac2 = ac.clone();

                ac.borrow_mut(|v| {
                    *v += 1;
                });

                ac2.borrow_mut(|v| {
                    *v += 1;
                });
            });
        }

        inner(&mut a);

        assert!(a == 2);
    }

    //#[test]
    //fn miri_fail() {
    //    use std::marker::PhantomData;

    //    struct Guard<'a>(PhantomData<&'a u8>);
    //    fn new<'a>(_val: &'a mut u8) -> Guard<'a> {
    //        Guard(PhantomData)
    //    }

    //    let mut a = 0u8;
    //    let a_ptr = &mut a as *mut _;

    //    // Should be sound:
    //    {
    //        let guard = new(&mut a);

    //        // Compiler is aware that `a` is borrowed mutably at this point, but
    //        // no actual reference exists. Therefore there will only be one active
    //        // mutable reference when we create one from the pointer.

    //        let inner_ref = unsafe { &mut *a_ptr };
    //        *inner_ref += 1u8;
    //        std::mem::drop(inner_ref);

    //        std::mem::drop(guard);
    //    }
    //}

    #[test]
    fn raw_usage() {
        let mut a: u32 = 0;
        let aco;
        {
            let sc = new(&mut a);
            let ac1 = sc.clone_cell();
            let ac2 = sc.clone_cell();
            aco = ac2.clone();
            ac1.borrow_mut(|_i| {
                assert!(ac2.try_borrow_mut(|_i| ()).is_err());
            });
        }
        assert!(aco.try_borrow_mut(|_i| ()).is_err());
    }

    #[test]
    fn double_borrow_fails() {
        let mut a: u32 = 0;

        scoped_cell(&mut a, |ac| {
            let ac2 = ac.clone();
            ac.borrow_mut(|_i| {
                assert!(ac2.try_borrow_mut(|_i| ()).is_err());
            });
        });

    }

}
