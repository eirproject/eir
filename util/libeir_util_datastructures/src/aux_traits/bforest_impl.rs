use super::{AuxDebug, AuxImpl, HasAux};
use cranelift_bforest::{Set, SetForest};
use std::fmt::{Formatter, Result as FmtResult};

impl<T: Copy + AuxDebug<C>, C: HasAux<SetForest<T>>> AuxDebug<C> for Set<T> {
    fn aux_fmt(&self, f: &mut Formatter<'_>, aux: &C) -> FmtResult {
        let iaux = aux.get_aux();
        let mut b = f.debug_set();
        for val in self.iter(iaux) {
            b.entry(&AuxImpl(&val, aux));
        }
        b.finish()
    }
}
