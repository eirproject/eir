use std::any::TypeId;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Formatter};
use std::sync::Arc;

use lazy_static::lazy_static;
use libeir_intern::Symbol;
use meta_table::{MetaEntry, MetaTable};

use crate::operation::{self as op, Op};
use crate::traits::{OpBranches, OpParser, OpPrinter};

lazy_static! {
    pub static ref NORMAL: ArcDialect = {
        let mut d = Dialect::new();
        op::receive::register(&mut d);
        op::binary_construct::register(&mut d);
        op::case::register(&mut d);
        Arc::new(d)
    };
}

pub type ArcDialect = Arc<Dialect>;

// TODO: Expose better interface for registering trait implementations.

pub struct Dialect {
    /// This is the full set of operations that are registered for this dialect.
    operations: HashSet<TypeId>,

    op_branches: MetaTable<dyn OpBranches>,

    op_printer: MetaTable<dyn OpPrinter>,
    op_parser: HashMap<Symbol, Box<dyn OpParser>>,
}
impl Debug for Dialect {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{:?}", self.operations)
    }
}

impl Dialect {
    pub fn new() -> Self {
        Self {
            operations: HashSet::new(),
            op_branches: MetaTable::new(),
            op_printer: MetaTable::new(),
            op_parser: HashMap::new(),
        }
    }

    pub fn contains_op<T: Op>(&self) -> bool {
        self.operations.contains(&TypeId::of::<T>())
    }

    pub fn register_op<T: Op>(&mut self) {
        self.operations.insert(TypeId::of::<T>());
    }

    pub fn register_op_branches_impl<T: MetaEntry + OpBranches>(&mut self) {
        assert!(self.operations.contains(&TypeId::of::<T>()));
        self.op_branches.register::<T>();
    }

    pub fn get_op_branches<'a>(&self, obj: &'a dyn Op) -> Option<&'a dyn OpBranches> {
        self.op_branches.get(obj.meta_entry())
    }

    pub fn register_op_printer_impl<T: MetaEntry + OpPrinter>(&mut self) {
        assert!(self.operations.contains(&TypeId::of::<T>()));
        self.op_printer.register::<T>();
    }

    pub fn get_op_printer<'a>(&self, obj: &'a dyn Op) -> Option<&'a dyn OpPrinter> {
        self.op_printer.get(obj.meta_entry())
    }

    pub fn register_op_parser(&mut self, sym: Symbol, parser: Box<dyn OpParser>) {
        self.op_parser.insert(sym, parser);
    }

    pub fn get_op_parser(&self, sym: Symbol) -> Option<&dyn OpParser> {
        self.op_parser.get(&sym).map(|v| &**v)
    }
}
