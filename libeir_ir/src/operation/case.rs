use std::any::TypeId;

use cranelift_entity::EntityList;
use libeir_diagnostics::SourceSpan;
use meta_table::{impl_meta_entry, MetaEntry};
use pretty::{DocAllocator, RefDoc};

use super::{DynOp, Op, OpBuild};
use crate::pattern::{PatternClause, PatternContainer};
use crate::traits::{FormatOpCtx, OpBranches, OpPrinter};
use crate::{Block, Dialect, Function, FunctionBuilder, Value};

pub struct CaseToken(());

/// Case structure
/// ```ignore
/// (
///     no_match: fn(),
///     (
///         guard: fn(ok: fn(), fail: fn(), pat_refs..),
///         body: fn(pat_refs..),
///     )..,
///     match_val: <term..>,
///     match_values: term..,
/// )
/// ```
///
/// High level matching construct, lowered to explicit control flow
/// in a Eir compiler pass. Only allowed in high level Eir dialect.
/// This OP indicates the start of a case structure.
/// A guard is strictly required to return through either the ok
/// or fail continuation.
#[derive(Debug, Clone)]
pub struct Case {
    inner: Box<Inner>,
}
impl_meta_entry!(Case);

impl Case {
    pub fn pat<'a>(&'a self) -> &'a PatternContainer {
        &self.inner.container
    }
    pub fn clauses<'a>(&'a self) -> &'a [PatternClause] {
        &self.inner.clauses
    }
}

#[derive(Debug, Clone)]
struct Inner {
    container: PatternContainer,
    clauses: Vec<PatternClause>,
}

impl Op for Case {
    fn name(&self) -> &str {
        "case"
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
    fn op_eq(&self, other: &dyn Op) -> bool {
        if let Some(other_i) = other.downcast_ref::<Self>() {
            unimplemented!()
        } else {
            false
        }
    }
}

impl OpBranches for Case {
    fn branches_len(&self) -> usize {
        1 + self.inner.clauses.len()
    }
    fn branch_num(&self, fun: &Function, block: Block, branch_n: usize) -> Value {
        // [no_branch, guard_1, body_1, guard_2, body_2, ..]
        // guards are not control flow, they are lambda calls
        let reads = fun.block_reads(block);
        if branch_n == 0 {
            reads[0]
        } else {
            let n = branch_n - 1;
            reads[2 + n * 2]
        }
    }
}

impl OpPrinter for Case {
    fn to_doc<'doc>(&self, ctx: &mut dyn FormatOpCtx<'doc>, block: Block) -> RefDoc<'doc, ()> {
        let arena = ctx.arena();

        let block = arena.nil();

        arena
            .nil()
            .append(arena.text("case"))
            .append(arena.space())
            .append(block.nest(1).braces())
            .into_doc()
    }
}

impl Case {
    pub fn builder() -> CaseBuilder {
        CaseBuilder::default()
    }
}

pub struct CaseBuilder {
    span: SourceSpan,

    pub container: PatternContainer,

    pub match_on: Option<Value>,
    pub no_match: Option<Value>,

    clauses: Vec<PatternClause>,
    clauses_b: Vec<Value>,
    values: Vec<Value>,
}

impl Default for CaseBuilder {
    fn default() -> Self {
        CaseBuilder {
            span: SourceSpan::UNKNOWN,

            container: PatternContainer::new(),

            match_on: None,
            no_match: None,

            clauses: Vec::new(),
            clauses_b: Vec::new(),
            values: Vec::new(),
        }
    }
}

impl CaseBuilder {
    pub fn new(span: SourceSpan) -> Self {
        let mut this = Self::default();
        this.span = span;
        this
    }

    pub fn set_span(&mut self, span: SourceSpan) {
        self.span = span;
    }

    pub fn push_clause<'a>(
        &mut self,
        clause: PatternClause,
        guard: Value,
        body: Value,
        b: &mut FunctionBuilder<'a>,
    ) {
        self.clauses.push(clause);
        self.clauses_b.extend([guard, body].iter().cloned());
    }

    pub fn push_value<'a>(&mut self, value: Value, b: &mut FunctionBuilder<'a>) {
        self.values.push(value);
    }

    pub fn finish<'a>(mut self, block: Block, b: &mut FunctionBuilder<'a>) {
        // Validate that the number of values matches between the
        // clauses and reads
        let mut num_values = 0;
        for clause in &self.clauses {
            num_values += self.container.clause_values(*clause).len();
        }
        let num_value_reads = self.values.len();
        assert!(num_values == num_value_reads);

        let op = Case {
            inner: Box::new(Inner {
                container: self.container,
                clauses: self.clauses,
            }),
        };

        let mut args = vec![self.no_match.unwrap()];
        args.extend(self.clauses_b.iter().cloned());
        args.push(self.match_on.unwrap());
        args.extend(self.values.iter().cloned());

        b.op_intrinsic(block, op, &args, CaseToken(()));

        //let data = b.fun_mut().blocks.get_mut(block).unwrap();
        //assert!(data.op.is_none());
        //assert!(data.reads.is_empty());

        //b.op_intrinsic(block, op, args, _token)

        //data.op = Some(OpKind::Case {
        //    clauses: self.clauses,
        //});
        //data.location = b.fun_mut().locations.location(None, None, None, self.span);

        //let mut buf = b.value_buf.take().unwrap();
        //buf.clear();

        //data.reads
        //    .push(self.no_match.unwrap(), &mut b.fun.pool.value);

        //// Guard and body blocks for clauses
        //for c in self.clauses_b.as_slice(&b.fun.pool.value) {
        //    buf.push(*c);
        //}
        //data.reads
        //    .extend(buf.iter().cloned(), &mut b.fun.pool.value);

        //// Match value
        //data.reads
        //    .push(self.match_on.unwrap(), &mut b.fun.pool.value);

        //// Values
        //buf.clear();
        //for c in self.values.as_slice(&b.fun.pool.value) {
        //    buf.push(*c);
        //}
        //data.reads
        //    .extend(buf.iter().cloned(), &mut b.fun.pool.value);

        //buf.clear();
        //b.value_buf = Some(buf);

        //self.clauses_b.clear(&mut b.fun.pool.value);
        //self.values.clear(&mut b.fun.pool.value);

        //b.graph_update_block(block);
        //b.fun.graph_validate_block(block);
    }
}

impl OpBuild for Case {
    type Token = CaseToken;
}

pub fn register(dialect: &mut Dialect) {
    dialect.register_op::<Case>();
    dialect.register_op_branches_impl(&Case {
        inner: Box::new(Inner {
            container: PatternContainer::new(),
            clauses: Vec::new(),
        }),
    });
}
