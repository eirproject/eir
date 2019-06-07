use std::collections::HashMap;

use cranelift_entity::{ PrimaryMap, SecondaryMap, EntityList, ListPool, entity_impl };
use cranelift_entity::packed_option::ReservedValue;

use libeir_ir::pattern::{ PatternNode, PatternValue, PatternClause, PatternContainer };
use libeir_ir::FunctionIdent;

use libeir_diagnostics::{ ByteSpan, DUMMY_SPAN };
use libeir_intern::{ Ident, Symbol };

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expr(u32);
entity_impl!(Expr, "expr");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable(u32);
entity_impl!(Variable, "variable");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function(u32);
entity_impl!(Function, "function");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionRef(u32);
entity_impl!(FunctionRef, "function_ref");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clause(u32);
entity_impl!(Clause, "clause");

struct ExprData {
    kind: ExprKind,

    finished: bool,
    pub span: ByteSpan,
}

struct VariableData {
    /// This variable is used only for name resolution.
    /// If this is Some, then lexical scope resolving is used.
    /// If this is None, then it is instead resolved directly
    /// by value.
    name: Option<Ident>,

    pub span: Option<Symbol>,
}

pub struct FunctionRefData {
    /// Refers to local if None.
    pub module: Option<Ident>,
    pub name: Ident,
    pub arity: usize,

    pub span: ByteSpan,
}

pub struct FunctionData {
    /// Function name is optional.
    /// A case where a function does not have a name is anonymous
    /// lambdas.
    pub name: Option<Ident>,

    /// In the case of a closure, enables binding the function
    /// value directly to a Variable.
    pub var_binding: Option<Variable>,

    pub args: EntityList<Variable>,
    pub body: Expr,

    /// Whether this function is a lambda and can access variables
    /// from an outer scope.
    /// In the scope resolve pass, lambdas are inlined while
    /// non-lambdas (module local functions or imports) are simply
    /// referenced.
    pub lambda: bool,

    finished: bool,
    pub span: ByteSpan,
}

struct ClauseData {
    nodes: PatternClause,

    values: EntityList<Expr>,

    guard: Expr,
    body: Expr,

    finished: bool,
    span: ByteSpan,
}

pub struct HirModule {
    pub name: Ident,
    span: ByteSpan,

    /// All functions that are in the root of this module.
    root_functions: HashMap<FunctionIdent, Function>,

    exprs: PrimaryMap<Expr, ExprData>,
    variables: PrimaryMap<Variable, VariableData>,
    functions: PrimaryMap<Function, FunctionData>,
    function_refs: PrimaryMap<FunctionRef, FunctionRefData>,
    clauses: PrimaryMap<Clause, ClauseData>,

    pub expr_pool: ListPool<Expr>,
    pub variable_pool: ListPool<Variable>,
    pub function_pool: ListPool<Function>,
    pub clause_pool: ListPool<Clause>,

    pub pattern_container: PatternContainer,
}

impl HirModule {

    pub fn new(name: Ident) -> Self {
        HirModule {
            name,
            span: DUMMY_SPAN,

            root_functions: HashMap::new(),

            exprs: PrimaryMap::new(),
            variables: PrimaryMap::new(),
            functions: PrimaryMap::new(),
            function_refs: PrimaryMap::new(),
            clauses: PrimaryMap::new(),

            expr_pool: ListPool::new(),
            variable_pool: ListPool::new(),
            function_pool: ListPool::new(),
            clause_pool: ListPool::new(),

            pattern_container: PatternContainer::new(),
        }
    }

    pub fn variable_anon(&mut self) -> Variable {
        self.variables.push(VariableData {
            name: None,
            span: None,
        })
    }

    pub fn clause_start(&mut self, pat_clause: PatternClause) -> Clause {
        self.clauses.push(ClauseData {
            nodes: pat_clause,

            values: EntityList::new(),

            guard: Expr::reserved_value(),
            body: Expr::reserved_value(),

            finished: false,
            span: DUMMY_SPAN,
        })
    }
    pub fn clause_set_body(&mut self, clause: Clause, body: Expr) {
        let data = &mut self.clauses[clause];
        assert!(!data.finished);
        data.body = body;
    }
    pub fn clause_set_guard(&mut self, clause: Clause, guard: Expr) {
        let data = &mut self.clauses[clause];
        assert!(!data.finished);
        data.guard = guard;
    }

    pub fn function_start(&mut self) -> Function {
        self.functions.push(FunctionData {
            name: None,
            var_binding: None,

            args: EntityList::new(),
            body: Expr::reserved_value(),

            lambda: false,

            span: DUMMY_SPAN,

            finished: false,
        })
    }
    pub fn function_set_name(&mut self, fun: Function, name: Option<Ident>) {
        let data = &mut self.functions[fun];
        assert!(!data.finished);
        data.name = name;
    }
    pub fn function_set_body(&mut self, fun: Function, body: Expr) {
        let data = &mut self.functions[fun];
        assert!(!data.finished);
        data.body = body;
    }
    pub fn function_push_arg(&mut self, fun: Function, arg: Variable) {
        let data = &mut self.functions[fun];
        assert!(!data.finished);
        data.args.push(arg, &mut self.variable_pool);
    }
    pub fn function_finish(&mut self, fun: Function) {
        let data = &mut self.functions[fun];
        assert!(!data.finished);
        assert!(data.body != Expr::reserved_value());
        data.finished = true;
    }

    pub fn expr_variable(&mut self, var: Variable) -> Expr {
        self.exprs.push(ExprData {
            span: DUMMY_SPAN,
            kind: ExprKind::Variable(var),
            finished: true,
        })
    }

    pub fn expr_function(&mut self, fun: FunctionRef) -> Expr {
        self.exprs.push(ExprData {
            span: DUMMY_SPAN,
            kind: ExprKind::Function(fun),
            finished: true,
        })
    }

    pub fn expr_letrec_start(&mut self, body: Expr) -> Expr {
        self.exprs.push(ExprData {
            span: DUMMY_SPAN,
            kind: ExprKind::LetRec {
                body,
                closures: EntityList::new(),
            },
            finished: false,
        })
    }

    pub fn expr_value_list_start(&mut self) -> Expr {
        self.exprs.push(ExprData {
            span: DUMMY_SPAN,
            kind: ExprKind::ValueList {
                elems: EntityList::new(),
            },
            finished: false,
        })
    }
    pub fn expr_value_list_push(&mut self, val_list: Expr, val: Expr) {
        let data = &mut self.exprs[val_list];
        assert!(!data.finished);
        match data.kind {
            ExprKind::ValueList { ref mut elems } => {
                elems.push(val, &mut self.expr_pool);
            }
            _ => panic!(),
        }
    }

    pub fn expr_case_start(&mut self, match_on: Expr) -> Expr {
        self.exprs.push(ExprData {
            span: DUMMY_SPAN,
            kind: ExprKind::Case {
                match_on,
                clauses: EntityList::new(),
            },
            finished: false,
        })
    }
    pub fn expr_case_push_clause(&mut self, case: Expr, clause: Clause) {
        let data = &mut self.exprs[case];
        assert!(!data.finished);
        match data.kind {
            ExprKind::Case { ref mut clauses, .. } => {
                clauses.push(clause, &mut self.clause_pool);
            }
            _ => panic!(),
        }
    }

    pub fn expr_finish(&mut self, expr: Expr) {
        let data = &mut self.exprs[expr];
        assert!(!data.finished);
        data.finished = true;
    }

}

impl HirModule {

    pub fn expr_kind<'a>(&'a self, expr: Expr) -> &'a ExprKind {
        &self.exprs[expr].kind
    }

    pub fn function_name<'a>(&'a self, fun: Function) -> Ident {
        self.functions[fun].name.unwrap()
    }

    pub fn function_args<'a>(&'a self, fun: Function) -> &'a [Variable] {
        &self.functions[fun].args.as_slice(&self.variable_pool)
    }

    pub fn function_expr(&self, fun: Function) -> Expr {
        self.functions[fun].body
    }

    pub fn function_data<'a>(&'a self, fun: Function) -> &'a FunctionData {
        &self.functions[fun]
    }

    pub fn variable_name<'a>(&'a self, variable: Variable) -> Option<&'a Ident> {
        self.variables[variable].name.as_ref()
    }

    pub fn function_ref_data<'a>(&'a self, fun_ref: FunctionRef) -> &'a FunctionRefData {
        &self.function_refs[fun_ref]
    }

    pub fn clause_values<'a>(&'a self, clause: Clause) -> &'a [Expr] {
        self.clauses[clause].values.as_slice(&self.expr_pool)
    }

    pub fn clause_pattern_clause(&self, clause: Clause) -> PatternClause {
        self.clauses[clause].nodes
    }

    pub fn clause_guard_expr(&self, clause: Clause) -> Expr {
        self.clauses[clause].guard
    }
    pub fn clause_body_expr(&self, clause: Clause) -> Expr {
        self.clauses[clause].body
    }

    pub fn function_ref_module<'a>(&'a self, fun_ref: FunctionRef) -> Option<Ident> {
        self.function_refs[fun_ref].module
    }
    pub fn function_ref_name<'a>(&'a self, fun_ref: FunctionRef) -> Ident {
        self.function_refs[fun_ref].name
    }
    pub fn function_ref_arity(&self, fun_ref: FunctionRef) -> usize {
        self.function_refs[fun_ref].arity
    }

}

pub enum MapAssoc {
    /// Manditory association
    Exact,
    /// Optional association
    Assoc,
}

pub enum ExprKind {

    Sentinel,

    // Scope access
    Variable(Variable),
    Function(FunctionRef),

    // Closures
    LetRec {
        closures: EntityList<Function>,
        body: Expr,
    },

    // Value constructors
    Atomic(libeir_ir::AtomicTerm),
    Tuple {
        elems: EntityList<Expr>,
    },
    List {
        head: EntityList<Expr>,
        tail: Expr,
    },
    Map {
        merge: Option<Expr>,

        assoc: Vec<MapAssoc>,
        keys: EntityList<Expr>,
        values: EntityList<Expr>,
    },
    // TODO: Binary
    ValueList {
        elems: EntityList<Expr>,
    },

    // Calls
    PrimOp {
        name: Symbol,
        args: EntityList<Expr>,
    },
    Apply {
        fun: Expr,
        args: EntityList<Expr>,
    },

    // Combinators
    /// assigns <- value in body
    Let {
        assigns: EntityList<Variable>,
        value: Expr,
        body: Expr,
    },
    Do {
        bodies: EntityList<Expr>,
    },
    Try {
        body: Expr,

        then_vars: EntityList<Variable>,
        then: Expr,

        catch_vars: EntityList<Variable>,
        catch: Expr,
    },

    // Matching structures
    Case {
        match_on: Expr,
        clauses: EntityList<Clause>,
    },
    Receive {
        clauses: EntityList<Clause>,

        timeout_time: Expr,
        timeout_body: Expr,
    }

}

