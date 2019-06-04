use eir::Atom;
use cranelift_entity::{ PrimaryMap, SecondaryMap, EntityList, ListPool, entity_impl };

use eir::pattern::{ PatternNode, PatternValue, PatternClause, PatternContainer };

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
}

struct VariableData {
    name: Option<Atom>,
}

pub struct FunctionRefData {
    /// Refers to local if None.
    pub module: Option<Atom>,
    pub name: Atom,
    pub arity: usize,
}

pub struct FunctionData {
    pub name: Option<Atom>,

    /// In the case of a closure, enables binding the function
    /// value directly to a Variable.
    pub var_binding: Option<Variable>,

    pub args: EntityList<Variable>,
    pub body: Expr,

    /// Whether this function is a lambda and can access variables
    /// from an outer scope.
    /// In the scope resolve pass, lambdas are inlined while
    /// non-lambdas (module local functions) are simply referenced.
    pub lambda: bool,
}

struct ClauseData {
    nodes: PatternClause,

    values: EntityList<Expr>,

    guard: Expr,
    body: Expr,
}

pub struct HirModule {
    pub name: Atom,

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
    pattern_values: SecondaryMap<PatternValue, Variable>,
    pattern_binds: SecondaryMap<Variable, PatternNode>,

    //closure_pool: EntityPool<Closure>,
}

impl HirModule {

    pub fn expr_kind<'a>(&'a self, expr: Expr) -> &'a ExprKind {
        &self.exprs[expr].kind
    }

    pub fn function_name<'a>(&'a self, fun: Function) -> &'a Atom {
        &self.functions[fun].name.as_ref().unwrap()
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

    pub fn variable_name<'a>(&'a self, variable: Variable) -> Option<&'a Atom> {
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

    pub fn function_ref_module<'a>(&'a self, fun_ref: FunctionRef) -> Option<&'a Atom> {
        self.function_refs[fun_ref].module.as_ref()
    }
    pub fn function_ref_name<'a>(&'a self, fun_ref: FunctionRef) -> &'a Atom {
        &self.function_refs[fun_ref].name
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

mod kind {

    use eir::{ Atom, AtomicTerm };
    use cranelift_entity::{ PrimaryMap, SecondaryMap, EntityList, ListPool, entity_impl };
    use super::{ Variable as SVariable, Expr, Function };

    pub struct Variable {
        variable: SVariable
    }

    pub struct LocalFunction {
        name: Atom,
        arity: usize,
    }

    pub struct CaptureFunction {
        module: Expr,
        name: Expr,
        arity: Expr,
    }

    pub struct Closure {
        closure: Function,
    }

    pub struct LetRec {
        closures: EntityList<Function>,
    }

    pub struct Atomic {
        atomic: AtomicTerm,
    }

}

pub enum ExprKind {

    // Scope access
    Variable(Variable),
    Function(FunctionRef),

    // Functions
    LetRec {
        closures: EntityList<Function>,
        body: Expr,
    },

    // Value constructors
    Atomic(::eir::AtomicTerm),
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
        name: Atom,
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

#[derive(Debug, Copy, Clone)]
struct CaseExpr(Expr);
impl Into<Expr> for CaseExpr {
    fn into(self) -> Expr {
        self.0
    }
}
impl CaseExpr {
    fn clause() {
    }
}
