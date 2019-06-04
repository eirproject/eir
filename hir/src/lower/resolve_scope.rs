use ::std::collections::HashMap;
use ::Atom;
use ::eir::FunctionIdent;
use ::eir::{ ModuleEnvs, ClosureEnv };
use crate::hir::{ HirModule, Function, FunctionRef, Expr, Variable, ExprKind };
use eir::pattern::{ PatternValue, PatternNode };

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedVar(usize);
impl ResolvedVar {
    fn next(&mut self) -> Self {
        let n = *self;
        self.0 += 1;
        n
    }
}

#[derive(Debug)]
struct Scope {
    /// A simple scope contains one or more variable assignments
    bindings: HashMap<ScopeDefinition, ResolvedVar>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ScopeDefinition {
    Variable(Atom),
    Function(Atom, usize),
}

#[derive(Debug)]
pub struct ScopeTracker {
    scopes: Vec<Scope>,
}
impl ScopeTracker {

    // TODO: Reduce allocations

    pub fn new() -> Self {
        ScopeTracker {
            scopes: vec![],
        }
    }

    pub fn push_scope<'a>(&'a mut self) -> &'a mut HashMap<ScopeDefinition, ResolvedVar> {
        let bindings = HashMap::new();
        self.scopes.push(Scope { bindings });
        &mut self.scopes.last_mut().unwrap().bindings
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn get(&mut self, var: &ScopeDefinition) -> Option<ResolvedVar> {
        for scope in self.scopes.iter().rev() {
            if let Some(ssa) = scope.bindings.get(var) {
                return Some(*ssa);
            }
        }

        None
    }

}

pub struct ScopeResolver {
    tracker: ScopeTracker,
    map: ScopeMap,
}

pub struct ScopeMap {
    next_var: ResolvedVar,

    resolved: HashMap<Node, ResolvedVar>,
    /// If the ref refers to an externally captured function,
    /// it is in this map.
    external: HashMap<ResolvedVar, FunctionRef>,
}

impl ScopeMap {

    pub fn new() -> Self {
        ScopeMap {
            next_var: ResolvedVar(0),

            resolved: HashMap::new(),
            external: HashMap::new(),
        }
    }

    pub fn gen_var(&mut self) -> ResolvedVar {
        self.next_var.next()
    }

    pub fn put_assign(&mut self, node: Node) -> ResolvedVar {
        let res = self.next_var.next();

        assert!(!self.resolved.contains_key(&node));
        self.resolved.insert(node, res);

        res
    }

    pub fn put_external_fun(&mut self, fun: FunctionRef) -> ResolvedVar {
        let res = self.next_var.next();
        self.external.insert(res, fun);
        res
    }

    pub fn put_resolve(&mut self, node: Node, res: ResolvedVar) {
        assert!(!self.resolved.contains_key(&node));
        self.resolved.insert(node, res);
    }

    pub fn get(&self, node: Node) -> Option<ResolvedVar> {
        self.resolved.get(&node).cloned()
    }

    pub fn is_ext_fun(&self, node: ResolvedVar) -> Option<FunctionRef> {
        self.external.get(&node).cloned()
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Variable(Variable),
    Function(Function),
    FunctionRef(FunctionRef),
    Expr(Expr),
    PatternValue(PatternValue),
    PatternBind(PatternNode),
}


impl ScopeResolver {

    pub fn new() -> Self {
        ScopeResolver {
            tracker: ScopeTracker::new(),
            map: ScopeMap::new(),
        }
    }

    pub fn resolve(&mut self, hir: &HirModule, fun: Function) -> ScopeMap {
        self.resolve_fun(hir, fun);

        let mut map = ScopeMap::new();
        std::mem::swap(&mut self.map, &mut map);

        map
    }

    fn resolve_fun(&mut self, hir: &HirModule, fun: Function) {
        self.do_resolve_scope(hir, hir.function_expr(fun), hir.function_args(fun));
    }

    fn do_resolve_scope(&mut self, hir: &HirModule, expr: Expr, bound: &[Variable]) -> ResolvedVar {
        {
            let scope = self.tracker.push_scope();
            for var in bound {
                let res = self.map.put_assign(Node::Variable(*var));
                if let Some(name) = hir.variable_name(*var) {
                    scope.insert(
                        ScopeDefinition::Variable(name.clone()),
                        res
                    );
                }
            }
        }

        let res = self.do_resolve_node(hir, expr);

        self.tracker.pop_scope();
        res
    }

    fn do_resolve_node(&mut self, hir: &HirModule, expr: Expr) -> ResolvedVar {
        use crate::hir::ExprKind as EK;

        let kind = hir.expr_kind(expr);

        let res = match kind {
            EK::Variable(var) => {
                let node = Node::Variable(*var);
                if let Some(name) = hir.variable_name(*var) {
                    let resolved = self.tracker
                        .get(&ScopeDefinition::Variable(name.clone()))
                        .unwrap();
                    self.map.put_resolve(node, resolved);
                    resolved
                } else {
                    // Anonymous variables are resolved directly by value.
                    // No scope checks are performed here.
                    self.map.get(node).unwrap()
                }

            }
            EK::Function(fun_ref) => {
                let data = hir.function_ref_data(*fun_ref);

                // If we refer to a local function, try to resolve
                // a closure
                if data.module.is_none() {
                    let scope_def = ScopeDefinition::Function(
                        data.name.clone(), data.arity);
                    if let Some(res) = self.tracker.get(&scope_def) {
                        return res;
                    }
                }

                // If there is no closure, resolve to an external function
                // capture
                self.map.put_external_fun(*fun_ref)
            }
            EK::LetRec { closures, body } => {
                // Push a scope and assign function names and variable
                // bindings.
                {
                    let scope = self.tracker.push_scope();

                    for closure in closures.as_slice(&hir.function_pool) {
                        let res = self.map.put_assign(Node::Function(*closure));
                        let data = hir.function_data(*closure);

                        // If the function is actually bound to a name, insert
                        // that name binding in the current scope
                        if let Some(name) = &data.name {
                            scope.insert(ScopeDefinition::Function(
                                name.clone(), data.args.len(&hir.variable_pool)), res);
                        }

                        // Variable binding
                        if let Some(var) = data.var_binding {
                            if let Some(name) = hir.variable_name(var) {
                                scope.insert(
                                    ScopeDefinition::Variable(name.clone()), res);
                            }
                            self.map.put_resolve(Node::Variable(var), res);
                        }
                    }
                }

                // Resolve all within newly created scope
                for closure in closures.as_slice(&hir.function_pool) {
                    self.resolve_fun(hir, *closure);
                }
                let res = self.do_resolve_node(hir, *body);

                self.tracker.pop_scope();
                res
            }
            EK::Atomic(_) => {
                self.map.gen_var()
            }
            EK::Tuple { elems } => {
                for sub in elems.as_slice(&hir.expr_pool) {
                    self.do_resolve_node(hir, *sub);
                }
                self.map.gen_var()
            }
            EK::List { head, tail } => {
                for sub in head.as_slice(&hir.expr_pool) {
                    self.do_resolve_node(hir, *sub);
                }
                self.do_resolve_node(hir, *tail);
                self.map.gen_var()
            }
            EK::Map { merge, keys, values, .. } => {
                if let Some(merge) = merge {
                    self.do_resolve_node(hir, *merge);
                }
                for (key, value) in keys.as_slice(&hir.expr_pool).iter().zip(
                    values.as_slice(&hir.expr_pool).iter()) {

                    self.do_resolve_node(hir, *key);
                    self.do_resolve_node(hir, *value);
                }
                self.map.gen_var()
            }
            EK::ValueList { elems } => {
                for sub in elems.as_slice(&hir.expr_pool) {
                    self.do_resolve_node(hir, *sub);
                }
                self.map.gen_var()
            }
            EK::PrimOp { args, .. } => {
                for sub in args.as_slice(&hir.expr_pool) {
                    self.do_resolve_node(hir, *sub);
                }
                self.map.gen_var()
            }
            EK::Apply { args, .. } => {
                for sub in args.as_slice(&hir.expr_pool) {
                    self.do_resolve_node(hir, *sub);
                }
                self.map.gen_var()
            }
            EK::Let { assigns, value, body } => {
                self.do_resolve_node(hir, *value);
                self.do_resolve_scope(hir, *body, assigns.as_slice(&hir.variable_pool))
            }
            EK::Do { bodies } => {
                let mut last = None;
                for body in bodies.as_slice(&hir.expr_pool) {
                    last = Some(self.do_resolve_node(hir, *body));
                }
                last.unwrap()
            }
            EK::Try { body, then_vars, then, catch_vars, catch } => {
                self.do_resolve_node(hir, *body);
                self.do_resolve_scope(
                    hir, *then, then_vars.as_slice(&hir.variable_pool));
                self.do_resolve_scope(
                    hir, *catch, catch_vars.as_slice(&hir.variable_pool));
                self.map.gen_var()
            }
            EK::Case { match_on, clauses } => {
                self.do_resolve_node(hir, *match_on);
                for clause in clauses.as_slice(&hir.clause_pool) {
                    // TODO
                }
                unimplemented!();
                self.map.gen_var()
            }
            _ => unimplemented!(),
        };

        self.map.put_resolve(Node::Expr(expr), res);

        res
    }

}






