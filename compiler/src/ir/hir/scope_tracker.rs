use ::std::collections::HashMap;
use ::ir::SSAVariable;
use ::Variable;
use ::util::ssa_variable::SSAVariableGenerator;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LambdaEnvIdx(pub usize);

#[derive(Debug)]
pub struct LambdaEnv {
    pub captures: Vec<(ScopeDefinition, SSAVariable, SSAVariable)>,
    pub meta_binds: Vec<(::ir::FunctionIdent, SSAVariable)>,
}

#[derive(Debug)]
enum Scope {
    /// A simple scope contains one or more variable assignments
    Simple(HashMap<ScopeDefinition, SSAVariable>),
    /// A tracking scope contains no assignments, but tracks variables
    /// that are referenced across it. Used to capture variables for
    /// closures.
    Tracking(HashMap<ScopeDefinition, (SSAVariable, SSAVariable)>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ScopeDefinition {
    Variable(Variable),
    Function(::parser::FunctionName),
}

#[derive(Debug)]
pub struct ScopeTracker {
    scopes: Vec<Scope>,
    ssa_var_generator: SSAVariableGenerator,
    lambda_envs: Vec<LambdaEnv>,
}
impl ScopeTracker {

    pub fn new() -> Self {
        ScopeTracker {
            scopes: vec![],
            ssa_var_generator: SSAVariableGenerator::initial(),
            lambda_envs: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, bindings: HashMap<ScopeDefinition, SSAVariable>) {
        self.scopes.push(Scope::Simple(bindings));
    }
    pub fn push_tracking(&mut self) {
        self.scopes.push(Scope::Tracking(HashMap::new()));
    }
    pub fn pop_scope(&mut self) {
        match self.scopes.pop().unwrap() {
            Scope::Simple(_) => (),
            _ => panic!(),
        }
    }
    pub fn pop_tracking(&mut self) -> HashMap<ScopeDefinition, (SSAVariable, SSAVariable)> {
        match self.scopes.pop().unwrap() {
            Scope::Tracking(tracked) => {
                tracked
            },
            _ => panic!(),
        }
    }

    pub fn new_ssa(&mut self) -> SSAVariable {
        self.ssa_var_generator.next()
    }

    pub fn get(&mut self, var: &ScopeDefinition) -> Option<SSAVariable> {
        let mut ssa_var_root: Option<SSAVariable> = None;
        let mut end_idx: usize = 0;

        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if let &Scope::Simple(ref bindings) = scope {
                if let Some(ssa) = bindings.get(var) {
                    ssa_var_root = Some(*ssa);
                    end_idx = idx;
                    break;
                }
            }
        }
        let scopes_len = self.scopes.len();
        end_idx = scopes_len - end_idx;

        if let Some(mut ssa_var) = ssa_var_root {
            for scope in &mut self.scopes[end_idx..scopes_len] {
                if let &mut Scope::Tracking(ref mut escapes) = scope {
                    if let Some(&(outer_ssa, inner_ssa)) = escapes.get(var) {
                        assert!(ssa_var == outer_ssa);
                        ssa_var = inner_ssa;
                    } else {
                        let inner_ssa = self.ssa_var_generator.next();
                        escapes.insert(var.clone(), (ssa_var, inner_ssa));
                        ssa_var = inner_ssa;
                    }
                }
            }
            Some(ssa_var)
        } else {
            None
        }
    }

    pub fn next_env_idx(&self) -> LambdaEnvIdx {
        LambdaEnvIdx(self.lambda_envs.len())
    }

    pub fn add_lambda_env(&mut self, env_idx: LambdaEnvIdx, env: LambdaEnv) {
        assert!(env_idx.0 == self.lambda_envs.len());
        self.lambda_envs.push(env);
    }

    pub fn get_lambda_env<'a>(&'a self, env_idx: LambdaEnvIdx)
                          -> &'a LambdaEnv {
        &self.lambda_envs[env_idx.0]
    }

    pub fn finish(self) -> Vec<LambdaEnv> {
        self.lambda_envs
    }

}
