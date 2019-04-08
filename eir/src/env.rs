use cranelift_entity::{ EntityRef, PrimaryMap, entity_impl };

use super::FunctionIdent;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClosureEnv(u32);
entity_impl!(ClosureEnv, "lambda_env");

impl ClosureEnv {

    pub fn from_num(num: usize) -> ClosureEnv {
        ClosureEnv::new(num)
    }

}

#[derive(Debug, Clone)]
struct ClosureEnvData {
    num_captures: Option<usize>,
    meta_binds: Vec<FunctionIdent>
}

#[derive(Debug, Clone)]
pub struct ModuleEnvs {
    envs: PrimaryMap<ClosureEnv, ClosureEnvData>,
}

impl ModuleEnvs {

    pub fn new() -> Self {
        ModuleEnvs {
            envs: PrimaryMap::new(),
        }
    }

    pub fn add(&mut self) -> ClosureEnv {
        self.envs.push(ClosureEnvData {
            num_captures: None,
            meta_binds: Vec::new(),
        })
    }

    pub fn env_set_captures_num(&mut self, env: ClosureEnv, num: usize) {
        self.envs[env].num_captures = Some(num);
    }

    pub fn env_get_captures_num(&mut self, env: ClosureEnv) -> usize {
        self.envs[env].num_captures.unwrap()
    }

    pub fn env_add_meta_bind(&mut self, env: ClosureEnv, ident: FunctionIdent) {
        self.envs[env].meta_binds.push(ident)
    }

}
