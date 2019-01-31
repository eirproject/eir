#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SSAVariable(u32);
impl ::std::fmt::Debug for SSAVariable {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if self == &INVALID_SSA {
            write!(f, "%INVALID")
        } else {
            write!(f, "%{}", self.0)
        }
    }
}
pub const INVALID_SSA: SSAVariable = SSAVariable(0);

#[derive(Debug)]
pub struct SSAVariableGenerator(SSAVariable);

impl SSAVariableGenerator {

    pub fn initial() -> Self {
        SSAVariableGenerator(SSAVariable(1))
    }

    pub fn next(&mut self) -> SSAVariable {
        let ret = self.0;
        //if ret.0 == 5251 {
        //    panic!()
        //}
        (self.0).0 += 1;
        ret
    }

}
