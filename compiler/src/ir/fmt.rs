use std::fmt::{ Display, Debug, Formatter, Result };
use ::ToDoc;
use ::ir::{ FunctionIdent, Module };

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.to_doc().render_fmt(80, f)
    }
}

impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(lambda_num) = self.lambda {
            write!(f, "{}@{}/{}", self.name, lambda_num.0, self.arity)
        } else {
            write!(f, "{}/{}", self.name, self.arity)
        }
    }
}
