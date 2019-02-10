use std::fmt::{ Debug, Formatter, Result };
use ::ToDoc;
use ::ir::{ Module };

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.to_doc().render_fmt(80, f)
    }
}
