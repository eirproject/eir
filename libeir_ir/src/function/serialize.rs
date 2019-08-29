use super::Function;

use serde::{ Serialize, Serializer, Deserialize };

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut value_n = 0;
        let mut block_n = 0;
    }
}
