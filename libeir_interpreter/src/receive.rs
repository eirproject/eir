use ::term::Term;

#[derive(Debug)]
pub struct ReceiveContext {
}

impl ReceiveContext {

    pub fn new(timeout: Term) -> Self {
        // TODO: Timeout
        ReceiveContext {
        }
    }

}
