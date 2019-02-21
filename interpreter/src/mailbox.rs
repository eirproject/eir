use ::term::{ Term, Pid };

#[derive(Debug)]
pub struct Mailbox {
    trap_exits: bool,
    messages: Vec<(Pid, Term)>,
}

impl Mailbox {
    pub fn new() -> Self {
        Mailbox {
            trap_exits: false,
            messages: vec![],
        }
    }
    pub fn get_trap_exits(&self) -> bool {
        self.trap_exits
    }
    pub fn set_trap_exits(&mut self, val: bool) {
        self.trap_exits = val;
    }
}
