use std::time::Instant;
use std::thread::ThreadId;

pub struct Event {
    instant: Instant,
    thread: ThreadId,
    kind: EventKind,
}

pub enum EventKind {
    Begin {
    },
    End {
    },
}

pub struct Tracer {
    start: Instant,
    events: Vec<Event>,
}
