
use std::rc::Rc;
use crate::term::{ Term, Pid };

use libeir_ir::{ FunctionIdent, Block, Value };
use libeir_intern::Symbol;

use std::sync::{ Mutex, RwLock };
use std::collections::HashMap;
use std::borrow::BorrowMut;

use ::serde::{ Serialize };

#[derive(Serialize)]
#[serde(tag = "ph")]
enum TraceEntry {
    #[serde(rename = "B")]
    DurationStart {
        name: String,
        #[serde(rename = "cat")]
        categories: String,
        #[serde(rename = "ts")]
        timestamp: u64,
        pid: u64,
        tid: u64,
        cname: Option<String>,
        args: HashMap<String, ::serde_json::Value>,
    },
    #[serde(rename = "E")]
    DurationEnd {
        #[serde(rename = "ts")]
        timestamp: u64,
        pid: u64,
        tid: u64,
        args: HashMap<String, ::serde_json::Value>,
    },
    #[serde(rename = "i")]
    Instant {
        name: String,
        #[serde(rename = "ts")]
        timestamp: u64,
        pid: u64,
        tid: u64,
        #[serde(rename = "s")]
        scope: &'static str,
        args: HashMap<String, ::serde_json::Value>,
    }
}

lazy_static::lazy_static! {
    static ref VM_ID_COUNTER: Mutex<usize> = Mutex::new(0);
}

std::thread_local! {
    static VM_ID: RwLock<Option<usize>> = RwLock::new(None);
    static TRACE_COLLECTOR: Mutex<TraceCollector> = Mutex::new(TraceCollector::new());
}

pub fn gen_vm_id() {
    let counter = VM_ID_COUNTER.lock().unwrap();
    VM_ID.with(|v| {
        let mut vm_id = v.write().unwrap();
        *vm_id = Some(*counter);
    });
}

fn vm_id() -> usize {
    VM_ID.with(|v| {
        v.read().unwrap().unwrap()
    })
}

struct StackEntry {
    pid: Pid,
    module: Symbol,
    ident: FunctionIdent,
    args: Vec<Term>,
}

struct TraceEvent {
    pid: Pid,
    typ: TraceEventType,
}

enum TraceEventType {
    FunctionEnter {
        ident: FunctionIdent,
        lambda: Option<Block>,
        args: Vec<Rc<Term>>,
    },
    FunctionTailCall,
    FunctionReturn {
        kind: usize,
        ret: Rc<Term>,
    },
    BlockStart {
        ident: FunctionIdent,
        block: Block,
    },
    BlockEnd,
    Warning {
        text: String,
        args: HashMap<String, ::serde_json::Value>,
    },
}

struct TraceCollector {
    current_pid: Pid,
    pid_stacks: HashMap<Pid, Vec<StackEntry>>,
    events: Vec<TraceEvent>,
}

impl TraceCollector {

    fn new() -> Self {
        TraceCollector {
            current_pid: Pid(0),
            pid_stacks: HashMap::new(),
            events: Vec::new(),
        }
    }

}

pub fn set_pid(pid: Pid) {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        c.current_pid = pid;
    })
}

pub fn get_pid() -> Pid {
    TRACE_COLLECTOR.with(|c| {
        let c = c.lock().unwrap();
        c.current_pid
    })
}

pub fn enter_function(ident: &FunctionIdent, lambda: Option<Block>, args: &[Rc<Term>]) {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        let pid = c.current_pid;
        if !c.pid_stacks.contains_key(&pid) {
            c.pid_stacks.insert(pid, Vec::new());
        }
        {
            let stack = c.pid_stacks.get_mut(&pid).unwrap();
            stack.push(StackEntry {
                pid: pid,
                module: module.clone(),
                ident: ident.clone(),
                args: args.to_vec(),
            });
        }
        c.events.push(TraceEvent {
            pid: pid,
            typ: TraceEventType::FunctionEnter {
                ident: ident.clone(),
                args: args.to_vec(),
            },
        });
    })
}

pub fn exit_function(ident: &FunctionIdent, ret: Option<&CallReturn>) {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        let pid = c.current_pid;
        {
            let stack = c.pid_stacks.get_mut(&pid).unwrap();
            let removed = stack.pop().unwrap();
            assert!(&removed.module == module);
            assert!(&removed.ident == ident);
        }
        c.events.push(TraceEvent {
            pid: pid,
            typ: TraceEventType::FunctionExit {
                ret: ret.cloned(),
            },
        });
    })
}

pub fn start_basic_block(module: &Atom, ident: &FunctionIdent, block: LabelN) {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        let pid = c.current_pid;
        c.events.push(TraceEvent{
            pid: pid,
            typ: TraceEventType::BasicBlockStart {
                module: module.clone(),
                ident: ident.clone(),
                block: block,
            },
        });
    })
}

pub fn end_basic_block() {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        let pid = c.current_pid;
        c.events.push(TraceEvent{
            pid: pid,
            typ: TraceEventType::BasicBlockEnd,
        });
    })
}

pub fn warning(text: String) {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        let pid = c.current_pid;
        c.events.push(TraceEvent {
            pid: pid,
            typ: TraceEventType::Warning {
                text: text,
                args: HashMap::new(),
            }
        });
    })
}

pub fn warning_args<F>(text: String, make_args: F) where F: FnOnce() -> HashMap<String, ::serde_json::Value> {
    TRACE_COLLECTOR.with(|c| {
        let mut c = c.lock().unwrap();
        let pid = c.current_pid;
        c.events.push(TraceEvent {
            pid: pid,
            typ: TraceEventType::Warning {
                text: text,
                args: (make_args)()
            }
        });
    })
}

pub fn dump_trace(filename: String) {
    let json = TRACE_COLLECTOR.with(|c| {
        let c = c.lock().unwrap();
        let mut idx = 0;
        let json_events: Vec<_> = c.events.iter()
            .map(|event| {
                match &event.typ {
                    TraceEventType::FunctionEnter { ident, args, .. } => {
                        let mut event_args = HashMap::new();
                        let fun_args: Vec<_> = args.iter()
                            .map(|a| ::serde_json::Value::String(
                                format!("{}", a.to_doc().pretty(40))))
                            .collect();
                        event_args.insert(
                            "Call Arguments".to_string(),
                            ::serde_json::Value::Array(fun_args)
                        );
                        TraceEntry::DurationStart {
                            timestamp: idx as u64,
                            pid: event.pid.0 as u64,
                            tid: 0,
                            cname: None,
                            args: event_args,
                            name: format!("{}", ident),
                            categories: "".to_string(),
                        }
                    }
                    TraceEventType::FunctionExit { ret } => {
                        if ret.is_some() { idx += 1; }
                        let mut event_args = HashMap::new();
                        event_args.insert(
                            "Call Return".to_string(),
                            match ret {
                                None => ::serde_json::Value::String(
                                    "TailCall".to_string()),
                                Some(CallReturn::Return{ term: val }) =>
                                    ::serde_json::Value::String(
                                        format!("{}", val.to_doc().pretty(40))),
                                Some(CallReturn::Throw) =>
                                    ::serde_json::Value::String("Throw".to_string()),
                            }
                        );
                        TraceEntry::DurationEnd {
                            timestamp: idx as u64,
                            pid: event.pid.0 as u64,
                            tid: 0,
                            args: event_args,
                        }
                    }
                    TraceEventType::BasicBlockStart { module, ident, block } => {
                        TraceEntry::DurationStart {
                            timestamp: idx as u64,
                            pid: event.pid.0 as u64,
                            tid: 1,
                            cname: None,
                            args: HashMap::new(),
                            name: format!("{} ({}:{})", block, module, ident),
                            categories: "".to_string(),
                        }
                    }
                    TraceEventType::BasicBlockEnd => {
                        idx += 1;
                        TraceEntry::DurationEnd {
                            timestamp: idx as u64,
                            pid: event.pid.0 as u64,
                            tid: 1,
                            args: HashMap::new(),
                        }
                    }
                    TraceEventType::Warning { text, args } => {
                        TraceEntry::Instant {
                            name: text.clone(),
                            timestamp: idx as u64,
                            pid: event.pid.0 as u64,
                            tid: 0,
                            scope: "p",
                            args: args.clone(),
                        }
                    }
                }
            }).collect();
        serde_json::to_string(&json_events)
    });

    ::std::fs::write(filename, json.unwrap()).unwrap();
}





