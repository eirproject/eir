use std::collections::HashMap;

use libeir_util_datastructures::hashmap_stack::HashMapStack;
use libeir_intern::Ident;
use libeir_diagnostics::{ByteSpan, Diagnostic, Label};
use libeir_util_parse::{ErrorReceiver, ToDiagnostic};

use num_traits::ToPrimitive;

use snafu::Snafu;

use crate::{Module, FunctionIdent, Function, FunctionBuilder};
use crate::{Block, Value};
use crate::text::ast;

type ErrCollector<'a> = &'a mut dyn ErrorReceiver<LowerError, LowerError>;

#[derive(Debug, Snafu)]
pub enum LowerError {

    DuplicateDefinititon {
        previous: ByteSpan,
        current: ByteSpan,
    },

    LabelNotFinalized {
        span: ByteSpan,
    },

    OpOutsideOfLabel {
    },

    UndefinedVariable {
        span: ByteSpan,
    },

    UndefinedBlock {
        span: ByteSpan,
    },

}

impl ToDiagnostic for LowerError {
    fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            LowerError::DuplicateDefinititon { previous, current } => {
                Diagnostic::new_error("duplicate identifier definition")
                    .with_label(
                        Label::new_primary(*current)
                            .with_message("attempted redefinition")
                    )
                    .with_label(
                        Label::new_primary(*previous)
                            .with_message("previously defined here")
                    )
            },
            LowerError::LabelNotFinalized { span } => {
                Diagnostic::new_error("label not finalized")
                    .with_label(
                        Label::new_primary(*span)
                            .with_message("block does not end with an operation")
                    )
            },
            LowerError::UndefinedVariable { span } => {
                Diagnostic::new_error("undefined variable name")
                    .with_label(
                        Label::new_primary(*span)
                            .with_message("variable name was not defined in the IR")
                    )
            },
            LowerError::UndefinedBlock { span } => {
                Diagnostic::new_error("undefined block name")
                    .with_label(
                        Label::new_primary(*span)
                            .with_message("block name was not defined in the IR")
                    )
            },
            _ => Diagnostic::new_error(msg),
        }
    }
}

pub struct LowerMap {
    map: HashMap<Name, Value>,
    block_map: HashMap<Ident, Block>,
}
impl LowerMap {

    pub fn get_value(&self, ident: &str) -> Value {
        self.map[&Name::Value(Ident::from_str(ident))]
    }

    pub fn get_block(&self, ident: &str) -> Block {
        self.block_map[&Ident::from_str(ident)]
    }

}

impl ast::Module {
    pub fn lower(&self, errors: ErrCollector) -> Result<Module, ()> {
        let mut module = Module::new(self.name);

        for item in self.items.iter() {
            match item {
                ast::ModuleItem::Function(fun) => {
                    let fun_ir = module.add_function(
                        fun.name,
                        fun.arity.to_usize().unwrap(),
                    );
                    let mut b = fun_ir.function_mut().builder();
                    fun.lower_into(errors, &mut b)?;
                }
            }
        }

        Ok(module)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum Name {
    Value(Ident),
    Block(Ident),
}
impl Name {
    pub fn span(&self) -> ByteSpan {
        match self {
            Name::Value(ident) => ident.span,
            Name::Block(ident) => ident.span,
        }
    }
}

fn insert_check_duplicate(errors: ErrCollector, scope: &mut HashMapStack<Name, (ByteSpan, Value)>, name: Name, value: Value) -> Result<(), ()> {
    if let Some((span, _)) = scope.get(&name) {
        errors.error(LowerError::DuplicateDefinititon {
            previous: *span,
            current: name.span(),
        });
        Err(())
    } else {
        scope.insert(name, (name.span(), value));
        Ok(())
    }
}

impl ast::Function {
    pub fn lower(&self, errors: ErrCollector, module: Ident) -> Result<(Function, LowerMap), ()> {
        let ident = FunctionIdent {
            module: module,
            name: self.name,
            arity: self.arity.to_usize().unwrap(),
        };
        let mut fun = Function::new(ident);

        let mut b = fun.builder();
        let map = self.lower_into(errors, &mut b)?;

        Ok((fun, map))
    }

    pub fn lower_into(&self, errors: ErrCollector, b: &mut FunctionBuilder) -> Result<LowerMap, ()> {
        let mut blocks: HashMap<Ident, (ByteSpan, Block)> = HashMap::new();
        let mut scope: HashMapStack<Name, (ByteSpan, Value)> = HashMapStack::new();
        scope.push();

        // 1. Create blocks and their args
        let mut first_block = None;
        for item in self.items.iter() {
            match item {
                ast::FunctionItem::Label(label) => {
                    let block_name = label.name.block().unwrap();
                    let block = b.block_insert();
                    insert_check_duplicate(errors, &mut scope, Name::Block(block_name),
                                           b.value(block))?;
                    scope.insert(Name::Block(block_name),
                                 (block_name.span, b.value(block)));
                    blocks.insert(block_name, (block_name.span, block));

                    for arg in label.args.iter() {
                        let arg_name = arg.value().unwrap();
                        let arg = b.block_arg_insert(block);
                        insert_check_duplicate(errors, &mut scope, Name::Value(arg_name),
                                               arg)?;
                        scope.insert(Name::Value(arg_name), (block_name.span, arg));
                    }

                    if first_block.is_none() {
                        first_block = Some(block);
                    }
                }
                _ => (),
            }
        }

        b.block_set_entry(first_block.unwrap());

        // 2. Create assignments and bodies
        let mut current_block: Option<(ByteSpan, Block)> = None;
        for item in self.items.iter() {
            match item {
                ast::FunctionItem::Label(label) => {
                    let block_name = label.name.block().unwrap();

                    if let Some((span, _)) = current_block {
                        errors.error(
                            LowerError::LabelNotFinalized {
                                span,
                            }
                        );
                        return Err(());
                    }

                    current_block = Some((block_name.span, blocks[&block_name].1));
                    scope.push();
                }
                ast::FunctionItem::Assignment(assign) => {
                    let lhs = assign.lhs.value().unwrap();
                    let value = lower_value(errors, b, &mut scope, &assign.rhs)?;
                    scope.insert(Name::Value(lhs), (lhs.span, value));
                }
                ast::FunctionItem::Op(op) => {
                    if let Some((_, block)) = current_block {
                        match op {
                            ast::Op::CallControlFlow(call) => {
                                let target = lower_value(errors, b, &mut scope, &call.target)?;
                                let args: Result<Vec<_>, _> = call.args.iter()
                                    .map(|v| lower_value(errors, b, &mut scope, v))
                                    .collect();
                                b.op_call_flow(block, target, &args?);
                            }
                            ast::Op::CallFunction(call) => {
                                let target = lower_value(errors, b, &mut scope, &call.target)?;

                                let ret = lower_value(errors, b, &mut scope, &call.ret)?;
                                let thr = lower_value(errors, b, &mut scope, &call.thr)?;

                                let args: Result<Vec<_>, _> = call.args.iter()
                                    .map(|v| lower_value(errors, b, &mut scope, v))
                                    .collect();
                                b.op_call_function_next(block, target, ret, thr, &args?);
                            }
                            ast::Op::UnpackValueList(list) => {
                                let target = lower_value(errors, b, &mut scope, &list.block)?;
                                let value = lower_value(errors, b, &mut scope, &list.value)?;
                                b.op_unpack_value_list_next(block, target, value, list.arity);
                            }
                            ast::Op::IfBool(if_bool) => {
                                let value = lower_value(errors, b, &mut scope, &if_bool.value)?;
                                let tru = lower_value(errors, b, &mut scope, &if_bool.tru)?;
                                let fal = lower_value(errors, b, &mut scope, &if_bool.fal)?;
                                if let Some(or) = &if_bool.or {
                                    let or = lower_value(errors, b, &mut scope, or)?;
                                    b.op_if_bool_next(block, tru, fal, or, value);
                                } else {
                                    b.op_if_bool_strict_next(block, tru, fal, value);
                                }
                            }
                            ast::Op::TraceCaptureRaw(trace_op) => {
                                let then = lower_value(errors, b, &mut scope, &trace_op.then)?;
                                b.op_trace_capture_raw_next(block, then);
                            }
                            ast::Op::Match(match_op) => {
                                let mut builder = b.op_match_build();
                                for entry in match_op.entries.iter() {
                                    let next = lower_value(errors, b, &mut scope, &entry.target)?;
                                    match &entry.kind {
                                        ast::MatchKind::Value(v) => {
                                            let v_v = lower_value(errors, b, &mut scope, v)?;
                                            builder.push_value_next(next, v_v, b);
                                        }
                                        ast::MatchKind::ListCell => {
                                            builder.push_list_cell_next(next, b);
                                        }
                                        ast::MatchKind::Wildcard => {
                                            builder.push_wildcard_next(next, b);
                                        }
                                        ast::MatchKind::Type(typ) => {
                                            builder.push_type_next(next, *typ, b);
                                        }
                                        ast::MatchKind::MapItem(k) => {
                                            let k_v = lower_value(errors, b, &mut scope, k)?;
                                            builder.push_map_item_next(next, k_v, b);
                                        }
                                        ast::MatchKind::Tuple(n) => {
                                            builder.push_tuple_next(next, *n, b);
                                        }
                                        s => unimplemented!("{:?}", s),
                                    }
                                }

                                let match_val = lower_value(errors, b, &mut scope, &match_op.value)?;
                                builder.finish(block, match_val, b);
                            }
                            ast::Op::Unreachable => {
                                b.op_unreachable(block);
                            }
                        }
                    } else {
                        errors.error(LowerError::OpOutsideOfLabel {});
                        return Err(());
                    }

                    scope.pop();

                    current_block = None;
                }
            }
        }

        if let Some((span, _)) = current_block {
            errors.error(LowerError::LabelNotFinalized {
                span,
            });
            return Err(());
        }

        assert!(scope.height() == 1);
        let top = scope.layer(0);

        Ok(LowerMap {
            map: top.iter().map(|(k, (_, v))| (*k, *v)).collect(),
            block_map: blocks.iter().map(|(k, (_, v))| (*k, *v)).collect(),
        })
    }
}

fn lower_value(errors: ErrCollector, b: &mut FunctionBuilder, scope: &mut HashMapStack<Name, (ByteSpan, Value)>, val: &ast::Value) -> Result<Value, ()> {
    match val {
        ast::Value::Value(i) => {
            if let Some((_, val)) = scope.get(&Name::Value(*i)) {
                Ok(*val)
            } else {
                errors.error(LowerError::UndefinedVariable { span: i.span, });
                return Err(());
            }
        }
        ast::Value::Block(b) => {
            if let Some((_, val)) = scope.get(&Name::Block(*b)) {
                Ok(*val)
            } else {
                errors.error(LowerError::UndefinedBlock { span: b.span, });
                return Err(());
            }
        }
        ast::Value::Atom(atom) => {
            // TODO escapes
            Ok(b.value(crate::constant::AtomTerm(atom.name)))
        }
        ast::Value::Integer(int) => {
            match int {
                crate::constant::Integer::Small(int) => Ok(b.value(*int)),
                crate::constant::Integer::Big(int) => Ok(b.value(int.clone())),
            }
        }
        ast::Value::Nil => {
            Ok(b.value(crate::constant::NilTerm))
        }
        ast::Value::ValueList(list) => {
            let v_buf: Result<Vec<Value>, _> = list.iter()
                .map(|v| lower_value(errors, b, scope, v))
                .collect();
            Ok(b.prim_value_list(&v_buf?))
        }
        ast::Value::Tuple(tup) => {
            let v_buf: Result<Vec<Value>, _> = tup.iter()
                .map(|v| lower_value(errors, b, scope, v))
                .collect();
            Ok(b.prim_tuple(&v_buf?))
        }
        ast::Value::CaptureFunction(m, f, a) => {
            let m_v = lower_value(errors, b, scope, &*m)?;
            let f_v = lower_value(errors, b, scope, &*f)?;
            let a_v = lower_value(errors, b, scope, &*a)?;

            Ok(b.prim_capture_function(m_v, f_v, a_v))
        }
        ast::Value::BinOp(lhs, op, rhs) => {
            let lhs_v = lower_value(errors, b, scope, &*lhs)?;
            let rhs_v = lower_value(errors, b, scope, &*rhs)?;

            Ok(b.prim_binop(*op, lhs_v, rhs_v))
        }
        ast::Value::List(head, tail) => {
            let mut acc = tail.as_ref()
                .map(|v| lower_value(errors, b, scope, &*v))
                .transpose()?
                .unwrap_or(b.value(crate::constant::NilTerm));
            for v in head.iter().rev() {
                let new_val = lower_value(errors, b, scope, &*v)?;
                acc = b.prim_list_cell(new_val, acc);
            }
            Ok(acc)
        }
    }
}
