use std::collections::HashMap;
use std::error::Error;

use libeir_diagnostics::{CodeMap, Diagnostic, Label, SourceSpan, ToDiagnostic};
use libeir_intern::Ident;
use libeir_util_datastructures::hashmap_stack::HashMapStack;
use libeir_util_number::ToPrimitive;
use libeir_util_parse::ErrorReceiver;

use snafu::Snafu;

use crate::text::ast;
use crate::{Block, Value};
use crate::{Function, FunctionBuilder, FunctionIdent, Module};
use crate::{PatternContainer, PatternNode};

mod location;

type ErrCollector<'a> = &'a mut dyn ErrorReceiver<E = LowerError, W = LowerError>;

pub struct LowerContext<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    errors: ErrCollector<'a>,
    scope: &'a mut HashMapStack<Name, (SourceSpan, Value)>,
}

impl<'a, 'b> LowerContext<'a, 'b> {
    pub fn error<T: ToDiagnostic>(&mut self, diag: T) {
        self.errors.error(LowerError::DynError {
            diagnostic: diag.to_diagnostic(),
        })
    }
}

#[derive(Debug, Snafu)]
pub enum LowerError {
    DuplicateDefinititon {
        previous: SourceSpan,
        current: SourceSpan,
    },

    LabelNotFinalized {
        span: SourceSpan,
    },

    OpOutsideOfLabel {},

    UndefinedVariable {
        span: SourceSpan,
    },

    UndefinedBlock {
        span: SourceSpan,
    },

    UndefinedBind {
        span: SourceSpan,
    },

    UnknownMeta {
        span: SourceSpan,
        name: Ident,
    },

    UnknownDyn {
        span: SourceSpan,
    },
    DynError {
        diagnostic: Diagnostic,
    },
}

impl ToDiagnostic for LowerError {
    fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            LowerError::DuplicateDefinititon { previous, current } => Diagnostic::error()
                .with_message("duplicate identifier definition")
                .with_labels(vec![
                    Label::primary(current.source_id(), *current)
                        .with_message("attempted redefinition"),
                    Label::secondary(previous.source_id(), *previous)
                        .with_message("previously defined here"),
                ]),
            LowerError::LabelNotFinalized { span } => Diagnostic::error()
                .with_message("label not finalized")
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message("block does not end with an operation")]),
            LowerError::UndefinedVariable { span } => Diagnostic::error()
                .with_message("undefined variable name")
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message("variable name was not defined in the IR")]),
            LowerError::UndefinedBlock { span } => Diagnostic::error()
                .with_message("undefined block name")
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message("block name was not defined in the IR")]),
            LowerError::UnknownMeta { name, .. } => Diagnostic::error()
                .with_message("unknown meta entry")
                .with_labels(vec![Label::primary(name.span.source_id(), name.span)
                    .with_message("meta entry is unknown to the compiler")]),
            LowerError::UnknownDyn { span } => Diagnostic::error()
                .with_message("unknown dynop identifier")
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message("no parser exists for dynop in current dialect")]),
            LowerError::DynError { diagnostic } => diagnostic.clone(),
            _ => Diagnostic::error().with_message(msg),
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
                        SourceSpan::UNKNOWN,
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
    pub fn span(&self) -> SourceSpan {
        match self {
            Name::Value(ident) => ident.span,
            Name::Block(ident) => ident.span,
        }
    }
}

fn insert_check_duplicate(
    errors: ErrCollector,
    scope: &mut HashMapStack<Name, (SourceSpan, Value)>,
    name: Name,
    value: Value,
) -> Result<(), ()> {
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
            module,
            name: self.name,
            arity: self.arity.to_usize().unwrap(),
        };
        let mut fun = Function::new(SourceSpan::UNKNOWN, ident);

        let mut b = fun.builder();
        let map = self.lower_into(errors, &mut b)?;

        Ok((fun, map))
    }

    pub fn lower_into(
        &self,
        errors: ErrCollector,
        b: &mut FunctionBuilder,
    ) -> Result<LowerMap, ()> {
        let mut blocks: HashMap<Ident, (SourceSpan, Block)> = HashMap::new();
        let mut scope: HashMapStack<Name, (SourceSpan, Value)> = HashMapStack::new();
        scope.push();

        let mut location = None;

        // 1. Create blocks and their args
        let mut first_block = None;
        for item in self.items.iter() {
            match item {
                ast::FunctionItem::Label(label) => {
                    let block_name = label.name.block().unwrap();
                    let block = b.block_insert();

                    if let Some(loc) = location {
                        b.block_set_location(block, loc);
                    }

                    insert_check_duplicate(
                        errors,
                        &mut scope,
                        Name::Block(block_name),
                        b.value(block),
                    )?;
                    scope.insert(Name::Block(block_name), (block_name.span, b.value(block)));
                    blocks.insert(block_name, (block_name.span, block));

                    for arg in label.args.iter() {
                        let arg_name = arg.value().unwrap();
                        let arg = b.block_arg_insert(block);
                        insert_check_duplicate(errors, &mut scope, Name::Value(arg_name), arg)?;
                        scope.insert(Name::Value(arg_name), (block_name.span, arg));
                    }

                    if first_block.is_none() {
                        first_block = Some(block);
                    }
                }
                ast::FunctionItem::Meta(meta) => match &*meta.name.as_str() {
                    "location" => {
                        let mut ctx = super::parse_dyn::ParseCtx::new(&meta.tokens, meta.span);
                        match self::location::parse_location(&mut ctx) {
                            Ok(loc_desc) => {
                                let mut terminals = vec![];
                                for term_desc in loc_desc.terminals.iter() {
                                    let term = b.fun_mut().locations.terminal(
                                        term_desc.file.map(|v| v.to_string()),
                                        term_desc.line,
                                        term_desc.module.map(|v| v.to_string()),
                                        term_desc.function.map(|v| v.to_string()),
                                        meta.span,
                                    );
                                    terminals.push(term);
                                }
                                let loc = b.fun_mut().locations.from_terminals(&terminals);
                                location = Some(loc);
                            }
                            Err(err) => errors.error(LowerError::DynError {
                                diagnostic: err.to_diagnostic(),
                            }),
                        }
                    }
                    _ => errors.error(LowerError::UnknownMeta {
                        span: meta.span,
                        name: meta.name,
                    }),
                },
                _ => (),
            }
        }

        b.block_set_entry(first_block.unwrap());

        // 2. Create assignments and bodies
        let mut current_block: Option<(SourceSpan, Block)> = None;
        for item in self.items.iter() {
            match item {
                ast::FunctionItem::Label(label) => {
                    let block_name = label.name.block().unwrap();

                    if let Some((span, _)) = current_block {
                        errors.error(LowerError::LabelNotFinalized { span });
                        return Err(());
                    }

                    current_block = Some((block_name.span, blocks[&block_name].1));
                    scope.push();
                }
                ast::FunctionItem::Meta(meta) => match &*meta.name.as_str() {
                    "location" => {}
                    _ => errors.error(LowerError::UnknownMeta {
                        span: meta.span,
                        name: meta.name,
                    }),
                },
                ast::FunctionItem::Assignment(assign) => {
                    let lhs = assign.lhs.value().unwrap();
                    let value = lower_value(errors, b, &mut scope, &assign.rhs)?;
                    scope.insert(Name::Value(lhs), (lhs.span, value));
                }
                ast::FunctionItem::Op(op) => {
                    if let Some((_, block)) = current_block {
                        lower_operation(b, errors, &mut scope, block, op)?;
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
            errors.error(LowerError::LabelNotFinalized { span });
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

fn lower_operation(
    b: &mut FunctionBuilder,
    errors: ErrCollector,
    scope: &mut HashMapStack<Name, (SourceSpan, Value)>,
    block: Block,
    op: &ast::Op,
) -> Result<(), ()> {
    match op {
        ast::Op::Dyn(ident, opts) => {
            let mut ctx = LowerContext {
                builder: b,
                errors,
                scope,
            };

            if let Some(parser) = crate::dialect::NORMAL.get_op_parser(ident.name) {
                parser.parse(&mut ctx, block, opts)?;
            } else {
                errors.error(LowerError::UnknownDyn { span: ident.span });
            }
        }
        ast::Op::CallControlFlow(call) => {
            let target = lower_value(errors, b, scope, &call.target)?;
            let args: Result<Vec<_>, _> = call
                .args
                .iter()
                .map(|v| lower_value(errors, b, scope, v))
                .collect();
            b.op_call_flow(block, target, &args?);
        }
        ast::Op::CallFunction(call) => {
            let target = lower_value(errors, b, scope, &call.target)?;

            let ret = lower_value(errors, b, scope, &call.ret)?;
            let thr = lower_value(errors, b, scope, &call.thr)?;

            let args: Result<Vec<_>, _> = call
                .args
                .iter()
                .map(|v| lower_value(errors, b, scope, v))
                .collect();
            b.op_call_function_next(SourceSpan::UNKNOWN, block, target, ret, thr, &args?);
        }
        ast::Op::UnpackValueList(list) => {
            let target = lower_value(errors, b, scope, &list.block)?;
            let value = lower_value(errors, b, scope, &list.value)?;
            b.op_unpack_value_list_next(block, target, value, list.arity);
        }
        ast::Op::IfBool(if_bool) => {
            let value = lower_value(errors, b, scope, &if_bool.value)?;
            let tru = lower_value(errors, b, scope, &if_bool.tru)?;
            let fal = lower_value(errors, b, scope, &if_bool.fal)?;
            if let Some(or) = &if_bool.or {
                let or = lower_value(errors, b, scope, or)?;
                b.op_if_bool_next(SourceSpan::UNKNOWN, block, tru, fal, or, value);
            } else {
                b.op_if_bool_strict_next(SourceSpan::UNKNOWN, block, tru, fal, value);
            }
        }
        ast::Op::TraceCaptureRaw(trace_op) => {
            let then = lower_value(errors, b, scope, &trace_op.then)?;
            b.op_trace_capture_raw_next(SourceSpan::UNKNOWN, block, then);
        }
        ast::Op::Match(match_op) => {
            let mut builder = b.op_match_build(SourceSpan::UNKNOWN);
            for entry in match_op.entries.iter() {
                let next = lower_value(errors, b, scope, &entry.target)?;
                match &entry.kind {
                    ast::MatchKind::Value(v) => {
                        let v_v = lower_value(errors, b, scope, v)?;
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
                        let k_v = lower_value(errors, b, scope, k)?;
                        builder.push_map_item_next(next, k_v, b);
                    }
                    ast::MatchKind::Tuple(n) => {
                        builder.push_tuple_next(next, *n, b);
                    }
                    s => unimplemented!("{:?}", s),
                }
            }

            let match_val = lower_value(errors, b, scope, &match_op.value)?;
            builder.finish(block, match_val, b);
        }
        ast::Op::Unreachable => {
            b.op_unreachable(SourceSpan::UNKNOWN, block);
        }
        ast::Op::Case(case_op) => {
            use crate::operation::case::Case;
            let value = lower_value(errors, b, scope, &case_op.value)?;

            let mut binds = HashMap::new();

            let mut case_b = Case::builder();

            for entry in case_op.entries.iter() {
                binds.clear();
                let clause = case_b.container.clause_start(SourceSpan::UNKNOWN);

                for pattern in entry.patterns.iter() {
                    let pat = lower_case_pattern(
                        errors,
                        &mut case_b.container,
                        b,
                        scope,
                        &mut binds,
                        pattern,
                    )?;
                    case_b.container.clause_node_push(clause, pat);
                }

                for bind in entry.args.iter() {
                    if let Some(node) = binds.get(bind) {
                        case_b.container.clause_bind_push(clause, node.1);
                    } else {
                        errors.error(LowerError::UndefinedBind { span: bind.span });
                        return Err(());
                    }
                }

                case_b.container.clause_finish(clause);

                let guard = lower_value(errors, b, scope, &entry.guard)?;
                let target = lower_value(errors, b, scope, &entry.target)?;

                case_b.push_clause(clause, guard, target, b);
            }

            if let Some(no_match) = case_op.no_match.as_ref() {
                let val = lower_value(errors, b, scope, no_match)?;
                case_b.no_match = Some(val);
            }

            case_b.match_on = Some(value);
            case_b.finish(block, b);
        }
    }

    Ok(())
}

fn lower_case_pattern(
    errors: ErrCollector,
    pat: &mut PatternContainer,
    b: &mut FunctionBuilder,
    scope: &mut HashMapStack<Name, (SourceSpan, Value)>,
    binds: &mut HashMap<Ident, (SourceSpan, PatternNode)>,
    pattern: &ast::CasePattern,
) -> Result<PatternNode, ()> {
    match pattern {
        ast::CasePattern::Binding { name, pattern } => {
            let child = lower_case_pattern(errors, pat, b, scope, binds, pattern)?;
            if binds.contains_key(name) {
                errors.error(LowerError::DuplicateDefinititon {
                    current: name.span,
                    previous: binds[name].0,
                });
                return Err(());
            }
            binds.insert(*name, (name.span, child));
            Ok(child)
        }
        ast::CasePattern::Wildcard => {
            let node = pat.node_empty(Some(SourceSpan::UNKNOWN));
            pat.wildcard(node);
            Ok(node)
        }
        _ => unimplemented!(),
    }
}

fn lower_value(
    errors: ErrCollector,
    b: &mut FunctionBuilder,
    scope: &mut HashMapStack<Name, (SourceSpan, Value)>,
    val: &ast::Value,
) -> Result<Value, ()> {
    match val {
        ast::Value::Value(i) => {
            if let Some((_, val)) = scope.get(&Name::Value(*i)) {
                Ok(*val)
            } else {
                errors.error(LowerError::UndefinedVariable { span: i.span });
                return Err(());
            }
        }
        ast::Value::Block(b) => {
            if let Some((_, val)) = scope.get(&Name::Block(*b)) {
                Ok(*val)
            } else {
                errors.error(LowerError::UndefinedBlock { span: b.span });
                return Err(());
            }
        }
        ast::Value::Atom(atom) => {
            // TODO escapes
            Ok(b.value(crate::constant::AtomTerm(atom.name)))
        }
        ast::Value::Integer(int) => match int {
            crate::constant::Integer::Small(int) => Ok(b.value(*int)),
            crate::constant::Integer::Big(int) => Ok(b.value(int.clone())),
        },
        ast::Value::Nil => Ok(b.value(crate::constant::NilTerm)),
        ast::Value::ValueList(list) => {
            let v_buf: Result<Vec<Value>, _> = list
                .iter()
                .map(|v| lower_value(errors, b, scope, v))
                .collect();
            Ok(b.prim_value_list(&v_buf?))
        }
        ast::Value::Tuple(tup) => {
            let v_buf: Result<Vec<Value>, _> = tup
                .iter()
                .map(|v| lower_value(errors, b, scope, v))
                .collect();
            Ok(b.prim_tuple(SourceSpan::UNKNOWN, &v_buf?))
        }
        ast::Value::CaptureFunction(m, f, a) => {
            let m_v = lower_value(errors, b, scope, &*m)?;
            let f_v = lower_value(errors, b, scope, &*f)?;
            let a_v = lower_value(errors, b, scope, &*a)?;

            Ok(b.prim_capture_function(SourceSpan::UNKNOWN, m_v, f_v, a_v))
        }
        ast::Value::BinOp(lhs, op, rhs) => {
            let lhs_v = lower_value(errors, b, scope, &*lhs)?;
            let rhs_v = lower_value(errors, b, scope, &*rhs)?;

            Ok(b.prim_binop(SourceSpan::UNKNOWN, *op, lhs_v, rhs_v))
        }
        ast::Value::List(head, tail) => {
            let mut acc = tail
                .as_ref()
                .map(|v| lower_value(errors, b, scope, &*v))
                .transpose()?
                .unwrap_or(b.value(crate::constant::NilTerm));
            for v in head.iter().rev() {
                let new_val = lower_value(errors, b, scope, &*v)?;
                acc = b.prim_list_cell(SourceSpan::UNKNOWN, new_val, acc);
            }
            Ok(acc)
        }
    }
}
