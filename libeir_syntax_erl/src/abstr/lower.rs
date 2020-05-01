use libeir_diagnostics::SourceSpan;
use libeir_intern::Ident;
use libeir_ir::ToPrimitive;
use libeir_util_parse::MessageIgnore;

use std::convert::TryInto;

use crate::parser::ast;
use libeir_util_parse_listing::ast as aast;

fn to_list_expr(
    id_gen: &mut ast::NodeIdGenerator,
    span: SourceSpan,
    mut list: Vec<ast::Expr>,
) -> ast::Expr {
    let mut acc = ast::Expr::Nil(ast::Nil(span, id_gen.next()));
    for elem in list.drain(..).rev() {
        acc = ast::Expr::Cons(ast::Cons {
            id: id_gen.next(),
            span,
            head: Box::new(elem),
            tail: Box::new(acc),
        });
    }
    acc
}

pub fn lower(root: &aast::Root) -> ast::Module {
    let mut toplevel: Vec<ast::TopLevel> = Vec::new();

    let mut id_gen = ast::NodeIdGenerator::new();

    let module_span = root.span();
    let mut module_name = None;
    let mut eof = false;

    for item in root.items.iter() {
        let tuple = item.tuple().unwrap();
        let name_ident = tuple.entries[0].atom().unwrap();
        let name = name_ident.as_str();

        let _line = tuple.entries[1].integer().unwrap();

        assert!(!eof);

        match &*name {
            "attribute" => {
                let attr_ident = tuple.entries[2].atom().unwrap();
                let attr = attr_ident.as_str();

                match &*attr {
                    "file" => (),
                    "module" => {
                        module_name = Some(tuple.entries[3].atom().unwrap());
                    }
                    "export" => {
                        let exports = tuple.entries[3]
                            .list_iter()
                            .unwrap()
                            .map(|item| {
                                let item_tup = item.tuple().unwrap();
                                let name = item_tup.entries[0].atom().unwrap();
                                let arity = item_tup.entries[1].integer().unwrap();
                                ast::PartiallyResolvedFunctionName {
                                    span: item_tup.span,
                                    id: id_gen.next(),
                                    function: name,
                                    arity: arity.integer.to_usize().unwrap(),
                                }
                            })
                            .collect();
                        toplevel.push(ast::TopLevel::Attribute(ast::Attribute::Export(
                            tuple.span, exports,
                        )));
                    }
                    "compile" => {
                        let opts = tuple.entries[3]
                            .list_iter()
                            .unwrap()
                            .map(|item| match item {
                                aast::Item::Atom(ident) => {
                                    ast::Expr::Literal(ast::Literal::Atom(id_gen.next(), *ident))
                                }
                                _ => unimplemented!("{:?}", item),
                            })
                            .collect();
                        toplevel.push(ast::TopLevel::Attribute(ast::Attribute::Compile(
                            tuple.span,
                            to_list_expr(&mut id_gen, tuple.span, opts),
                        )))
                    }
                    "spec" => {
                        continue;
                    }
                    "dialyzer" => {
                        continue;
                    }
                    "export_type" => {
                        continue;
                    }
                    "type" => {
                        continue;
                    }
                    "opaque" => {
                        continue;
                        //let inner_tup = tuple.entries[3].tuple().unwrap();
                        //let def = ast::TypeDef {
                        //    span,
                        //    opaque: true,
                        //    name: inner_tup.entries[0].atom().unwrap(),

                        //};
                        //toplevel.push(ast::TopLevel::Attribute(
                        //    ast::Attribute::Type(def)));
                    }
                    "record" => {
                        let rec_tup = tuple.entries[3].tuple().unwrap();

                        let name = rec_tup.entries[0].atom().unwrap();
                        let fields = rec_tup.entries[1]
                            .list_iter()
                            .unwrap()
                            .map(|v| lower_record_field(&mut id_gen, v))
                            .collect();

                        let record = ast::Record {
                            span: rec_tup.span,
                            id: id_gen.next(),
                            name,
                            fields,
                        };
                        toplevel.push(ast::TopLevel::Record(record));
                    }
                    "behaviour" => toplevel.push(ast::TopLevel::Attribute(
                        ast::Attribute::Behaviour(tuple.span, tuple.entries[3].atom().unwrap()),
                    )),
                    n => unimplemented!("attribute {} {:?}", n, tuple),
                }
            }
            "function" => {
                let fun_name = tuple.entries[2].atom().unwrap();
                let fun_arity = tuple.entries[3]
                    .integer()
                    .unwrap()
                    .integer
                    .to_usize()
                    .unwrap();

                let clauses = tuple.entries[4]
                    .list_iter()
                    .unwrap()
                    .map(|clause| lower_function_clause(&mut id_gen, clause))
                    .collect();

                toplevel.push(ast::TopLevel::Function(ast::NamedFunction {
                    span: tuple.span,
                    id: id_gen.next(),
                    name: fun_name,
                    arity: fun_arity,
                    clauses: clauses,
                    spec: None,
                }));
            }
            "eof" => {
                eof = true;
            }
            n => unimplemented!("{}", n),
        }
    }

    let mut errors = MessageIgnore::new();
    let module = ast::Module::new(
        &mut errors,
        module_span,
        &mut id_gen,
        module_name.unwrap(),
        toplevel,
    );
    assert!(!errors.failed());
    module
}

fn lower_record_field(gen: &mut ast::NodeIdGenerator, tup_item: &aast::Item) -> ast::RecordField {
    let tup = tup_item.tuple().unwrap();

    assert!(&*tup.entries[0].atom().unwrap().as_str() == "record_field");
    assert!(tup.entries.len() == 3 || tup.entries.len() == 4);

    let name = atom(&tup.entries[2]);

    let value = tup.entries.get(3).map(|v| lower_expr(gen, v));

    ast::RecordField {
        span: tup.span,
        id: gen.next(),
        name,
        value,
        ty: None,
    }
}

fn lower_function_clause(
    gen: &mut ast::NodeIdGenerator,
    clause: &aast::Item,
) -> ast::FunctionClause {
    let tup = clause.tuple().unwrap();

    assert!(tup.entries[0].atom().unwrap().as_str() == "clause");
    let _line = tup.entries[1].integer().unwrap();

    let params = &tup.entries[2];
    let guard = &tup.entries[3];
    let body = &tup.entries[4];

    let params_n: Vec<_> = params
        .list_iter()
        .unwrap()
        .map(|param| lower_expr(gen, param))
        .collect();

    let guard_n = lower_guards(gen, guard);

    let body_n = lower_body(gen, body);

    ast::FunctionClause {
        span: clause.span(),
        name: None,
        params: params_n,
        guard: guard_n,
        body: body_n,
    }
}

fn lower_clause(gen: &mut ast::NodeIdGenerator, clause: &aast::Item) -> ast::Clause {
    let tup = clause.tuple().unwrap();

    assert!(tup.entries[0].atom().unwrap().as_str() == "clause");
    let _line = tup.entries[1].integer().unwrap();

    let pattern = &tup.entries[2];
    let guard = &tup.entries[3];
    let body = &tup.entries[4];

    let mut patterns = pattern.list_iter().unwrap();
    let pattern_n = lower_expr(gen, patterns.next().unwrap());
    assert!(patterns.next().is_none());

    let guard_n = lower_guards(gen, guard);

    let body_n = lower_body(gen, body);

    ast::Clause {
        span: clause.span(),
        id: gen.next(),
        pattern: pattern_n,
        guard: guard_n,
        body: body_n,
    }
}

fn lower_if_clause(gen: &mut ast::NodeIdGenerator, clause: &aast::Item) -> ast::IfClause {
    let tup = clause.tuple().unwrap();

    assert!(tup.entries[0].atom().unwrap().as_str() == "clause");
    let _line = tup.entries[1].integer().unwrap();

    assert!(tup.entries[2].list_iter().unwrap().count() == 0);
    let guard = &tup.entries[3];
    let body = &tup.entries[4];

    let guard_n = lower_guards(gen, guard);
    let body_n = lower_body(gen, body);

    ast::IfClause {
        span: clause.span(),
        id: gen.next(),
        guards: guard_n.unwrap(),
        body: body_n,
    }
}

fn lower_try_clause(gen: &mut ast::NodeIdGenerator, clause: &aast::Item) -> ast::TryClause {
    println!("{:#?}", clause);
    let tup = clause.tuple().unwrap();

    assert!(tup.entries[0].atom().unwrap().as_str() == "clause");
    let _line = tup.entries[1].integer().unwrap();

    let pattern = &tup.entries[2];
    let guard = &tup.entries[3];
    let body = &tup.entries[4];

    let mut patterns = pattern.list_iter().unwrap();
    let pattern = patterns.next().unwrap();
    assert!(patterns.next().is_none());

    let patterns_tup_cont = pattern.tuple().unwrap();
    assert!(patterns_tup_cont.entries.len() == 3);
    assert!(patterns_tup_cont.entries[0].atom().unwrap().as_str() == "tuple");

    let mut patterns_tup = patterns_tup_cont.entries[2].list_iter().unwrap();
    let err_kind = patterns_tup.next().unwrap();
    let err_error = patterns_tup.next().unwrap();
    let err_trace = patterns_tup.next().unwrap();
    assert!(patterns_tup.next().is_none());

    println!("{:?}", pattern);
    println!("{:?}", err_kind);
    let err_kind_tup = err_kind.tuple().unwrap();
    let err_kind_name = match &*err_kind_tup.entries[0].atom().unwrap().as_str() {
        "var" => ast::Name::Var(err_kind_tup.entries[2].atom().unwrap()),
        "atom" => ast::Name::Atom(err_kind_tup.entries[2].atom().unwrap()),
        _ => panic!(),
    };

    let err_trace_tup = err_trace.tuple().unwrap();
    assert!(&*err_trace_tup.entries[0].atom().unwrap().as_str() == "var");
    let err_trace_ident = err_trace_tup.entries[2].atom().unwrap();

    let guard_n = lower_guards(gen, guard);

    let body_n = lower_body(gen, body);

    ast::TryClause {
        span: clause.span(),
        id: gen.next(),
        kind: err_kind_name,
        error: lower_expr(gen, err_error),
        trace: err_trace_ident,
        guard: guard_n,
        body: body_n,
    }
}

fn lower_guards(gen: &mut ast::NodeIdGenerator, guard: &aast::Item) -> Option<Vec<ast::Guard>> {
    let guard_n: Vec<_> = guard
        .list_iter()
        .unwrap()
        .map(|guard| ast::Guard {
            span: guard.span(),
            conditions: guard
                .list_iter()
                .unwrap()
                .map(|v| lower_expr(gen, v))
                .collect(),
        })
        .collect();
    if guard_n.len() == 0 {
        None
    } else {
        Some(guard_n)
    }
}

fn lower_body(gen: &mut ast::NodeIdGenerator, body: &aast::Item) -> Vec<ast::Expr> {
    let body_n: Vec<_> = body
        .list_iter()
        .unwrap()
        .map(|expr| lower_expr(gen, expr))
        .collect();
    body_n
}

fn lower_expr(gen: &mut ast::NodeIdGenerator, expr: &aast::Item) -> ast::Expr {
    println!("{:#?}", expr);
    let tup = expr.tuple().unwrap();

    let name = tup.entries[0].atom().unwrap().as_str();
    let _line = tup.entries[1].integer().unwrap();

    let span = tup.span;

    match &*name {
        "var" => ast::Expr::Var(ast::Var(gen.next(), tup.entries[2].atom().unwrap())),
        "op" => {
            let tup_len = tup.entries.len();
            let op = tup.entries[2].atom().unwrap().as_str();

            enum ExprKind {
                Unary(ast::UnaryOp),
                Binary(ast::BinaryOp),
            }

            let expr_kind = match (&*op, tup_len) {
                ("+", 5) => ExprKind::Binary(ast::BinaryOp::Add),
                ("-", 5) => ExprKind::Binary(ast::BinaryOp::Sub),
                ("*", 5) => ExprKind::Binary(ast::BinaryOp::Multiply),
                ("==", 5) => ExprKind::Binary(ast::BinaryOp::Equal),
                ("=:=", 5) => ExprKind::Binary(ast::BinaryOp::StrictEqual),
                ("=/=", 5) => ExprKind::Binary(ast::BinaryOp::StrictNotEqual),
                ("andalso", 5) => ExprKind::Binary(ast::BinaryOp::AndAlso),
                ("++", 5) => ExprKind::Binary(ast::BinaryOp::Append),
                (">", 5) => ExprKind::Binary(ast::BinaryOp::Gt),
                ("<", 5) => ExprKind::Binary(ast::BinaryOp::Lt),
                ("rem", 5) => ExprKind::Binary(ast::BinaryOp::Rem),

                ("+", 4) => ExprKind::Unary(ast::UnaryOp::Plus),
                ("-", 4) => ExprKind::Unary(ast::UnaryOp::Minus),
                ("bnot", 4) => ExprKind::Unary(ast::UnaryOp::Bnot),
                ("not", 4) => ExprKind::Unary(ast::UnaryOp::Not),

                (n, a) => unimplemented!("{} {}", n, a),
            };

            match expr_kind {
                ExprKind::Unary(op) => ast::Expr::UnaryExpr(ast::UnaryExpr {
                    span,
                    id: gen.next(),
                    op: op,
                    operand: Box::new(lower_expr(gen, &tup.entries[3])),
                }),
                ExprKind::Binary(op) => ast::Expr::BinaryExpr(ast::BinaryExpr {
                    span,
                    id: gen.next(),
                    op: op,
                    lhs: Box::new(lower_expr(gen, &tup.entries[3])),
                    rhs: Box::new(lower_expr(gen, &tup.entries[4])),
                }),
            }
        }
        "integer" => {
            let int = tup.entries[2].integer().unwrap();
            let lit = ast::Literal::Integer(span, gen.next(), int.integer.clone());
            ast::Expr::Literal(lit)
        }
        "string" => {
            if let Some(string) = tup.entries[2].string() {
                ast::Expr::Literal(ast::Literal::String(gen.next(), string))
            } else {
                let elems: Vec<_> = tup.entries[2].list_iter().unwrap().collect();
                let mut acc = ast::Expr::Nil(ast::Nil(tup.span, gen.next()));
                for elem in elems.iter().rev() {
                    acc = ast::Expr::Cons(ast::Cons {
                        span: elem.span(),
                        id: gen.next(),
                        head: Box::new(lower_expr(gen, elem)),
                        tail: Box::new(acc),
                    });
                }
                acc
            }
        }
        "atom" => {
            let atom = tup.entries[2].atom().unwrap();
            ast::Expr::Literal(ast::Literal::Atom(gen.next(), atom))
        }
        "nil" => ast::Expr::Nil(ast::Nil(span, gen.next())),
        "tuple" => ast::Expr::Tuple(ast::Tuple {
            span,
            id: gen.next(),
            elements: tup.entries[2]
                .list_iter()
                .unwrap()
                .map(|e| lower_expr(gen, e))
                .collect(),
        }),
        "cons" => {
            let head = lower_expr(gen, &tup.entries[2]);
            let tail = lower_expr(gen, &tup.entries[3]);
            ast::Expr::Cons(ast::Cons {
                span,
                id: gen.next(),
                head: Box::new(head),
                tail: Box::new(tail),
            })
        }
        "map" => {
            let tup_len = tup.entries.len();

            let fields = tup.entries[tup_len - 1]
                .list_iter()
                .unwrap()
                .map(|field| {
                    let field_tup = field.tuple().unwrap();
                    let span = field_tup.span;

                    let op_name = field_tup.entries[0].atom().unwrap().as_str();

                    let key = lower_expr(gen, &field_tup.entries[2]);
                    let value = lower_expr(gen, &field_tup.entries[3]);

                    match &*op_name {
                        "map_field_exact" => ast::MapField::Exact {
                            span,
                            id: gen.next(),
                            key,
                            value,
                        },
                        "map_field_assoc" => ast::MapField::Assoc {
                            span,
                            id: gen.next(),
                            key,
                            value,
                        },
                        r => panic!("{}", r),
                    }
                })
                .collect();

            match tup_len {
                3 => ast::Expr::Map(ast::Map {
                    span,
                    id: gen.next(),
                    fields,
                }),
                4 => ast::Expr::MapUpdate(ast::MapUpdate {
                    span,
                    id: gen.next(),
                    map: Box::new(lower_expr(gen, &tup.entries[2])),
                    updates: fields,
                }),
                _ => panic!(),
            }
        }
        "case" => {
            let expr = lower_expr(gen, &tup.entries[2]);
            let clauses = tup.entries[3]
                .list_iter()
                .unwrap()
                .map(|c| lower_clause(gen, c))
                .collect();
            ast::Expr::Case(ast::Case {
                span,
                id: gen.next(),
                expr: Box::new(expr),
                clauses,
            })
        }
        "call" => {
            let target = lower_expr(gen, &tup.entries[2]);

            let args = tup.entries[3]
                .list_iter()
                .unwrap()
                .map(|v| lower_expr(gen, v))
                .collect();

            ast::Expr::Apply(ast::Apply {
                span,
                id: gen.next(),
                callee: Box::new(target),
                args,
            })
        }
        "remote" => ast::Expr::Remote(ast::Remote {
            span,
            id: gen.next(),
            module: Box::new(lower_expr(gen, &tup.entries[2])),
            function: Box::new(lower_expr(gen, &tup.entries[3])),
        }),
        "bin" => {
            let elements = tup.entries[2]
                .list_iter()
                .unwrap()
                .map(|elem| {
                    let tup = elem.tuple().unwrap();
                    assert!(tup.entries[0].atom().unwrap().as_str() == "bin_element");

                    let bit_expr = lower_expr(gen, &tup.entries[2]);

                    let bit_size_v = &tup.entries[3];
                    let bit_size = if let Some(atom) = bit_size_v.atom() {
                        assert!(&*atom.as_str() == "default");
                        None
                    } else {
                        Some(lower_expr(gen, bit_size_v))
                    };

                    let bit_type_v = &tup.entries[4];
                    let bit_type = if let Some(atom) = bit_type_v.atom() {
                        assert!(&*atom.as_str() == "default");
                        None
                    } else {
                        let list = bit_type_v
                            .list_iter()
                            .unwrap()
                            .map(|item| {
                                if let Some(atom) = item.atom() {
                                    ast::BitType::Name(span, gen.next(), atom)
                                } else {
                                    unimplemented!()
                                }
                            })
                            .collect();
                        Some(list)
                    };

                    ast::BinaryElement {
                        span: elem.span(),
                        id: gen.next(),
                        bit_expr,
                        bit_size,
                        bit_type,
                    }
                })
                .collect();
            ast::Expr::Binary(ast::Binary {
                span,
                id: gen.next(),
                elements,
            })
        }
        "fun" => {
            // We expect either a M:F/A or a function definition.
            let inner = tup.entries[2].tuple().unwrap();
            let inner_name = inner.entries[0].atom().unwrap();
            match &*inner_name.as_str() {
                "function" => {
                    let module = atom(&inner.entries[1]);
                    let function = atom(&inner.entries[2]);
                    let arity = integer(&inner.entries[3]);
                    ast::Expr::FunctionName(ast::FunctionName::Resolved(
                        ast::ResolvedFunctionName {
                            span: inner.span,
                            id: gen.next(),
                            module,
                            function,
                            arity: arity.integer.to_usize().unwrap(),
                        },
                    ))
                }
                "clauses" => {
                    let clauses: Vec<_> = inner.entries[1]
                        .list_iter()
                        .unwrap()
                        .map(|v| lower_function_clause(gen, v))
                        .collect();
                    let arity = clauses[0].params.len();
                    for clause in clauses.iter() {
                        assert!(clause.params.len() == arity);
                    }
                    ast::Expr::Fun(ast::Function::Unnamed(ast::Lambda {
                        span: inner.span,
                        id: gen.next(),
                        arity: arity,
                        clauses,
                    }))
                }
                v => unimplemented!("{}", v),
            }
        }
        "match" => {
            let pattern = lower_expr(gen, &tup.entries[2]);
            let expr = lower_expr(gen, &tup.entries[3]);
            ast::Expr::Match(ast::Match {
                span,
                id: gen.next(),
                pattern: Box::new(pattern),
                expr: Box::new(expr),
            })
        }
        "char" => {
            let int = tup.entries[2].integer().unwrap();
            ast::Expr::Literal(ast::Literal::Char(
                span,
                gen.next(),
                int.integer.to_u32().unwrap().try_into().unwrap(),
            ))
        }
        "float" => {
            let float = tup.entries[2].float().unwrap();
            ast::Expr::Literal(ast::Literal::Float(span, gen.next(), float.float))
        }
        "catch" => ast::Expr::Catch(ast::Catch {
            span,
            id: gen.next(),
            expr: Box::new(lower_expr(gen, &tup.entries[2])),
        }),
        "generate" => ast::Expr::Generator(ast::Generator {
            span,
            id: gen.next(),
            pattern: Box::new(lower_expr(gen, &tup.entries[2])),
            expr: Box::new(lower_expr(gen, &tup.entries[3])),
        }),
        "lc" => ast::Expr::ListComprehension(ast::ListComprehension {
            span,
            id: gen.next(),
            body: Box::new(lower_expr(gen, &tup.entries[2])),
            qualifiers: tup.entries[3]
                .list_iter()
                .unwrap()
                .map(|v| lower_expr(gen, v))
                .collect(),
        }),
        "receive" => {
            let mut clauses = Vec::new();
            let after = None;
            for clause in tup.entries[2].list_iter().unwrap() {
                let clause_tup = clause.tuple().unwrap();
                match &*clause_tup.entries[0].atom().unwrap().as_str() {
                    "clause" => {
                        let mut arg_iter = clause_tup.entries[2].list_iter().unwrap();
                        let arg = arg_iter.next().unwrap();
                        assert!(arg_iter.next().is_none());

                        let pattern = lower_expr(gen, arg);
                        let guard = lower_guards(gen, &clause_tup.entries[3]);
                        let body = lower_body(gen, &clause_tup.entries[4]);

                        clauses.push(ast::Clause {
                            span: clause_tup.span,
                            id: gen.next(),
                            pattern,
                            guard: guard,
                            body,
                        });
                    }
                    _ => unimplemented!(),
                }
            }
            ast::Expr::Receive(ast::Receive {
                span,
                id: gen.next(),
                clauses: if clauses.len() == 0 {
                    None
                } else {
                    Some(clauses)
                },
                after: after,
            })
        }
        "block" => {
            let body = lower_body(gen, &tup.entries[2]);
            ast::Expr::Begin(ast::Begin {
                span,
                id: gen.next(),
                body,
            })
        }
        "if" => {
            let clauses = tup.entries[2]
                .list_iter()
                .unwrap()
                .map(|v| lower_if_clause(gen, v))
                .collect();
            ast::Expr::If(ast::If {
                span,
                id: gen.next(),
                clauses,
            })
        }
        "record" => match tup.entries.len() {
            4 => {
                let name = tup.entries[2].atom().unwrap();
                let fields = tup.entries[3]
                    .list_iter()
                    .unwrap()
                    .map(|v| lower_record_field(gen, v))
                    .collect();
                ast::Expr::Record(ast::Record {
                    span,
                    id: gen.next(),
                    name,
                    fields,
                })
            }
            5 => {
                let old = lower_expr(gen, &tup.entries[2]);
                let name = tup.entries[3].atom().unwrap();
                let fields = tup.entries[4]
                    .list_iter()
                    .unwrap()
                    .map(|v| lower_record_field(gen, v))
                    .collect();
                ast::Expr::RecordUpdate(ast::RecordUpdate {
                    span,
                    id: gen.next(),
                    record: Box::new(old),
                    name,
                    updates: fields,
                })
            }
            _ => unimplemented!(),
        },
        "try" => {
            let exprs = lower_body(gen, &tup.entries[2]);
            let clauses: Vec<_> = tup.entries[3]
                .list_iter()
                .unwrap()
                .map(|v| lower_clause(gen, v))
                .collect();
            let catch_clauses: Vec<_> = tup.entries[4]
                .list_iter()
                .unwrap()
                .map(|v| lower_try_clause(gen, v))
                .collect();
            let after = lower_body(gen, &tup.entries[5]);
            ast::Expr::Try(ast::Try {
                span,
                id: gen.next(),
                exprs,
                clauses: if clauses.len() == 0 {
                    None
                } else {
                    Some(clauses)
                },
                catch_clauses: if catch_clauses.len() == 0 {
                    None
                } else {
                    Some(catch_clauses)
                },
                after: if after.len() == 0 { None } else { Some(after) },
            })
        }
        v => unimplemented!("{}", v),
    }
}

fn atom(item: &aast::Item) -> Ident {
    let tup = item.tuple().unwrap();
    assert!(&*tup.entries[0].atom().unwrap().as_str() == "atom");
    tup.entries[2].atom().unwrap()
}

fn integer(item: &aast::Item) -> &aast::Int {
    let tup = item.tuple().unwrap();
    assert!(&*tup.entries[0].atom().unwrap().as_str() == "integer");
    tup.entries[2].integer().unwrap()
}

#[cfg(test)]
mod test {
    use std::sync::Arc;
    use std::path::Path;
    use libeir_diagnostics::{CodeMap, Diagnostic, ToDiagnostic};
    use libeir_util_parse::{error_tee, Errors, Parse, Parser};
    use libeir_util_parse_listing::ast::Root;
    use libeir_util_parse_listing::parser::ParseError;

    use crate::LowerError;

    enum ParseOrLowerError {
        Parse(ParseError),
        Lower(LowerError),
    }
    impl ToDiagnostic for ParseOrLowerError {
        fn to_diagnostic(&self) -> Diagnostic {
            match self {
                ParseOrLowerError::Parse(err) => err.to_diagnostic(),
                ParseOrLowerError::Lower(err) => err.to_diagnostic(),
            }
        }
    }
    impl From<ParseError> for ParseOrLowerError {
        fn from(e: ParseError) -> Self {
            Self::Parse(e)
        }
    }
    impl From<LowerError> for ParseOrLowerError {
        fn from(e: LowerError) -> Self {
            Self::Lower(e)
        }
    }

    fn parse<T, S>(input: S) -> T
    where
        T: Parse<T, Config = (), Error = ParseError>,
        S: AsRef<str>,
    {
        let parser = Parser::new((), Arc::new(CodeMap::new()));

        let mut errors = Errors::new();

        match parser.parse_string::<T, S>(&mut errors, input) {
            Ok(ast) => return ast,
            Err(()) => {
                errors.print(&parser.codemap);
                panic!()
            }
        };
    }

    fn parse_file<T, S>(path: S) -> T
    where
        T: Parse<T, Config = (), Error = ParseError>,
        S: AsRef<Path>,
    {
        let parser = Parser::new((), Arc::new(CodeMap::new()));

        let mut errors = Errors::new();

        match parser.parse_file::<T, S>(&mut errors, path) {
            Ok(ast) => return ast,
            Err(()) => {
                errors.print(&parser.codemap);
                panic!()
            }
        };
    }

    #[test]
    fn basic_ast() {
        let root: Root = parse(
            "
{attribute,1,file,{\"woo.erl\",1}}.
{attribute,1,module,woo}.
{attribute,3,export,[{foo,2},{bar,1},{barr,1}]}.
{function,5,foo,2,
    [{clause,5,
    [{var,5,'A'},{var,5,'B'}],
    [],
    [{op,5,'+',{var,5,'A'},{var,5,'B'}}]}]}.
{function,7,bar,1,
    [{clause,7,[{integer,7,1}],[],[{integer,7,2}]},
    {clause,8,[{integer,8,2}],[],[{integer,8,4}]},
    {clause,9,[{var,9,'N'}],[],[{var,9,'N'}]}]}.
{function,11,barr,1,
    [{clause,11,[{integer,11,1}],[],[{integer,11,2}]},
    {clause,12,[{integer,12,2}],[],[{integer,12,4}]}]}.
{function,14,binary,0,
    [{clause,14,[],[],
    [{bin,14,[{bin_element,14,{string,14,\"woo\"},default,default}]}]}]}.
{function,16,string,0,[{clause,16,[],[],[{string,16,\"woo\"}]}]}.
{eof,17}.
",
        );
        super::lower(&root);
    }

    #[test]
    fn maps() {
        let root: Root = parse_file("../test_data/maps.abstr");
        super::lower(&root);
    }

    #[test]
    fn match_suite() {
        let parser = Parser::new((), Arc::new(CodeMap::new()));

        let mut errors: Errors<ParseOrLowerError, ParseOrLowerError> = Errors::new();
        let res = error_tee(&mut errors, |mut errors| {
            match parser.parse_file::<Root, &str>(
                &mut errors.make_into_adapter(),
                "../test_data/match_SUITE.abstr",
            ) {
                Ok(ast) => {
                    let module = super::lower(&ast);
                    crate::lower_module(&mut errors.make_into_adapter(), parser.codemap.clone(), &module)
                }
                Err(()) => Err(()),
            }
        });

        match res {
            Ok(_res) => (),
            Err(()) => {
                errors.print(&parser.codemap);
                panic!();
            }
        }
    }
}
