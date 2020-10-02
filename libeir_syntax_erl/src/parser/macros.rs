/// Construct a keyword list AST from a list of expressions, e.g.:
///
///     kwlist!(tuple!(atom!(key), var!(Value)))
///
/// It is required that elements are two-tuples, but no checking is done
/// to make sure that the first element in each tuple is an atom
#[allow(unused_macros)]
macro_rules! kwlist {
    ($nid:expr, $($element:expr),*) => {
        let mut elements = vec![$($element),*];
        elements.reverse();
        elements.fold(nil!($nid), |acc, (key, val)| {
            cons!($nid, Expr::Tuple(Tuple { span: SourceSpan::UNKNOWN, id: $nid.next(), elements: vec![key, val] }), acc)
        })
    }
}

/// Like `kwlist!`, but produces a list of elements produced by any arbitrary expression
///
///     list!(atom!(foo), tuple!(atom!(bar), atom!(baz)))
///
/// Like `kwlist!`, this produces a proper list
#[allow(unused_macros)]
macro_rules! list {
    ($nid:expr, $($element:expr),*) => {
        {
            let mut elements = vec![$($element),*];
            elements.reverse();
            elements.iter().fold(nil!($nid), |acc, el| {
                cons!($nid, *el, acc)
            })
        }
    }
}

/// A lower-level primitive for constructing lists, via cons cells.
/// Given the following:
///
///     cons!(atom!(a), cons!(atom!(b), nil!()))
///
/// This is equivalent to `[a | [b | []]]`, which is in turn equivalent
/// to `[a, b]`. You are better off using `list!` unless you explicitly
/// need to construct an improper list
#[allow(unused_macros)]
macro_rules! cons {
    ($nid:expr, $head:expr, $tail:expr) => {
        Expr::Cons(Cons {
            span: SourceSpan::UNKNOWN,
            id: $nid.next(),
            head: Box::new($head),
            tail: Box::new($tail),
        })
    };
}

macro_rules! nil {
    ($nid:expr) => {
        Expr::Nil(Nil(SourceSpan::UNKNOWN, $nid.next()))
    };
}

/// Produces a tuple expression with the given elements
macro_rules! tuple {
    ($nid:expr, $($element:expr),*) => {
        Expr::Tuple(Tuple{
            span: SourceSpan::UNKNOWN,
            id: $nid.next(),
            elements: vec![$($element),*],
        })
    }
}

/// Produces an integer literal expression
macro_rules! int {
    ($nid:expr, $i:expr) => {
        Expr::Literal(Literal::Integer(SourceSpan::UNKNOWN, $nid.next(), $i))
    };
}

/// Produces a literal expression which evaluates to an atom
macro_rules! atom {
    ($nid:expr, $sym:ident) => {
        Expr::Literal(Literal::Atom(
            $nid.next(),
            Ident::with_empty_span(Symbol::intern(stringify!($sym))),
        ))
    };
    ($nid:expr, $sym:expr) => {
        Expr::Literal(Literal::Atom(
            $nid.next(),
            Ident::with_empty_span(Symbol::intern($sym)),
        ))
    };
}

macro_rules! atom_from_sym {
    ($nid:expr, $sym:expr) => {
        Expr::Literal(Literal::Atom($nid.next(), Ident::with_empty_span($sym)))
    };
}

/// Produces an Ident from an expression, meant to be used to simplify generating
/// identifiers in the AST from strings or symbols
macro_rules! ident {
    ($sym:ident) => {
        Ident::with_empty_span(Symbol::intern(stringify!($sym)))
    };
    ($sym:expr) => {
        Ident::with_empty_span(Symbol::intern($sym))
    };
    (_) => {
        Ident::with_empty_span(Symbol::intern("_"))
    };
}

/// Produces an Option<Ident> from an expression, meant to be used to simplify generating
/// identifiers in the AST from strings or symbols
#[allow(unused_macros)]
macro_rules! ident_opt {
    ($sym:ident) => {
        Some(Ident::with_empty_span(Symbol::intern(stringify!($sym))))
    };
    ($sym:expr) => {
        Some(Ident::with_empty_span(Symbol::intern($sym)))
    };
    (_) => {
        Ident::with_empty_span(Symbol::intern("_"))
    };
}

/// Produces a variable expression
macro_rules! var {
    ($nid:expr, $name:ident) => {
        Expr::Var(Var($nid.next(), ident!(stringify!($name))))
    };
    ($nid:expr, _) => {
        Expr::Var(Var($nid.next(), ident!(_)))
    };
}

/// Produces a remote expression, e.g. `erlang:get_module_info`
///
/// Expects the module/function to be identifier symbols
macro_rules! remote {
    ($nid: expr, $module:ident, $function:ident) => {
        Expr::Remote(Remote {
            span: SourceSpan::UNKNOWN,
            id: $nid.next(),
            module: Box::new(atom!($nid, $module)),
            function: Box::new(atom!($nid, $function)),
        })
    };
    ($nid:expr, $module:expr, $function:expr) => {
        Expr::Remote(Remote {
            span: SourceSpan::UNKNOWN,
            id: $nid.next(),
            module: Box::new($module),
            function: Box::new($function),
        })
    };
}

/// Produces a function application expression
macro_rules! apply {
    ($nid:expr, $callee:expr, $($args:expr),*) => {
        Expr::Apply(Apply {
            span: SourceSpan::UNKNOWN,
            id: $nid.next(),
            callee: Box::new($callee),
            args: vec![$($args),*]
        })
    }
}

/// Produces a function definition
macro_rules! fun {
    ($nid:expr, $name:ident ($($params:ident),*) -> $body:expr) => {
        {
            let params = vec![$(var!($nid, $params)),*];
            let arity = params.len();
            NamedFunction {
                span: SourceSpan::UNKNOWN,
                id: $nid.next(),
                name: Name::Atom(ident!($name)),
                arity,
                clauses: vec![
                    FunctionClause{
                        span: SourceSpan::UNKNOWN,
                        name: Some(Name::Atom(ident!($name))),
                        params,
                        guard: None,
                        body: vec![$body],
                    }
                ],
                spec: None,
            }
        }
    };
    ($nid:expr, $name:ident $(($($params:expr),*) -> $body:expr);*) => {
        {
            let mut clauses = Vec::new();
            $(
                clauses.push(FunctionClause {
                    span: SourceSpan::UNKNOWN,
                    name: Some(Name::Atom(ident!($name))),
                    params: vec![$($params),*],
                    guard: None,
                    body: vec![$body],
                });
            )*
            let arity = clauses.first().as_ref().unwrap().params.len();
            NamedFunction {
                span: SourceSpan::UNKNOWN,
                id: $nid.next(),
                name: Name::Atom(ident!($name)),
                arity,
                clauses,
                spec: None,
            }
        }
    }
}
