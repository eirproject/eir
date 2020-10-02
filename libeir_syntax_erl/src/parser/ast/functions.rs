use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

use libeir_diagnostics::{Diagnostic, Label, SourceSpan};
use libeir_util_parse::ErrorReceiver;

use crate::preprocessor::PreprocessorError;

use super::ParserError;
use super::{Arity, Expr, Ident, Name, NodeId, NodeIdGenerator, TypeSpec};

#[derive(Debug, Copy, Clone)]
pub struct LocalFunctionName {
    pub span: SourceSpan,
    pub function: Ident,
    pub arity: usize,
}
impl PartialEq for LocalFunctionName {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.arity == other.arity
    }
}
impl Eq for LocalFunctionName {}
impl Hash for LocalFunctionName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.function.hash(state);
        self.arity.hash(state);
    }
}
impl PartialOrd for LocalFunctionName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (xf, xa) = (self.function, self.arity);
        let (yf, ya) = (other.function, other.arity);
        match xf.partial_cmp(&yf) {
            None | Some(Ordering::Equal) => xa.partial_cmp(&ya),
            Some(order) => Some(order),
        }
    }
}
impl Ord for LocalFunctionName {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

/// Represents a fully-resolved function name, with module/function/arity explicit
#[derive(Debug, Clone)]
pub struct ResolvedFunctionName {
    pub span: SourceSpan,
    pub id: NodeId,
    pub module: Ident,
    pub function: Ident,
    pub arity: usize,
}
impl PartialEq for ResolvedFunctionName {
    fn eq(&self, other: &Self) -> bool {
        self.module == other.module && self.function == other.function && self.arity == other.arity
    }
}
impl Eq for ResolvedFunctionName {}
impl Hash for ResolvedFunctionName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module.hash(state);
        self.function.hash(state);
        self.arity.hash(state);
    }
}
impl PartialOrd for ResolvedFunctionName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (xm, xf, xa) = (self.module, self.function, self.arity);
        let (ym, yf, ya) = (other.module, other.function, other.arity);
        match xm.partial_cmp(&ym) {
            None | Some(Ordering::Equal) => match xf.partial_cmp(&yf) {
                None | Some(Ordering::Equal) => xa.partial_cmp(&ya),
                Some(order) => Some(order),
            },
            Some(order) => Some(order),
        }
    }
}
impl Ord for ResolvedFunctionName {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl ResolvedFunctionName {
    pub fn to_local(&self) -> LocalFunctionName {
        LocalFunctionName {
            span: self.span,
            function: self.function,
            arity: self.arity,
        }
    }
}

/// Represents a partially-resolved function name, not yet associated with a module
/// This is typically used to express local captures, e.g. `fun do_stuff/0`
#[derive(Debug, Clone)]
pub struct PartiallyResolvedFunctionName {
    pub span: SourceSpan,
    pub id: NodeId,
    pub function: Ident,
    pub arity: usize,
}
impl PartiallyResolvedFunctionName {
    pub fn resolve(&self, module: Ident) -> ResolvedFunctionName {
        ResolvedFunctionName {
            span: self.span.clone(),
            id: self.id,
            module,
            function: self.function.clone(),
            arity: self.arity,
        }
    }
}
impl PartialEq for PartiallyResolvedFunctionName {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.arity == other.arity
    }
}
impl Eq for PartiallyResolvedFunctionName {}
impl Hash for PartiallyResolvedFunctionName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.function.hash(state);
        self.arity.hash(state);
    }
}
impl PartialOrd for PartiallyResolvedFunctionName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (xf, xa) = (self.function, self.arity);
        let (yf, ya) = (other.function, other.arity);
        match xf.partial_cmp(&yf) {
            None | Some(Ordering::Equal) => xa.partial_cmp(&ya),
            Some(order) => Some(order),
        }
    }
}

impl PartiallyResolvedFunctionName {
    pub fn to_local(&self) -> LocalFunctionName {
        LocalFunctionName {
            span: self.span,
            function: self.function,
            arity: self.arity,
        }
    }
}

/// Represents a function name which contains parts which are not yet concrete,
/// i.e. they are expressions which need to be evaluated to know precisely which
/// module or function is referenced
#[derive(Debug, Clone)]
pub struct UnresolvedFunctionName {
    pub span: SourceSpan,
    pub id: NodeId,
    pub module: Option<Name>,
    pub function: Name,
    pub arity: Arity,
}
impl PartialEq for UnresolvedFunctionName {
    fn eq(&self, other: &Self) -> bool {
        self.module == other.module && self.function == other.function && self.arity == other.arity
    }
}
impl Eq for UnresolvedFunctionName {}
impl Hash for UnresolvedFunctionName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module.hash(state);
        self.function.hash(state);
        self.arity.hash(state);
    }
}
impl PartialOrd for UnresolvedFunctionName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.module.partial_cmp(&other.module) {
            None | Some(Ordering::Equal) => match self.function.partial_cmp(&other.function) {
                None | Some(Ordering::Equal) => self.arity.partial_cmp(&other.arity),
                Some(order) => Some(order),
            },
            Some(order) => Some(order),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum FunctionName {
    Resolved(ResolvedFunctionName),
    PartiallyResolved(PartiallyResolvedFunctionName),
    Unresolved(UnresolvedFunctionName),
}
impl FunctionName {
    pub fn span(&self) -> SourceSpan {
        match self {
            &FunctionName::Resolved(ResolvedFunctionName { ref span, .. }) => span.clone(),
            &FunctionName::PartiallyResolved(PartiallyResolvedFunctionName {
                ref span, ..
            }) => span.clone(),
            &FunctionName::Unresolved(UnresolvedFunctionName { ref span, .. }) => span.clone(),
        }
    }
    pub fn id(&self) -> NodeId {
        match self {
            FunctionName::Resolved(fun) => fun.id,
            FunctionName::PartiallyResolved(fun) => fun.id,
            FunctionName::Unresolved(fun) => fun.id,
        }
    }

    pub fn detect(
        span: SourceSpan,
        nid: &mut NodeIdGenerator,
        module: Option<Name>,
        function: Name,
        arity: Arity,
    ) -> Self {
        if module.is_none() {
            return match (function, arity) {
                (Name::Atom(f), Arity::Int(a)) => {
                    FunctionName::PartiallyResolved(PartiallyResolvedFunctionName {
                        span,
                        id: nid.next(),
                        function: f,
                        arity: a,
                    })
                }
                _ => FunctionName::Unresolved(UnresolvedFunctionName {
                    span,
                    id: nid.next(),
                    module: None,
                    function,
                    arity,
                }),
            };
        }

        if let (Some(Name::Atom(m)), Name::Atom(f), Arity::Int(a)) = (module, function, arity) {
            return FunctionName::Resolved(ResolvedFunctionName {
                span,
                id: nid.next(),
                module: m,
                function: f,
                arity: a,
            });
        }

        FunctionName::Unresolved(UnresolvedFunctionName {
            span,
            id: nid.next(),
            module,
            function,
            arity,
        })
    }

    pub fn from_clause(nid: &mut NodeIdGenerator, clause: &FunctionClause) -> FunctionName {
        match clause {
            &FunctionClause {
                name: Some(ref name),
                ref span,
                ref params,
                ..
            } => FunctionName::PartiallyResolved(PartiallyResolvedFunctionName {
                span: span.clone(),
                id: nid.next(),
                function: name.atom(),
                arity: params.len(),
            }),
            _ => panic!("cannot create a FunctionName from an anonymous FunctionClause!"),
        }
    }
}
impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionName::Resolved(ResolvedFunctionName {
                ref module,
                ref function,
                arity,
                ..
            }) => write!(f, "{}:{}/{}", module, function, arity),
            FunctionName::PartiallyResolved(PartiallyResolvedFunctionName {
                ref function,
                arity,
                ..
            }) => write!(f, "{}/{}", function, arity),
            FunctionName::Unresolved(UnresolvedFunctionName {
                module: Some(ref module),
                ref function,
                arity,
                ..
            }) => write!(f, "{:?}:{:?}/{:?}", module, function, arity),
            FunctionName::Unresolved(UnresolvedFunctionName {
                ref function,
                arity,
                ..
            }) => write!(f, "{:?}/{:?}", function, arity),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamedFunction {
    pub span: SourceSpan,
    pub id: NodeId,
    pub name: Name,
    pub arity: usize,
    pub clauses: Vec<FunctionClause>,
    pub spec: Option<TypeSpec>,
}
impl PartialEq for NamedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.arity == other.arity
            && self.clauses == other.clauses
            && self.spec == other.spec
    }
}
impl NamedFunction {
    pub fn new(
        errs: &mut dyn ErrorReceiver<E = ParserError, W = ParserError>,
        span: SourceSpan,
        nid: &mut NodeIdGenerator,
        clauses: Vec<FunctionClause>,
    ) -> Result<Self, ()> {
        debug_assert!(clauses.len() > 0);
        let (head, rest) = clauses.split_first().unwrap();

        if head.name.is_none() {
            errs.error(
                PreprocessorError::ShowDiagnostic {
                    diagnostic: Diagnostic::error()
                        .with_message("expected named function")
                        .with_labels(vec![Label::primary(head.span.source_id(), head.span)
                            .with_message(
                                "this clause has no name, but a name is required here",
                            )]),
                }
                .into(),
            );
            return Err(());
        }

        let head_span = &head.span;
        let name = head.name.clone().unwrap();
        let params = &head.params;
        let arity = params.len();

        // Check clauses
        let mut last_clause = head_span.clone();
        for clause in rest.iter() {
            if clause.name.is_none() {
                errs.error(
                    PreprocessorError::ShowDiagnostic {
                        diagnostic: Diagnostic::error()
                            .with_message("expected named function clause")
                            .with_labels(vec![
                                Label::primary(clause.span.source_id(), clause.span).with_message(
                                    "this clause has no name, but a name is required here",
                                ),
                                Label::secondary(last_clause.source_id(), last_clause)
                                    .with_message(
                                        "expected a clause with the same name as this clause",
                                    ),
                            ]),
                    }
                    .into(),
                );
                return Err(());
            }

            let clause_span = &clause.span;
            let clause_name = clause.name.clone().unwrap();
            let clause_params = &clause.params;
            let clause_arity = clause_params.len();

            if clause_name != name {
                errs.error(ParserError::ShowDiagnostic {
                    diagnostic: Diagnostic::error()
                        .with_message("unterminated function clause")
                        .with_labels(vec![
                            Label::primary(last_clause.source_id(), last_clause.clone())
                                .with_message(
                                "this clause ends with ';', indicating that another clause follows",
                            ),
                            Label::secondary(clause_span.source_id(), clause_span.clone())
                                .with_message("but this clause has a different name"),
                        ]),
                });
                continue;
            }
            if clause_arity != arity {
                errs.error(ParserError::ShowDiagnostic {
                    diagnostic: Diagnostic::error()
                        .with_message("unterminated function clause")
                        .with_labels(vec![
                            Label::primary(last_clause.source_id(), last_clause.clone())
                                .with_message(
                                "this clause ends with ';', indicating that another clause follows",
                            ),
                            Label::secondary(clause_span.source_id(), clause_span.clone())
                                .with_message("but this clause has a different arity"),
                        ]),
                });
                continue;
            }

            last_clause = clause_span.clone();
        }

        Ok(NamedFunction {
            span,
            id: nid.next(),
            name: name.clone(),
            arity,
            clauses,
            spec: None,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub span: SourceSpan,
    pub id: NodeId,
    pub arity: usize,
    pub clauses: Vec<FunctionClause>,
}
impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.clauses == other.clauses
    }
}
impl Lambda {
    pub fn new(
        errs: &mut dyn ErrorReceiver<E = ParserError, W = ParserError>,
        span: SourceSpan,
        nid: &mut NodeIdGenerator,
        clauses: Vec<FunctionClause>,
    ) -> Result<Self, ()> {
        debug_assert!(clauses.len() > 0);
        let (head, rest) = clauses.split_first().unwrap();

        let head_span = &head.span;
        let params = &head.params;
        let arity = params.len();

        // Check clauses
        let mut last_clause = head_span.clone();
        for clause in rest.iter() {
            let clause_span = &clause.span;
            let clause_name = &clause.name;
            let clause_params = &clause.params;
            let clause_arity = clause_params.len();

            if clause_name.is_some() {
                errs.error(ParserError::ShowDiagnostic {
                    diagnostic: Diagnostic::error()
                        .with_message("mismatched function clause")
                        .with_labels(vec![
                            Label::primary(clause_span.source_id(), clause_span.clone())
                                .with_message("this clause is named"),
                            Label::secondary(last_clause.source_id(), last_clause.clone())
                                .with_message(
                                "but this clause is unnamed, all clauses must share the same name",
                            ),
                        ]),
                });
                return Err(());
            }

            if clause_arity != arity {
                errs.error(ParserError::ShowDiagnostic {
                    diagnostic: Diagnostic::error()
                        .with_message("mismatched function clause")
                        .with_labels(vec![
                            Label::primary(clause_span.source_id(), clause_span.clone())
                                .with_message(
                                    "the arity of this clause does not match the previous clause",
                                ),
                            Label::secondary(last_clause.source_id(), last_clause.clone())
                                .with_message("this is the previous clause"),
                        ]),
                });
                continue;
            }

            last_clause = clause_span.clone();
        }

        Ok(Lambda {
            span,
            id: nid.next(),
            arity,
            clauses,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Named(NamedFunction),
    Unnamed(Lambda),
}
impl Function {
    pub fn span(&self) -> SourceSpan {
        match self {
            &Function::Named(NamedFunction { ref span, .. }) => span.clone(),
            &Function::Unnamed(Lambda { ref span, .. }) => span.clone(),
        }
    }
    pub fn id(&self) -> NodeId {
        match self {
            Function::Named(fun) => fun.id,
            Function::Unnamed(fun) => fun.id,
        }
    }

    pub fn new(
        errs: &mut dyn ErrorReceiver<E = ParserError, W = ParserError>,
        span: SourceSpan,
        nid: &mut NodeIdGenerator,
        clauses: Vec<FunctionClause>,
    ) -> Result<Self, ()> {
        debug_assert!(clauses.len() > 0);

        if clauses[0].name.is_some() {
            Ok(Function::Named(NamedFunction::new(
                errs, span, nid, clauses,
            )?))
        } else {
            Ok(Function::Unnamed(Lambda::new(errs, span, nid, clauses)?))
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionClause {
    pub span: SourceSpan,
    pub name: Option<Name>,
    pub params: Vec<Expr>,
    pub guard: Option<Vec<Guard>>,
    pub body: Vec<Expr>,
}
impl PartialEq for FunctionClause {
    fn eq(&self, other: &FunctionClause) -> bool {
        self.name == other.name
            && self.params == other.params
            && self.guard == other.guard
            && self.body == other.body
    }
}
impl FunctionClause {
    pub fn new(
        span: SourceSpan,
        name: Option<Name>,
        params: Vec<Expr>,
        guard: Option<Vec<Guard>>,
        body: Vec<Expr>,
    ) -> Self {
        FunctionClause {
            span,
            name,
            params,
            guard,
            body,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Guard {
    pub span: SourceSpan,
    pub conditions: Vec<Expr>,
}
impl PartialEq for Guard {
    fn eq(&self, other: &Guard) -> bool {
        self.conditions == other.conditions
    }
}
