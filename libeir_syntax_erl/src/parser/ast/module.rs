use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::{HashMap, HashSet};

use libeir_diagnostics::{Diagnostic, Label, SourceSpan};
use libeir_util_number::ToPrimitive;
use libeir_util_parse::ErrorReceiver;

use super::NodeIdGenerator;
use super::ParserError;
use super::{Apply, Cons, Nil, Remote, Tuple, Var};
use super::{Attribute, Deprecation, UserAttribute};
use super::{Callback, Record, TypeDef, TypeSig, TypeSpec};
use super::{Expr, Ident, Literal, Symbol};
use super::{
    FunctionClause, FunctionName, LocalFunctionName, NamedFunction, PartiallyResolvedFunctionName,
    ResolvedFunctionName,
};

/// Represents expressions valid at the top level of a module body
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Attribute(Attribute),
    Record(Record),
    Function(NamedFunction),
}

#[derive(Debug, Clone)]
pub struct DefinedRecord {
    pub record: Record,
    pub field_idx_map: HashMap<Ident, usize>,
}
impl PartialEq for DefinedRecord {
    fn eq(&self, other: &Self) -> bool {
        self.record == other.record
    }
}

/// Represents a complete module, broken down into its constituent parts
///
/// Creating a module via `Module::new` ensures that each field is correctly
/// populated, that sanity checking of the top-level constructs is performed,
/// and that a module is ready for semantic analysis and lowering to IR
///
/// A key step performed by `Module::new` is decorating `ResolvedFunctionName`
/// structs with the current module where appropriate (as this is currently not
/// done during parsing, as the module is constructed last). This means that once
/// constructed, one can use `ResolvedFunctionName` equality in sets/maps, which
/// allows us to easily check definitions, usages, and more.
#[derive(Debug, Clone)]
pub struct Module {
    pub span: SourceSpan,
    pub name: Ident,
    pub vsn: Option<Expr>,
    pub author: Option<Expr>,
    pub compile: Option<CompileOptions>,
    pub on_load: Option<LocalFunctionName>,
    pub imports: HashMap<LocalFunctionName, ResolvedFunctionName>,
    pub exports: HashSet<LocalFunctionName>,
    pub types: HashMap<LocalFunctionName, TypeDef>,
    pub exported_types: HashSet<LocalFunctionName>,
    pub behaviours: HashSet<Ident>,
    pub callbacks: HashMap<LocalFunctionName, Callback>,
    pub records: HashMap<Symbol, DefinedRecord>,
    pub attributes: HashMap<Ident, UserAttribute>,
    pub functions: BTreeMap<LocalFunctionName, NamedFunction>,
    // Used for module-level deprecation
    pub deprecation: Option<Deprecation>,
    // Used for function-level deprecation
    pub deprecations: HashSet<Deprecation>,
}
impl Module {
    /// Called by the parser to create the module once all of the top-level expressions have been
    /// parsed, in other words this is the last function called when parsing a module.
    ///
    /// As a result, this function performs some initial linting of the module:
    ///
    /// * If configured to do so, warns if functions are missing type specs
    /// * Warns about type specs for undefined functions
    /// * Warns about redefined attributes
    /// * Errors on invalid syntax in built-in attributes (e.g. -import(..))
    /// * Errors on mismatched function clauses (name/arity)
    /// * Errors on unterminated function clauses
    /// * Errors on redefined functions
    ///
    /// And a few other similar lints
    pub fn new(
        errs: &mut dyn ErrorReceiver<E = ParserError, W = ParserError>,
        span: SourceSpan,
        nid: &mut NodeIdGenerator,
        name: Ident,
        mut body: Vec<TopLevel>,
    ) -> Self {
        let mut module = Module {
            span,
            name,
            vsn: None,
            author: None,
            on_load: None,
            compile: None,
            imports: HashMap::new(),
            exports: HashSet::new(),
            types: HashMap::new(),
            exported_types: HashSet::new(),
            behaviours: HashSet::new(),
            callbacks: HashMap::new(),
            records: HashMap::new(),
            attributes: HashMap::new(),
            functions: BTreeMap::new(),
            deprecation: None,
            deprecations: HashSet::new(),
        };

        // Functions will be decorated with their type specs as they are added
        // to the module. To accomplish this, we keep track of seen type specs
        // as they are defined, then later look up the spec for a function when
        // a definition is encountered
        let mut specs: HashMap<ResolvedFunctionName, TypeSpec> = HashMap::new();

        // Walk every top-level expression and extend our initial module definition accordingly
        for item in body.drain(..) {
            match item {
                TopLevel::Attribute(Attribute::Vsn(aspan, vsn)) => {
                    if module.vsn.is_none() {
                        module.vsn = Some(vsn);
                        continue;
                    }
                    let module_vsn_span = module.vsn.as_ref().map(|v| v.span()).unwrap();
                    let module_vsn_source_id = module_vsn_span.source_id();
                    errs.error(ParserError::ShowDiagnostic {
                        diagnostic: Diagnostic::error()
                            .with_message("attribute is already defined")
                            .with_labels(vec![
                                Label::primary(aspan.source_id(), aspan)
                                    .with_message("redefinition occurs here"),
                                Label::secondary(module_vsn_source_id, module_vsn_span)
                                    .with_message("first defined here"),
                            ]),
                    });
                }
                TopLevel::Attribute(Attribute::Author(aspan, author)) => {
                    if module.author.is_none() {
                        module.author = Some(author);
                        continue;
                    }
                    let module_author_span = module.author.as_ref().map(|v| v.span()).unwrap();
                    let module_author_source_id = module_author_span.source_id();
                    errs.error(ParserError::ShowDiagnostic {
                        diagnostic: Diagnostic::error()
                            .with_message("attribute is already defined")
                            .with_labels(vec![
                                Label::primary(aspan.source_id(), aspan)
                                    .with_message("redefinition occurs here"),
                                Label::secondary(module_author_source_id, module_author_span)
                                    .with_message("first defined here"),
                            ]),
                    });
                }
                TopLevel::Attribute(Attribute::OnLoad(aspan, fname)) => {
                    if module.on_load.is_none() {
                        module.on_load = Some(fname.to_local());
                        continue;
                    }
                    let module_onload_span = module.on_load.as_ref().map(|v| v.span).unwrap();
                    let module_onload_source_id = module_onload_span.source_id();
                    errs.error(ParserError::ShowDiagnostic {
                        diagnostic: Diagnostic::error()
                            .with_message("on_load can only be defined once")
                            .with_labels(vec![
                                Label::primary(aspan.source_id(), aspan)
                                    .with_message("redefinition occurs here"),
                                Label::secondary(module_onload_source_id, module_onload_span)
                                    .with_message("first defined here"),
                            ]),
                    });
                }
                TopLevel::Attribute(Attribute::Import(aspan, from_module, mut imports)) => {
                    for import in imports.drain(..) {
                        let import = import.resolve(from_module.clone());
                        match module.imports.get(&import.to_local()) {
                            None => {
                                module.imports.insert(import.to_local(), import);
                            }
                            Some(ResolvedFunctionName {
                                span: ref prev_span,
                                ..
                            }) => {
                                errs.error(ParserError::ShowDiagnostic {
                                    diagnostic: Diagnostic::warning()
                                        .with_message("unused import")
                                        .with_labels(vec![
                                            Label::primary(aspan.source_id(), aspan).with_message(
                                                "this import is a duplicate of a previous import",
                                            ),
                                            Label::secondary(
                                                prev_span.source_id(),
                                                prev_span.clone(),
                                            )
                                            .with_message("function was first imported here"),
                                        ]),
                                });
                            }
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Export(aspan, mut exports)) => {
                    for export in exports.drain(..) {
                        match module.exports.get(&export.to_local()) {
                            None => {
                                module.exports.insert(export.to_local());
                            }
                            Some(LocalFunctionName {
                                span: ref prev_span,
                                ..
                            }) => {
                                errs.error(ParserError::ShowDiagnostic {
                                    diagnostic: Diagnostic::warning()
                                        .with_message("already exported")
                                        .with_labels(vec![
                                            Label::primary(aspan.source_id(), aspan)
                                                .with_message("duplicate export occurs here"),
                                            Label::secondary(
                                                prev_span.source_id(),
                                                prev_span.clone(),
                                            )
                                            .with_message("function was first exported here"),
                                        ]),
                                });
                            }
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Type(ty)) => {
                    let arity = ty.params.len();
                    let type_name = ResolvedFunctionName {
                        span: ty.name.span.clone(),
                        id: nid.next(),
                        module: module.name.clone(),
                        function: ty.name.clone(),
                        arity,
                    };
                    match module.types.get(&type_name.to_local()) {
                        None => {
                            module.types.insert(type_name.to_local(), ty);
                        }
                        Some(TypeDef {
                            span: ref prev_span,
                            ..
                        }) => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::warning()
                                    .with_message("type is already defined")
                                    .with_labels(vec![
                                        Label::primary(ty.span.source_id(), ty.span)
                                            .with_message("redefinition occurs here"),
                                        Label::secondary(prev_span.source_id(), prev_span.clone())
                                            .with_message("type was first defined here"),
                                    ]),
                            });
                        }
                    }
                }
                TopLevel::Attribute(Attribute::ExportType(aspan, mut exports)) => {
                    for export in exports.drain(..) {
                        match module.exported_types.get(&export.to_local()) {
                            None => {
                                module.exported_types.insert(export.to_local());
                            }
                            Some(LocalFunctionName {
                                span: ref prev_span,
                                ..
                            }) => {
                                errs.error(ParserError::ShowDiagnostic {
                                    diagnostic: Diagnostic::warning()
                                        .with_message("type already exported")
                                        .with_labels(vec![
                                            Label::primary(aspan.source_id(), aspan)
                                                .with_message("duplicate export occurs here"),
                                            Label::secondary(
                                                prev_span.source_id(),
                                                prev_span.clone(),
                                            )
                                            .with_message("type was first exported here"),
                                        ]),
                                });
                            }
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Behaviour(aspan, b_module)) => {
                    match module.behaviours.get(&b_module) {
                        None => {
                            module.behaviours.insert(b_module);
                        }
                        Some(Ident {
                            span: ref prev_span,
                            ..
                        }) => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::warning()
                                    .with_message("duplicate behaviour declaration")
                                    .with_labels(vec![
                                        Label::primary(aspan.source_id(), aspan)
                                            .with_message("duplicate declaration occurs here"),
                                        Label::secondary(prev_span.source_id(), prev_span.clone())
                                            .with_message("first declaration occurs here"),
                                    ]),
                            });
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Callback(callback)) => {
                    let first_sig = callback.sigs.first().unwrap();
                    let arity = first_sig.params.len();

                    // Verify that all clauses match
                    if callback.sigs.len() > 1 {
                        for TypeSig {
                            span: ref sigspan,
                            ref params,
                            ..
                        } in &callback.sigs[1..]
                        {
                            if params.len() != arity {
                                errs.error(ParserError::ShowDiagnostic {
                                    diagnostic: Diagnostic::error()
                                        .with_message("mismatched arity")
                                        .with_labels(vec![
                                            Label::primary(sigspan.source_id(), sigspan.clone())
                                                .with_message(format!(
                                                    "expected arity of {}",
                                                    arity
                                                )),
                                            Label::secondary(
                                                first_sig.span.source_id(),
                                                first_sig.span.clone(),
                                            )
                                            .with_message(
                                                "expected arity was derived from this clause",
                                            ),
                                        ]),
                                });
                            }
                        }
                    }
                    // Check for redefinition
                    let cb_name = ResolvedFunctionName {
                        span: callback.span.clone(),
                        id: nid.next(),
                        module: module.name.clone(),
                        function: callback.function.clone(),
                        arity,
                    };
                    match module.callbacks.get(&cb_name.to_local()) {
                        None => {
                            module.callbacks.insert(cb_name.to_local(), callback);
                            continue;
                        }
                        Some(ref a @ Callback {
                            //span: ref prev_span,
                            ..
                        }) => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::error()
                                    .with_message("cannot redefine callback")
                                    .with_labels(vec![
                                        Label::primary(callback.span.source_id(), callback.span)
                                            .with_message("redefinition occurs here"),
                                        Label::secondary(a.span.source_id(), a.span.clone())
                                            .with_message("callback first defined here")
                                    ]),
                            });
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Spec(typespec)) => {
                    let first_sig = typespec.sigs.first().unwrap();
                    let arity = first_sig.params.len();

                    // Verify that all clauses match
                    if typespec.sigs.len() > 1 {
                        for TypeSig {
                            span: ref sigspan,
                            ref params,
                            ..
                        } in &typespec.sigs[1..]
                        {
                            if params.len() != arity {
                                errs.error(ParserError::ShowDiagnostic {
                                    diagnostic: Diagnostic::error()
                                        .with_message("mismatched arity")
                                        .with_labels(vec![
                                            Label::primary(sigspan.source_id(), sigspan.clone())
                                                .with_message(format!(
                                                    "expected arity of {}",
                                                    arity
                                                )),
                                            Label::secondary(
                                                first_sig.span.source_id(),
                                                first_sig.span.clone(),
                                            )
                                            .with_message(
                                                "expected arity was derived from this clause",
                                            ),
                                        ]),
                                });
                            }
                        }
                    }
                    // Check for redefinition
                    let spec_name = ResolvedFunctionName {
                        span: typespec.span.clone(),
                        id: nid.next(),
                        module: module.name.clone(),
                        function: typespec.function.clone(),
                        arity,
                    };
                    match specs.get(&spec_name) {
                        None => {
                            specs.insert(spec_name.clone(), typespec);
                        }
                        Some(TypeSpec {
                            span: ref prev_span,
                            ..
                        }) => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::error()
                                    .with_message("spec already defined")
                                    .with_labels(vec![
                                        Label::primary(typespec.span.source_id(), typespec.span)
                                            .with_message("redefinition occurs here"),
                                        Label::secondary(prev_span.source_id(), prev_span.clone())
                                            .with_message("spec first defined here"),
                                    ]),
                            });
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Compile(_, compile)) => match module.compile {
                    None => {
                        let (opts, mut validation_errs) =
                            CompileOptions::from_expr(&module.name, &compile);
                        module.compile = Some(opts);
                        for diagnostic in validation_errs.drain(..) {
                            errs.error(ParserError::ShowDiagnostic { diagnostic });
                        }
                        continue;
                    }
                    Some(ref mut opts) => {
                        if let Err(mut validation_errs) =
                            opts.merge_from_expr(&module.name, &compile)
                        {
                            for diagnostic in validation_errs.drain(..) {
                                errs.error(ParserError::ShowDiagnostic { diagnostic });
                            }
                        }
                    }
                },
                TopLevel::Attribute(Attribute::Deprecation(mut deprecations)) => {
                    for deprecation in deprecations.drain(..) {
                        match deprecation {
                            Deprecation::Module {
                                span: ref dspan, ..
                            } => match module.deprecation {
                                None => {
                                    module.deprecation = Some(deprecation);
                                }
                                Some(Deprecation::Module {
                                    span: ref orig_span,
                                    ..
                                }) => {
                                    errs.error(ParserError::ShowDiagnostic {
                                        diagnostic: Diagnostic::warning()
                                            .with_message("redundant deprecation")
                                            .with_labels(vec![
                                                Label::primary(dspan.source_id(), dspan.clone())
                                                    .with_message("this module is already deprecated by a previous declaration"),
                                                Label::secondary(orig_span.source_id(), orig_span.clone())
                                                    .with_message("deprecation first declared here")
                                            ]),
                                    });
                                }
                                Some(Deprecation::Function { .. }) => unreachable!(),
                            },
                            Deprecation::Function {
                                span: ref fspan, ..
                            } => {
                                if let Some(Deprecation::Module {
                                    span: ref mspan, ..
                                }) = module.deprecation
                                {
                                    errs.error(ParserError::ShowDiagnostic {
                                        diagnostic: Diagnostic::warning()
                                            .with_message("redundant deprecation")
                                            .with_labels(vec![
                                                Label::primary(fspan.source_id(), *fspan)
                                                    .with_message("module is deprecated, so deprecating functions is redundant"),
                                                Label::secondary(mspan.source_id(), mspan.clone())
                                                    .with_message("module deprecation occurs here")
                                            ]),
                                    });
                                    continue;
                                }

                                match module.deprecations.get(&deprecation) {
                                    None => {
                                        module.deprecations.insert(deprecation);
                                    }
                                    Some(Deprecation::Function {
                                        span: ref prev_span,
                                        ..
                                    }) => {
                                        errs.error(ParserError::ShowDiagnostic {
                                            diagnostic: Diagnostic::warning()
                                                .with_message("redundant deprecation")
                                                .with_labels(vec![
                                                    Label::primary(fspan.source_id(), *fspan)
                                                        .with_message("this function is already deprecated by a previous declaration"),
                                                    Label::secondary(prev_span.source_id(), prev_span.clone())
                                                        .with_message("deprecation first declared here")
                                                ])
                                        });
                                    }
                                    Some(Deprecation::Module { .. }) => unreachable!(),
                                }
                            }
                        }
                    }
                }
                TopLevel::Attribute(Attribute::Custom(attr)) => {
                    match attr.name.name.as_str().get() {
                        "module" => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::error()
                                    .with_message("multiple module declarations")
                                    .with_labels(vec![
                                        Label::primary(attr.span.source_id(), attr.span.clone())
                                            .with_message("invalid declaration occurs here"),
                                        Label::secondary(
                                            module.name.span.source_id(),
                                            module.name.span.clone(),
                                        )
                                        .with_message("module first declared here"),
                                    ]),
                            });
                            continue;
                        }
                        "optional_callbacks" => {
                            //if let Some(callback) = module.callbacks.get_mut()
                            continue;
                        }
                        "dialyzer" => {
                            // Drop dialyzer attributes as they are unused
                            continue;
                        }
                        _ => (),
                    }
                    match module.attributes.get(&attr.name) {
                        None => {
                            module.attributes.insert(attr.name.clone(), attr);
                        }
                        Some(UserAttribute {
                            span: ref prev_span,
                            ..
                        }) => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::warning()
                                    .with_message("redefined attribute")
                                    .with_labels(vec![
                                        Label::primary(attr.span.source_id(), attr.span.clone())
                                            .with_message("redefinition occurs here"),
                                        Label::secondary(prev_span.source_id(), prev_span.clone())
                                            .with_message("previously defined here"),
                                    ]),
                            });
                            module.attributes.insert(attr.name.clone(), attr);
                        }
                    }
                }
                TopLevel::Record(mut record) => {
                    let name = record.name.name.clone();
                    match module.records.get(&name) {
                        None => {
                            // FIXME: Remove the set when hashmap gets api
                            // for getting keys.
                            let mut fields = HashSet::<Ident>::new();
                            let mut field_idx_map = HashMap::<Ident, usize>::new();
                            for (idx, field) in record.fields.iter_mut().enumerate() {
                                if field.value.is_none() {
                                    field.value = Some(atom!(nid, undefined));
                                }
                                if let Some(prev) = fields.get(&field.name) {
                                    errs.error(ParserError::ShowDiagnostic {
                                        diagnostic: Diagnostic::error()
                                            .with_message("duplicate field in record")
                                            .with_labels(vec![
                                                Label::primary(
                                                    field.name.span.source_id(),
                                                    field.name.span,
                                                )
                                                .with_message("duplicate field occurs here"),
                                                Label::secondary(prev.span.source_id(), prev.span)
                                                    .with_message("previous field"),
                                            ]),
                                    });
                                }
                                fields.insert(field.name);
                                field_idx_map.insert(field.name, idx);
                            }
                            module.records.insert(
                                name,
                                DefinedRecord {
                                    record,
                                    field_idx_map,
                                },
                            );
                        }
                        Some(prev) => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::error()
                                    .with_message("record already defined")
                                    .with_labels(vec![
                                        Label::primary(record.span.source_id(), record.span)
                                            .with_message("duplicate definition occurs here"),
                                        Label::secondary(
                                            prev.record.span.source_id(),
                                            prev.record.span,
                                        )
                                        .with_message("previously defined here"),
                                    ]),
                            });
                        }
                    }
                }
                TopLevel::Function(mut function @ NamedFunction { .. }) => {
                    let name = &function.name;
                    let resolved_name = ResolvedFunctionName {
                        span: name.span.clone(),
                        id: nid.next(),
                        module: module.name.clone(),
                        function: name.clone(),
                        arity: function.arity,
                    };
                    let warn_missing_specs = module
                        .compile
                        .as_ref()
                        .map(|c| c.warn_missing_spec)
                        .unwrap_or(false);
                    function.spec = match specs.get(&resolved_name) {
                        None if warn_missing_specs => {
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::warning()
                                    .with_message("missing function spec")
                                    .with_labels(vec![Label::primary(
                                        function.span.source_id(),
                                        function.span.clone(),
                                    )
                                    .with_message("expected type spec for this function")]),
                            });
                            None
                        }
                        None => None,
                        Some(spec) => Some(spec.clone()),
                    };
                    match module.functions.entry(resolved_name.to_local()) {
                        Entry::Vacant(f) => {
                            f.insert(function);
                        }
                        Entry::Occupied(initial_def) => {
                            let def = initial_def.into_mut();
                            errs.error(ParserError::ShowDiagnostic {
                                diagnostic: Diagnostic::error()
                                    .with_message(
                                        "clauses from the same function should be grouped together",
                                    )
                                    .with_labels(vec![
                                        Label::primary(
                                            function.span.source_id(),
                                            function.span.clone(),
                                        )
                                        .with_message("found more clauses here"),
                                        Label::secondary(def.span.source_id(), def.span.clone())
                                            .with_message("function is first defined here"),
                                    ]),
                            });
                            def.clauses.append(&mut function.clauses);
                        }
                    }
                }
            }
        }

        // Ensure internal pseudo-locals are defined
        module.define_pseudolocals(nid);

        // Auto imports
        module.add_auto_imports(nid);

        // Verify on_load function exists
        if let Some(ref on_load_name) = module.on_load {
            if !module.functions.contains_key(on_load_name) {
                errs.error(ParserError::ShowDiagnostic {
                    diagnostic: Diagnostic::error()
                        .with_message("invalid on_load function")
                        .with_labels(vec![Label::primary(
                            on_load_name.span.source_id(),
                            on_load_name.span.clone(),
                        )
                        .with_message("this function is not defined in this module")]),
                });
            }
        }

        // Check for orphaned type specs
        for (spec_name, spec) in &specs {
            if !module.functions.contains_key(&spec_name.to_local()) {
                errs.warning(ParserError::ShowDiagnostic {
                    diagnostic: Diagnostic::warning()
                        .with_message("type spec for undefined function")
                        .with_labels(vec![Label::primary(
                            spec.span.source_id(),
                            spec.span.clone(),
                        )
                        .with_message("this type spec has no corresponding function definition")]),
                });
            }
        }

        module
    }

    fn add_auto_imports(&mut self, nid: &mut NodeIdGenerator) {
        macro_rules! auto_imports {
            ($($m:ident : $f:ident / $a:expr),*) => {
                [
                    $(
                        ResolvedFunctionName {
                            span: self.name.span,
                            id: nid.next(),
                            module: Ident::from_str(stringify!($m)),
                            function: Ident::from_str(stringify!($f)),
                            arity: $a,
                        },
                    )*
                ]
            }
        }

        let autos = auto_imports! {
            erlang:abs/1,
            erlang:apply/2,
            erlang:apply/3,
            erlang:atom_to_binary/2,
            erlang:atom_to_list/1,
            erlang:binary_part/2,
            erlang:binary_part/3,
            erlang:binary_to_atom/2,
            erlang:binary_to_existing_atom/2,
            erlang:binary_to_float/1,
            erlang:binary_to_integer/1,
            erlang:binary_to_integer/2,
            erlang:binary_to_list/1,
            erlang:binary_to_list/3,
            erlang:binary_to_term/1,
            erlang:binary_to_term/2,
            erlang:bit_size/1,
            erlang:bitstring_to_list/1,
            erlang:byte_size/1,
            erlang:ceil/1,
            erlang:check_old_code/1,
            erlang:check_process_code/2,
            erlang:check_process_code/3,
            erlang:date/0,
            erlang:delete_module/1,
            erlang:demonitor/1,
            erlang:demonitor/2,
            erlang:disconnect_node/1,
            erlang:element/2,
            erlang:erase/0,
            erlang:erase/1,
            erlang:error/1,
            erlang:error/2,
            erlang:exit/1,
            erlang:exit/2,
            erlang:float/1,
            erlang:float_to_binary/1,
            erlang:float_to_binary/2,
            erlang:float_to_list/1,
            erlang:float_to_list/2,
            erlang:floor/1,
            erlang:garbage_collect/0,
            erlang:garbage_collect/1,
            erlang:garbage_collect/2,
            erlang:get/0,
            erlang:get/1,
            erlang:get_keys/0,
            erlang:get_keys/1,
            erlang:group_leader/0,
            erlang:group_leader/2,
            erlang:halt/0,
            erlang:halt/1,
            erlang:halt/2,
            erlang:hd/1,
            erlang:integer_to_binary/1,
            erlang:integer_to_binary/2,
            erlang:integer_to_list/1,
            erlang:integer_to_list/2,
            erlang:iolist_size/1,
            erlang:iolist_to_binary/1,
            erlang:iolist_to_iovec/1,
            erlang:is_alive/0,
            erlang:is_atom/1,
            erlang:is_binary/1,
            erlang:is_bitstring/1,
            erlang:is_boolean/1,
            erlang:is_float/1,
            erlang:is_function/1,
            erlang:is_function/2,
            erlang:is_integer/1,
            erlang:is_list/1,
            erlang:is_map/1,
            erlang:is_map_key/2,
            erlang:is_number/1,
            erlang:is_pid/1,
            erlang:is_port/1,
            erlang:is_process_alive/1,
            erlang:is_record/2,
            erlang:is_record/3,
            erlang:is_reference/1,
            erlang:is_tuple/1,
            erlang:length/1,
            erlang:link/1,
            erlang:list_to_atom/1,
            erlang:list_to_binary/1,
            erlang:list_to_bitstring/1,
            erlang:list_to_existing_atom/1,
            erlang:list_to_float/1,
            erlang:list_to_integer/1,
            erlang:list_to_integer/2,
            erlang:list_to_pid/1,
            erlang:list_to_port/1,
            erlang:list_to_ref/1,
            erlang:list_to_tuple/1,
            erlang:load_module/2,
            erlang:make_ref/0,
            erlang:map_get/2,
            erlang:map_size/1,
            erlang:max/2,
            erlang:min/2,
            erlang:module_loaded/1,
            erlang:monitor/2,
            erlang:monitor_node/2,
            erlang:node/0,
            erlang:node/1,
            erlang:nodes/0,
            erlang:nodes/1,
            erlang:now/0,
            erlang:open_port/2,
            erlang:pid_to_list/1,
            erlang:port_close/1,
            erlang:port_command/2,
            erlang:port_command/3,
            erlang:port_connect/2,
            erlang:port_control/3,
            erlang:port_to_list/1,
            erlang:pre_loaded/0,
            erlang:process_flag/2,
            erlang:process_flag/3,
            erlang:process_info/1,
            erlang:process_info/2,
            erlang:processes/0,
            erlang:purge_module/1,
            erlang:put/2,
            erlang:ref_to_list/1,
            erlang:register/2,
            erlang:registered/0,
            erlang:round/1,
            erlang:setelement/3,
            erlang:self/0,
            erlang:size/1,
            erlang:spawn/1,
            erlang:spawn/2,
            erlang:spawn/3,
            erlang:spawn/4,
            erlang:spawn_link/1,
            erlang:spawn_link/2,
            erlang:spawn_link/3,
            erlang:spawn_link/4,
            erlang:spawn_monitor/1,
            erlang:spawn_monitor/3,
            erlang:spawn_opt/2,
            erlang:spawn_opt/3,
            erlang:spawn_opt/4,
            erlang:spawn_opt/5,
            erlang:split_binary/2,
            erlang:statistics/1,
            erlang:term_to_binary/1,
            erlang:term_to_binary/2,
            erlang:throw/1,
            erlang:time/0,
            erlang:tl/1,
            erlang:trunc/1,
            erlang:tuple_size/1,
            erlang:tuple_to_list/1,
            erlang:unlink/1,
            erlang:unregister/1,
            erlang:whereis/1
        };

        if let Some(compile) = self.compile.as_ref() {
            if compile.no_auto_import {
                return;
            }

            for fun in autos.iter() {
                if !compile.no_auto_imports.contains(fun) {
                    self.imports.insert(fun.to_local(), fun.clone());
                }
            }
        } else {
            for fun in autos.iter() {
                self.imports.insert(fun.to_local(), fun.clone());
            }
        }
    }

    // Every module in Erlang has some functions implicitly defined for internal use:
    //
    // * `module_info/0` (exported)
    // * `module_info/1` (exported)
    // * `record_info/2`
    // * `behaviour_info/1` (optional)
    fn define_pseudolocals(&mut self, nid: &mut NodeIdGenerator) {
        let mod_info_0 = fun!(nid, module_info () ->
            apply!(nid, remote!(nid, erlang, get_module_info), Expr::Literal(Literal::Atom(nid.next(), self.name.clone())))
        );
        let mod_info_1 = fun!(nid, module_info (Key) ->
            apply!(nid, remote!(nid, erlang, get_module_info), Expr::Literal(Literal::Atom(nid.next(), self.name.clone())), var!(nid, Key))
        );

        self.define_function(mod_info_0);
        self.define_function(mod_info_1);

        if self.callbacks.len() > 0 {
            let callbacks = self.callbacks.iter().fold(nil!(nid), |acc, (cbname, cb)| {
                if cb.optional {
                    acc
                } else {
                    cons!(
                        nid,
                        tuple!(
                            nid,
                            atom_from_sym!(nid, cbname.function.name.clone()),
                            int!(nid, (cbname.arity as i64).into())
                        ),
                        acc
                    )
                }
            });
            let opt_callbacks = self.callbacks.iter().fold(nil!(nid), |acc, (cbname, cb)| {
                if cb.optional {
                    cons!(
                        nid,
                        tuple!(
                            nid,
                            atom_from_sym!(nid, cbname.function.name.clone()),
                            int!(nid, (cbname.arity as i64).into())
                        ),
                        acc
                    )
                } else {
                    acc
                }
            });

            let behaviour_info_1 = fun!(nid, behaviour_info
                                        (atom!(nid, callbacks)) -> callbacks;
                                        (atom!(nid, optional_callbacks)) -> opt_callbacks);

            self.define_function(behaviour_info_1);
        }
    }

    fn define_function(&mut self, f: NamedFunction) {
        let name = ResolvedFunctionName {
            span: f.span.clone(),
            id: f.id,
            module: self.name.clone(),
            function: f.name.clone(),
            arity: f.arity,
        };
        self.functions.insert(name.to_local(), f);
    }
}
impl PartialEq for Module {
    fn eq(&self, other: &Module) -> bool {
        if self.name != other.name {
            return false;
        }
        if self.vsn != other.vsn {
            return false;
        }
        if self.on_load != other.on_load {
            return false;
        }
        if self.imports != other.imports {
            return false;
        }
        if self.exports != other.exports {
            return false;
        }
        if self.types != other.types {
            return false;
        }
        if self.exported_types != other.exported_types {
            return false;
        }
        if self.behaviours != other.behaviours {
            return false;
        }
        if self.callbacks != other.callbacks {
            return false;
        }
        if self.records != other.records {
            return false;
        }
        if self.attributes != other.attributes {
            return false;
        }
        if self.functions != other.functions {
            return false;
        }
        true
    }
}

/// This structure holds all module-specific compiler options
/// and configuration; it is passed through all phases of
/// compilation and is a superset of options in CompilerSettings
/// where applicable
#[derive(Debug, Clone)]
pub struct CompileOptions {
    // Same as erlc, prints informational warnings about
    // binary matching optimizations
    pub compile_info: HashMap<Symbol, Expr>,
    // Used to override the filename used in errors/warnings
    pub file: Option<String>,
    // Treats all warnings as errors
    pub warnings_as_errors: bool,
    // Disables warnings
    pub no_warn: bool,
    // Exports all functions
    pub export_all: bool,
    // Prevents auto importing any functions
    pub no_auto_import: bool,
    // Prevents auto importing the specified functions
    pub no_auto_imports: HashSet<ResolvedFunctionName>,
    // Warns if export_all is used
    pub warn_export_all: bool,
    // Warns when exported variables are used
    pub warn_export_vars: bool,
    // Warns when variables are shadowed
    pub warn_shadow_vars: bool,
    // Warns when a function is unused
    pub warn_unused_function: bool,
    // Disables the unused function warning for the specified functions
    pub no_warn_unused_functions: HashSet<LocalFunctionName>,
    // Warns about unused imports
    pub warn_unused_import: bool,
    // Warns about unused variables
    pub warn_unused_var: bool,
    // Warns about unused records
    pub warn_unused_record: bool,
    // Warns about missing type specs
    pub warn_missing_spec: bool,
    // Inlines the given functions
    pub inline_functions: HashSet<ResolvedFunctionName>,
}
impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions {
            compile_info: HashMap::new(),
            file: None,
            warnings_as_errors: false,
            no_warn: false,
            export_all: false,
            no_auto_import: false,
            no_auto_imports: HashSet::new(),
            warn_export_all: true,
            warn_export_vars: true,
            warn_shadow_vars: true,
            warn_unused_function: true,
            no_warn_unused_functions: HashSet::new(),
            warn_unused_import: true,
            warn_unused_var: true,
            warn_unused_record: true,
            warn_missing_spec: false,
            inline_functions: HashSet::new(),
        }
    }
}
impl CompileOptions {
    pub fn from_expr(module: &Ident, expr: &Expr) -> (Self, Vec<Diagnostic>) {
        let mut opts = CompileOptions::default();
        match opts.merge_from_expr(module, expr) {
            Ok(_) => (opts, Vec::new()),
            Err(errs) => (opts, errs),
        }
    }

    pub fn merge_from_expr(&mut self, module: &Ident, expr: &Expr) -> Result<(), Vec<Diagnostic>> {
        self.set_option(module, expr)
    }

    fn set_option(&mut self, module: &Ident, expr: &Expr) -> Result<(), Vec<Diagnostic>> {
        let mut diagnostics = Vec::new();
        match expr {
            // e.g. -compile(export_all).
            &Expr::Literal(Literal::Atom(id, ref option_name)) => {
                match option_name.as_str().get() {
                    "export_all" => self.export_all = true,
                    "nowarn_export_all" => self.warn_export_all = false,
                    "nowarn_shadow_vars" => self.warn_shadow_vars = false,
                    "nowarn_unused_function" => self.warn_unused_function = false,
                    "nowarn_unused_vars" => self.warn_unused_var = false,
                    "no_auto_import" => self.no_auto_import = true,
                    "inline_list_funcs" => {
                        let funs = [
                            ("lists", "all", 2),
                            ("lists", "any", 2),
                            ("lists", "foreach", 2),
                            ("lists", "map", 2),
                            ("lists", "flatmap", 2),
                            ("lists", "filter", 2),
                            ("lists", "foldl", 3),
                            ("lists", "foldr", 3),
                            ("lists", "mapfoldl", 3),
                            ("lists", "mapfoldr", 3),
                        ];
                        for (m, f, a) in funs.iter() {
                            self.inline_functions.insert(ResolvedFunctionName {
                                span: option_name.span,
                                id: id,
                                module: Ident::from_str(m),
                                function: Ident::from_str(f),
                                arity: *a,
                            });
                        }
                    }
                    _name => {
                        diagnostics.push(
                            Diagnostic::warning()
                                .with_message("invalid compile option")
                                .with_labels(vec![Label::primary(
                                    option_name.span.source_id(),
                                    option_name.span,
                                )
                                .with_message(
                                    "this option is either unsupported or unrecognized",
                                )]),
                        );
                    }
                }
            }
            // e.g. -compile([export_all, nowarn_unused_function]).
            &Expr::Cons(Cons {
                ref head, ref tail, ..
            }) => self.compiler_opts_from_list(&mut diagnostics, module, to_list(head, tail)),
            // e.g. -compile({nowarn_unused_function, [some_fun/0]}).
            &Expr::Tuple(Tuple { ref elements, .. }) if elements.len() == 2 => {
                //if let Some((head, tail)) = elements.split_first() {
                if let &Expr::Literal(Literal::Atom(_id, ref option_name)) = &elements[0] {
                    let list = to_list_simple(&elements[1]);
                    match option_name.as_str().get() {
                        "no_auto_import" => {
                            self.no_auto_imports(&mut diagnostics, module, &list);
                        }
                        "nowarn_unused_function" => {
                            self.no_warn_unused_functions(&mut diagnostics, module, &list);
                        }
                        "inline" => {
                            self.inline_functions(&mut diagnostics, module, &list);
                        }
                        "hipe" => {
                            // Should we warn about this? I'm inclined to think
                            // not, since we want to ignore warning spam.
                        }
                        _name => {
                            diagnostics.push(
                                Diagnostic::warning()
                                    .with_message("invalid compile option")
                                    .with_labels(vec![Label::primary(
                                        option_name.span.source_id(),
                                        option_name.span,
                                    )
                                    .with_message(
                                        "this option is either unsupported or unrecognized",
                                    )]),
                            );
                        }
                    }
                }
                //}
            }
            term => {
                let term_span = term.span();
                diagnostics.push(
                    Diagnostic::warning()
                        .with_message("invalid compile option")
                        .with_labels(vec![Label::primary(term_span.source_id(), term_span)
                            .with_message(
                                "unexpected expression: expected atom, list, or tuple",
                            )]),
                );
            }
        }

        if diagnostics.len() > 0 {
            return Err(diagnostics);
        }

        Ok(())
    }

    fn compiler_opts_from_list(
        &mut self,
        diagnostics: &mut Vec<Diagnostic>,
        module: &Ident,
        options: Vec<Expr>,
    ) {
        for option in options {
            match self.set_option(module, &option) {
                Ok(_) => continue,
                Err(mut diags) => diagnostics.append(&mut diags),
            }
        }
    }

    fn no_auto_imports(
        &mut self,
        diagnostics: &mut Vec<Diagnostic>,
        module: &Ident,
        imports: &[Expr],
    ) {
        for import in imports {
            match import {
                Expr::FunctionName(FunctionName::PartiallyResolved(name)) => {
                    self.no_auto_imports.insert(name.resolve(module.clone()));
                }
                Expr::Tuple(tup) if tup.elements.len() == 2 => {
                    match (&tup.elements[0], &tup.elements[1]) {
                        (
                            Expr::Literal(Literal::Atom(_, name)),
                            Expr::Literal(Literal::Integer(_, _, arity)),
                        ) => {
                            let local = PartiallyResolvedFunctionName {
                                span: tup.span,
                                id: tup.id,
                                function: *name,
                                arity: (*arity).to_usize().unwrap(),
                            };
                            self.no_auto_imports.insert(local.resolve(*module));
                            continue;
                        }
                        _ => (),
                    }
                }
                other => {
                    let other_span = other.span();
                    diagnostics.push(
                        Diagnostic::warning()
                            .with_message("invalid compile option")
                            .with_labels(vec![Label::primary(other_span.source_id(), other_span)
                                .with_message(
                                    "expected function name/arity term for no_auto_imports",
                                )]),
                    );
                }
            }
        }
    }

    fn no_warn_unused_functions(
        &mut self,
        diagnostics: &mut Vec<Diagnostic>,
        _module: &Ident,
        funs: &[Expr],
    ) {
        for fun in funs {
            match fun {
                Expr::FunctionName(FunctionName::PartiallyResolved(name)) => {
                    self.no_warn_unused_functions.insert(name.to_local());
                }
                other => {
                    let other_span = other.span();
                    diagnostics.push(
                        Diagnostic::warning()
                            .with_message("invalid compile option")
                            .with_labels(vec![Label::primary(other_span.source_id(), other_span)
                                .with_message(
                                "expected function name/arity term for no_warn_unused_functions",
                            )]),
                    );
                }
            }
        }
    }

    fn inline_functions(
        &mut self,
        diagnostics: &mut Vec<Diagnostic>,
        module: &Ident,
        funs: &[Expr],
    ) {
        for fun in funs {
            match fun {
                Expr::FunctionName(FunctionName::PartiallyResolved(name)) => {
                    self.inline_functions.insert(name.resolve(*module));
                    continue;
                }
                Expr::Tuple(tup) if tup.elements.len() == 2 => {
                    match (&tup.elements[0], &tup.elements[1]) {
                        (
                            Expr::Literal(Literal::Atom(_, name)),
                            Expr::Literal(Literal::Integer(_, _, arity)),
                        ) => {
                            let local = PartiallyResolvedFunctionName {
                                span: tup.span,
                                id: tup.id,
                                function: *name,
                                arity: (*arity).to_usize().unwrap(),
                            };
                            self.inline_functions.insert(local.resolve(*module));
                            continue;
                        }
                        _ => (),
                    }
                }
                _ => (),
            }

            let fun_span = fun.span();
            diagnostics.push(
                Diagnostic::warning()
                    .with_message("invalid compile option")
                    .with_labels(vec![Label::primary(fun_span.source_id(), fun_span)
                        .with_message("expected function name/arity term for inline")]),
            );
        }
    }
}

fn to_list_simple(mut expr: &Expr) -> Vec<Expr> {
    let mut list = Vec::new();
    loop {
        match expr {
            Expr::Cons(cons) => {
                list.push((*cons.head).clone());
                expr = &cons.tail;
            }
            Expr::Nil(_) => {
                return list;
            }
            _ => {
                list.push(expr.clone());
                return list;
            }
        }
    }
}

fn to_list(head: &Expr, tail: &Expr) -> Vec<Expr> {
    let mut list = Vec::new();
    match head {
        &Expr::Cons(Cons {
            head: ref head2,
            tail: ref tail2,
            ..
        }) => {
            let mut h = to_list(head2, tail2);
            list.append(&mut h);
        }
        expr => list.push(expr.clone()),
    }
    match tail {
        &Expr::Cons(Cons {
            head: ref head2,
            tail: ref tail2,
            ..
        }) => {
            let mut t = to_list(head2, tail2);
            list.append(&mut t);
        }
        &Expr::Nil(_) => (),
        expr => list.push(expr.clone()),
    }

    list
}
