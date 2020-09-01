use std::hash::{Hash, Hasher};

use libeir_util_datastructures::dedup_aux_primary_map::DedupPrimaryMap;
use libeir_util_datastructures::{
    aux_traits::{AuxEq, AuxHash},
    dedup_aux_primary_map::DedupAuxPrimaryMap,
};

use cranelift_entity::{entity_impl, EntityList, ListPool};
use libeir_diagnostics::{CodeMap, SourceSpan};

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location(u32);
entity_impl!(Location, "loc");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocationTerminal(u32);
entity_impl!(LocationTerminal, "loc_terminal");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct LocationTerminalData {
    /// Path to display for the origin file
    file: Option<String>,
    /// Line number in origin file
    line: Option<u32>,

    /// Name of module
    module: Option<String>,
    /// Name of function/stack entity
    entity: Option<String>,

    /// Span in the file this was read from.
    /// While the `file`, `line` and `name` files are
    /// meant to be preserved when reading/writing
    /// to textual Eir, this span is meant to be the
    /// direct file it was read from.
    span: SourceSpan,
}

impl AuxHash<()> for LocationTerminalData {
    fn aux_hash<H: Hasher>(&self, state: &mut H, _container: &()) {
        self.hash(state)
    }
}
impl AuxEq<()> for LocationTerminalData {
    fn aux_eq(&self, rhs: &Self, _self_aux: &(), _rhs_aux: &()) -> bool {
        self.eq(rhs)
    }
}

#[derive(Debug, Clone)]
struct LocationData {
    terminals: EntityList<LocationTerminal>,
}
impl AuxHash<ListPool<LocationTerminal>> for LocationData {
    fn aux_hash<H: Hasher>(&self, state: &mut H, container: &ListPool<LocationTerminal>) {
        self.terminals.as_slice(container).hash(state)
    }
}
impl AuxEq<ListPool<LocationTerminal>> for LocationData {
    fn aux_eq(
        &self,
        other: &Self,
        self_aux: &ListPool<LocationTerminal>,
        other_aux: &ListPool<LocationTerminal>,
    ) -> bool {
        self.terminals.as_slice(self_aux) == other.terminals.as_slice(other_aux)
    }
}

#[derive(Debug, Clone)]
pub struct LocationContainer {
    terminals: DedupPrimaryMap<LocationTerminal, LocationTerminalData>,
    locations: DedupAuxPrimaryMap<Location, LocationData, ListPool<LocationTerminal>>,

    terminal_pool: ListPool<LocationTerminal>,
}

impl LocationContainer {
    pub fn new() -> Self {
        let locations = DedupAuxPrimaryMap::new();
        let terminals = DedupPrimaryMap::new();
        let terminal_pool = ListPool::new();

        LocationContainer {
            locations,
            terminals,
            terminal_pool,
        }
    }

    pub fn lookup(&self, location: &Location) -> Vec<SourceSpan> {
        let terminals = self.locations[*location]
            .terminals
            .as_slice(&self.terminal_pool);
        let mut locs = Vec::with_capacity(terminals.len());
        for terminal in terminals.iter().cloned() {
            let terminal_data = &self.terminals[terminal];
            locs.push(terminal_data.span.clone());
        }
        locs
    }

    pub fn location_empty(&mut self) -> Location {
        self.locations.push(
            LocationData {
                terminals: EntityList::new(),
            },
            &mut self.terminal_pool,
        )
    }

    pub fn location_unknown(&mut self) -> Location {
        let terminal = self.terminals.push(
            LocationTerminalData {
                file: None,
                line: None,
                module: None,
                entity: None,
                span: SourceSpan::UNKNOWN,
            },
            &mut (),
        );

        let mut terminals = EntityList::new();
        terminals.push(terminal, &mut self.terminal_pool);

        self.locations.push(
            LocationData {
                terminals: EntityList::new(),
            },
            &mut self.terminal_pool,
        )
    }

    pub fn terminal(
        &mut self,
        file: Option<String>,
        line: Option<u32>,
        module: Option<String>,
        entity: Option<String>,
        span: SourceSpan,
    ) -> LocationTerminal {
        self.terminals.push(
            LocationTerminalData {
                file,
                line,
                module,
                entity,
                span,
            },
            &mut (),
        )
    }

    pub fn location(
        &mut self,
        file: Option<String>,
        line: Option<u32>,
        module: Option<String>,
        entity: Option<String>,
        span: SourceSpan,
    ) -> Location {
        let terminal = self.terminal(file, line, module, entity, span);

        let mut terminals = EntityList::new();
        terminals.push(terminal, &mut self.terminal_pool);

        self.locations
            .push(LocationData { terminals }, &mut self.terminal_pool)
    }

    pub fn terminal_from_bytespan(
        &mut self,
        codemap: &CodeMap,
        span: SourceSpan,
        module: Option<String>,
        entity: Option<String>,
    ) -> LocationTerminal {
        let mut file = None;
        let mut line = None;

        let start_idx = span.start_index();
        if let Some(filemap) = codemap.get(span.source_id()) {
            file = Some(filemap.name().to_string());
            let line_idx = filemap.line_index(start_idx);
            line = Some(line_idx.0);
        }

        self.terminal(file, line, module, entity, span)
    }

    pub fn from_bytespan(
        &mut self,
        codemap: &CodeMap,
        span: SourceSpan,
        module: Option<String>,
        entity: Option<String>,
    ) -> Location {
        let terminal = self.terminal_from_bytespan(codemap, span, module, entity);

        let mut terminals = EntityList::new();
        terminals.push(terminal, &mut self.terminal_pool);

        self.locations
            .push(LocationData { terminals }, &mut self.terminal_pool)
    }

    pub fn from_terminals(&mut self, terminals: &[LocationTerminal]) -> Location {
        let mut n_terminals = EntityList::new();
        n_terminals.extend(terminals.iter().cloned(), &mut self.terminal_pool);

        self.locations.push(
            LocationData {
                terminals: n_terminals,
            },
            &mut self.terminal_pool,
        )
    }

    pub fn concat_locations(&mut self, bottom: Location, top: Location) -> Location {
        let mut terminals = Vec::new();
        terminals.extend(
            self.locations[bottom]
                .terminals
                .as_slice(&self.terminal_pool)
                .iter()
                .cloned(),
        );
        terminals.extend(
            self.locations[top]
                .terminals
                .as_slice(&self.terminal_pool)
                .iter()
                .cloned(),
        );

        let mut new_terminals = EntityList::new();
        new_terminals.extend(terminals.iter().cloned(), &mut self.terminal_pool);

        self.locations.push(
            LocationData {
                terminals: new_terminals,
            },
            &mut self.terminal_pool,
        )
    }

    pub fn location_eq(&self, l_loc: Location, r: &Self, r_loc: Location) -> bool {
        let l_n = &self.locations[l_loc];
        let r_n = &r.locations[r_loc];

        let l_ts = l_n.terminals.as_slice(&self.terminal_pool);
        let r_ts = r_n.terminals.as_slice(&r.terminal_pool);

        if l_ts.len() != r_ts.len() {
            return false;
        }

        for (l_t, r_t) in l_ts.iter().zip(r_ts.iter()) {
            let l_i = &self.terminals[*l_t];
            let r_i = &r.terminals[*r_t];

            if l_i.file != r_i.file
                || l_i.module != r_i.module
                || l_i.entity != r_i.entity
                || l_i.line != r_i.line
            {
                return false;
            }
        }

        return true;
    }

    pub fn format_loc(&self, loc: Location) -> String {
        use std::fmt::Write;

        let loc_inner = &self.locations[loc];
        let mut out = String::new();

        write!(&mut out, "[").unwrap();
        for term in loc_inner.terminals.as_slice(&self.terminal_pool) {
            let term_inner = &self.terminals[*term];

            if let Some(file) = &term_inner.file {
                write!(&mut out, "{:?}:", file).unwrap();
            } else {
                write!(&mut out, "nil:").unwrap();
            }
            if let Some(line) = &term_inner.line {
                write!(&mut out, "{:?}@", line).unwrap();
            } else {
                write!(&mut out, "nil@").unwrap();
            }
            if let Some(module) = &term_inner.module {
                write!(&mut out, "{:?}:", module).unwrap();
            } else {
                write!(&mut out, "nil:").unwrap();
            }
            if let Some(entity) = &term_inner.entity {
                write!(&mut out, "{:?}", entity).unwrap();
            } else {
                write!(&mut out, "nil").unwrap();
            }

            write!(&mut out, ", ").unwrap();
        }
        write!(&mut out, "]").unwrap();

        out
    }
}
