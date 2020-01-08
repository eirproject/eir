use std::hash::{Hash, Hasher};

use libeir_util_datastructures::dedup_aux_primary_map::DedupPrimaryMap;
use libeir_util_datastructures::{
    dedup_aux_primary_map::DedupAuxPrimaryMap,
    aux_hash_map::{AuxHash, AuxEq},
};

use libeir_diagnostics::{CodeMap, ByteSpan, DUMMY_SPAN};
use cranelift_entity::{EntityList, ListPool, entity_impl};

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
    /// Name of module and function/stack entity
    names: Option<(String, String)>,

    /// Span in the file this was read from.
    /// While the `file`, `line` and `name` files are
    /// meant to be preserved when reading/writing
    /// to textual Eir, this span is meant to be the
    /// direct file it was read from.
    span: ByteSpan,
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
    fn aux_eq(&self, other: &Self, container: &ListPool<LocationTerminal>) -> bool {
        self.terminals.as_slice(container)
            == other.terminals.as_slice(container)
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

    pub fn lookup(&self, location: &Location) -> Vec<ByteSpan> {
        let terminals = self.locations[*location].terminals
            .as_slice(&self.terminal_pool);
        let mut locs = Vec::with_capacity(terminals.len());
        for terminal in terminals.iter().cloned() {
            let terminal_data = &self.terminals[terminal];
            locs.push(terminal_data.span.clone());
        }
        locs
    }

    pub fn location_empty(&mut self) -> Location {
        self.locations.push(LocationData {
            terminals: EntityList::new(),
        }, &mut self.terminal_pool)
    }

    pub fn location_unknown(&mut self) -> Location {
        let terminal = self.terminals.push(LocationTerminalData {
            file: None,
            line: None,
            names: None,
            span: DUMMY_SPAN,
        }, &mut ());

        let mut terminals = EntityList::new();
        terminals.push(terminal, &mut self.terminal_pool);

        self.locations.push(LocationData {
            terminals: EntityList::new(),
        }, &mut self.terminal_pool)
    }

    pub fn location(
        &mut self,
        file: Option<String>,
        line: Option<u32>,
        names: Option<(String, String)>,
        span: ByteSpan,
    ) -> Location {
        let terminal = self.terminals.push(LocationTerminalData {
            file,
            line,
            names,
            span,
        }, &mut ());

        let mut terminals = EntityList::new();
        terminals.push(terminal, &mut self.terminal_pool);

        self.locations.push(LocationData {
            terminals,
        }, &mut self.terminal_pool)
    }

    pub fn from_bytespan(&mut self, codemap: &CodeMap, span: ByteSpan, names: Option<(String, String)>) -> Location {
        let mut file = None;
        let mut line = None;

        let start_idx = span.start();
        if let Some(filemap) = codemap.find_file(start_idx) {
            file = Some(filemap.name().to_string());
            if let Ok(line_idx) = filemap.find_line(start_idx) {
                line = Some(line_idx.0);
            }
        }

        self.location(file, line, names, span)
    }

    pub fn concat_locations(&mut self, bottom: Location, top: Location) -> Location {
        let mut terminals = Vec::new();
        terminals.extend(
            self.locations[bottom].terminals
                .as_slice(&self.terminal_pool)
                .iter()
                .cloned()
        );
        terminals.extend(
            self.locations[top].terminals
                .as_slice(&self.terminal_pool)
                .iter()
                .cloned()
        );

        let mut new_terminals = EntityList::new();
        new_terminals.extend(terminals.iter().cloned(), &mut self.terminal_pool);

        self.locations.push(LocationData {
            terminals: new_terminals,
        }, &mut self.terminal_pool)
    }

}
