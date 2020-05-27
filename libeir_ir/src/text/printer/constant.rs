use pretty::{Arena, DocAllocator, RefDoc};

use crate::{AtomicTerm, Const, ConstKind, ConstantContainer};

pub fn constant_to_doc<'a>(
    arena: &'a Arena<'a>,
    container: &ConstantContainer,
    constant: Const,
) -> RefDoc<'a, ()> {
    constant_to_doc_state(arena, container, constant, ConstantState::Normal)
}

#[derive(Debug, Copy, Clone)]
enum ConstantState {
    Normal,
    ListTail,
}

macro_rules! norm_state {
    ($arena:expr, $state:expr) => {
        match $state {
            ConstantState::Normal => $arena.nil(),
            ConstantState::ListTail => $arena
                .space()
                .append($arena.text("|"))
                .append($arena.space()),
        }
    };
}

fn constant_to_doc_state<'a>(
    arena: &'a Arena<'a>,
    container: &ConstantContainer,
    constant: Const,
    state: ConstantState,
) -> RefDoc<'a, ()> {
    match container.const_kind(constant) {
        ConstKind::Atomic(atomic) => norm_state!(arena, state)
            .append(atomic_to_doc(arena, atomic))
            .into_doc(),
        ConstKind::ListCell { head, tail } => match state {
            ConstantState::Normal => arena
                .nil()
                .append(arena.text("["))
                .append(constant_to_doc_state(
                    arena,
                    container,
                    *head,
                    ConstantState::Normal,
                ))
                .append(constant_to_doc_state(
                    arena,
                    container,
                    *tail,
                    ConstantState::ListTail,
                ))
                .append(arena.text("]"))
                .into_doc(),
            ConstantState::ListTail => arena
                .nil()
                .append(arena.text(","))
                .append(arena.space())
                .append(constant_to_doc_state(
                    arena,
                    container,
                    *head,
                    ConstantState::Normal,
                ))
                .append(constant_to_doc_state(
                    arena,
                    container,
                    *tail,
                    ConstantState::ListTail,
                ))
                .into_doc(),
        },
        ConstKind::Tuple { entries } => {
            norm_state!(arena, state)
                .append(arena.text("{"))
                .append(arena.intersperse(
                    entries.as_slice(&container.const_pool).iter().map(|c| {
                        constant_to_doc_state(arena, container, *c, ConstantState::Normal)
                    }),
                    arena.text(",").append(arena.space()),
                ))
                .append(arena.text("}"))
                .into_doc()
        }
        ConstKind::Map { keys, values } => norm_state!(arena, state)
            .append(arena.text("%{"))
            .append(
                arena.intersperse(
                    keys.as_slice(&container.const_pool)
                        .iter()
                        .zip(values.as_slice(&container.const_pool).iter())
                        .map(|(k, v)| {
                            arena
                                .nil()
                                .append(constant_to_doc_state(
                                    arena,
                                    container,
                                    *k,
                                    ConstantState::Normal,
                                ))
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(constant_to_doc_state(
                                    arena,
                                    container,
                                    *v,
                                    ConstantState::Normal,
                                ))
                        }),
                    arena.text(",").append(arena.space()),
                ),
            )
            .append(arena.text("}"))
            .into_doc(),
    }
}

fn atomic_to_doc<'a>(arena: &'a Arena<'a>, atomic: &AtomicTerm) -> RefDoc<'a, ()> {
    arena.text(format!("{}", atomic)).into_doc()
}
