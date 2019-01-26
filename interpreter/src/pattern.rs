use std::collections::HashMap;

use ::num_bigint::BigInt;

use ::term::Term;
use core_erlang_compiler::ir::lir::{ Source, Clause };
use core_erlang_compiler::ir::hir::{ Pattern, PatternNode };
use core_erlang_compiler::parser::AtomicLiteral;
use core_erlang_compiler::ir::SSAVariable;
use core_erlang_compiler::Variable;

#[derive(Debug, Copy, Clone)]
pub enum MatchState {
    MatchClause(usize),
    GuardWait(usize),
    Finished,
}

impl MatchState {

    fn clause_num(&self) -> usize {
        match self {
            MatchState::MatchClause(num) => *num,
            MatchState::GuardWait(num) => *num,
            _ => panic!(),
        }
    }

    fn clause_num_mut(&mut self) -> &mut usize {
        match self {
            MatchState::MatchClause(num) => num,
            _ => panic!(),
        }
    }

    fn into_guard(&mut self) {
        match *self {
            MatchState::MatchClause(num) => *self = MatchState::GuardWait(num),
            _ => unreachable!(),
        }
    }

    fn into_body(&mut self) {
        match *self {
            MatchState::GuardWait(num) => *self = MatchState::MatchClause(num+1),
            _ => unreachable!(),
        }
    }

    fn into_finished(&mut self) {
        *self = MatchState::Finished;
    }

}

#[derive(Debug, Clone)]
pub struct CaseContext {
    pub state: MatchState,
    pub vars: Vec<Term>,
    pub clauses: Vec<Clause>,
    pub last_binds: Option<HashMap<SSAVariable, Term>>,
}

fn match_node(term: &Term, node: &PatternNode,
              binds: &mut HashMap<SSAVariable, Term>,
              binds_ref: &Vec<(Variable, SSAVariable)>) -> bool {
    println!("MATCH_NODE: {:?} {:?}", term, node);
    match (term, node) {
        (Term::List(ref t_head, ref t_tail),
         PatternNode::List(ref p_head, ref p_tail)) => {
            if t_head.len() < p_head.len() {
                unimplemented!();
            } else if t_head.len() == p_head.len() {
                for (pat, term) in p_head.iter().zip(t_head.iter()) {
                    if !match_node(term, pat, binds, binds_ref) {
                        return false;
                    }
                }
                return match_node(t_tail, p_tail, binds, binds_ref);
            } else { // >
                println!("PAT GT");
                assert!(t_head.len() > p_head.len());
                for (pat, term) in p_head.iter().zip(t_head.iter()) {
                    println!("HEAD MATCH TRY");
                    if !match_node(term, pat, binds, binds_ref) {
                        println!("HEAD MATCH FAIL");
                        return false;
                    }
                }
                println!("REST");
                let head_rest: Vec<_> = t_head.iter().skip(p_head.len())
                    .cloned().collect();
                println!("HEAD REST: {:?}", head_rest);
                let rest_term = Term::List(head_rest, t_tail.clone());
                println!("REST TERM: {:?}", rest_term);
                let a = match_node(&rest_term, p_tail, binds, binds_ref);
                println!("DONE {}", a);
                return a;
            }
        }
        (Term::Atom(v1), PatternNode::Atomic(AtomicLiteral::Atom(v2))) => v1 == v2,
        (Term::Nil, PatternNode::Atomic(AtomicLiteral::Nil)) => true,
        (Term::Nil, PatternNode::List(ref list, ref tail)) if list.len() == 0 =>
            match_node(term, tail, binds, binds_ref),
        (Term::Integer(ref int),
         PatternNode::Atomic(AtomicLiteral::Integer(ref pat_int))) => {
            let mut bi = BigInt::parse_bytes(pat_int.digits.as_bytes(), 10)
                .unwrap();
            if !pat_int.sign {
                bi *= -1;
            }
            println!("Int pattern {} {}", int, bi);
            int == &bi
        }
        (_, PatternNode::BindVar(var_name, i_node)) => {
            binds.insert(
                binds_ref.iter().find(|(k, _)| k == var_name).unwrap().1,
                term.clone()
            );
            match_node(term, i_node, binds, binds_ref)
        },
        (_, PatternNode::Wildcard) => true,
        _ => {
            println!("Warning: Pattern matching incomplete");
            false
        },
    }
}

impl CaseContext {

    pub fn new(vars: Vec<Term>, clauses: Vec<Clause>) -> Self {
        CaseContext {
            state: MatchState::MatchClause(0),
            vars: vars,
            clauses: clauses,
            last_binds: None,
        }
    }

    pub fn do_body(&mut self) -> usize {
        let (matched, values) = {
            let clause = &self.clauses[self.state.clause_num()];
            assert!(clause.patterns.len() == self.vars.len());

            println!("{:?}", clause);
            println!("{:?}", self.vars);

            let mut values: HashMap<SSAVariable, Term> = HashMap::new();
            let matched = self.vars.iter()
                .zip(&clause.patterns)
                .all(|(term, pattern)| match_node(
                    term, &pattern.node, &mut values, &pattern.binds));
            (matched, values)
        };

        if matched {
            let clause_num = self.state.clause_num();
            self.state.into_guard();
            self.last_binds = Some(values);
            clause_num + 1
        } else {
            *self.state.clause_num_mut() += 1;
            if self.state.clause_num() >= self.clauses.len() {
                0
            } else {
                self.do_body()
            }
        }

        //println!("PAT: {:?}", self.clauses[0]);
        //println!("TERMS: {:?}", terms);
    }

    pub fn case_values(&self) -> HashMap<SSAVariable, Term> {
        self.last_binds.as_ref().unwrap().clone()
    }

    pub fn guard_ok(&mut self) {
        self.state.into_finished();
    }

    pub fn guard_fail(&mut self, clause_num: usize) {
        assert!(clause_num == self.state.clause_num());
        self.state.into_body();
    }

}
