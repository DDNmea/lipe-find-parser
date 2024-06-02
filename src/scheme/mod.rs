#![allow(unused_variables, deprecated)]
mod error;
mod manager;
mod target_scheme;

use crate::ast::{Action, Expression, Operator};
use error::CompileError;
use manager::{DistributedSchemeManager, LocalSchemeManager, SchemeManager, Target};
use std::collections::HashMap;
use std::rc::Rc;
use target_scheme::TargetScheme;

pub struct CompiledExpression {
    policy_body: String,
    options: String,
    modules: String,
    definitions: String,
    initialization: String,
    terminate: String,
    io_map: Option<HashMap<usize, Target>>,
}

/// Returns a closure that will return the code needed to parse an MDT when given a path towards
/// this MDT. This allows to compile an expression once and insert the MDT path after the fact
pub fn compile(
    exp: &Expression,
    options: &crate::RunOptions,
) -> Result<CompiledExpression, CompileError> {
    // This will contain the scheme code for the expression
    let mut policy_body = String::new();

    // We determine here if the expression needs special handling because of its IO pattern
    // This affects what helper struct to use during compilation
    let mut manager: Box<dyn SchemeManager> = if exp.complex_frames() {
        Box::new(DistributedSchemeManager::default())
    } else {
        Box::new(LocalSchemeManager::default())
    };

    // If the expression does not contain an explicit action, the user assumes to print and we wrap
    // everything using And(expression, Action::Print)
    let target = if !exp.action() {
        Expression::Operator(Rc::new(Operator::And(
            exp.clone(),
            Expression::Action(Action::DefaultPrint),
        )))
    } else {
        exp.clone()
    };

    // Compile the expression
    target.compile(&mut policy_body, &mut *manager)?;

    // Just the threads, for now
    let options = options
        .threads
        .and_then(|c| Some(c.to_string()))
        .unwrap_or(String::from("(lipe-getopt-thread-count)"));

    Ok(CompiledExpression {
        policy_body,
        options,
        modules: manager.modules().into(),
        definitions: manager.definitions(),
        initialization: manager.initialization(),
        terminate: manager.terminate(),
        io_map: manager.printer_map(),
    })
}

impl CompiledExpression {
    pub fn scheme<S: AsRef<str>>(&self, mdt: S) -> String {
        let mdt = mdt.as_ref();
        format!(
            "(use-modules (lipe) (lipe find){})

(let* ({})
  (dynamic-wind
    (lambda () {})
    (lambda () (lipe-scan
        \"{mdt}\"
        (lipe-getopt-client-mount-path)
        (lambda () {})
        (lipe-getopt-required-attrs)
        {}))
    (lambda () {})))",
            self.modules,
            self.definitions,
            self.initialization,
            self.policy_body,
            self.options,
            self.terminate,
        )
    }

    pub fn io_map(&self) -> Option<HashMap<usize, Target>> {
        self.io_map.clone()
    }
}
