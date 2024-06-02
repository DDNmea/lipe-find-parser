#![allow(unused_variables, deprecated)]
mod error;
mod manager;
mod target_scheme;

use crate::ast::{Action, Expression, Operator};
use error::CompileError;
use manager::{DistributedSchemeManager, LocalSchemeManager, SchemeManager};
use std::rc::Rc;
use target_scheme::TargetScheme;

/// Returns a closure that will return the code needed to parse an MDT when given a path towards
/// this MDT. This allows to compile an expression once and insert the MDT path after the fact
pub fn compile<S: AsRef<str>>(
    exp: &Expression,
    options: &crate::RunOptions,
) -> Result<impl Fn(S) -> String, CompileError> {
    // This will contain the scheme code for the expression
    let mut buffer = String::new();

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
    target.compile(&mut buffer, &mut *manager)?;

    log::debug!("Frame indexes: {:?}", manager.printer_map());

    // Just the threads, for now
    let options = options
        .threads
        .and_then(|c| Some(c.to_string()))
        .unwrap_or(String::from("(lipe-getopt-thread-count)"));

    // Wrap all of that processed output into a closure and return it
    Ok(move |mdt: S| {
        let mdt = mdt.as_ref();
        format!(
            "(use-modules (lipe) (lipe find){})

(let* ({})
  (dynamic-wind
    (lambda () {})
    (lambda () (lipe-scan
        \"{mdt}\"
        (lipe-getopt-client-mount-path)
        (lambda () {buffer})
        (lipe-getopt-required-attrs)
        {options}))
    (lambda () {})))",
            manager.modules(),
            manager.definitions(),
            manager.initialization(),
            manager.terminate(),
        )
    })
}
