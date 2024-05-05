#![allow(dead_code)]
use crate::find_parser::prelude::Parseable;
use thiserror::Error;
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::Parser;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Syntax error: {0}")]
    SyntaxError(SyntaxError),
    #[error("Grammar error: {0}")]
    GrammarError(GrammarError),
}

impl From<SyntaxError> for ParserError {
    fn from(syn: SyntaxError) -> Self {
        Self::SyntaxError(syn)
    }
}

impl From<GrammarError> for ParserError {
    fn from(syn: GrammarError) -> Self {
        Self::GrammarError(syn)
    }
}

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[error("Failed to parse token: `{0}`")]
    InvalidToken(String),
    #[error("Failed to parse argument `{1}` of test `{0}`: expected {2}")]
    InvalidTestArgument(String, String, String),
    #[error("Failed to parse argument `{1}` of action `{0}`: expected {2}")]
    InvalidActionArgument(String, String, String),
    #[error("Unknown error with input: `{0}`")]
    UnknownError(String),
}

#[derive(Default, Debug)]
struct SyntaxContext {
    test: Option<String>,
    action: Option<String>,
    description: Option<String>,
}

impl SyntaxContext {
    fn expecting_test(&self) -> bool {
        self.test.as_ref().is_some_and(|t| t.is_empty())
    }

    fn expecting_action(&self) -> bool {
        self.action.as_ref().is_some_and(|t| t.is_empty())
    }

    fn new(raw: &Vec<&StrContext>) -> Self {
        raw.iter().fold(Self::default(), |mut acc, ctx| {
            match ctx {
                StrContext::Label(s) if *s == "test" => acc.test = Some(String::new()),
                StrContext::Label(s) if acc.expecting_test() => acc.test = Some(String::from(*s)),
                StrContext::Label(s) if *s == "action" => acc.action = Some(String::new()),
                StrContext::Label(s) if acc.expecting_action() => {
                    acc.action = Some(String::from(*s))
                }
                StrContext::Expected(StrContextValue::Description(d)) => {
                    acc.description = Some(String::from(*d))
                }
                _ => (),
            };
            acc
        })
    }
}

#[derive(Debug, Error)]
pub enum GrammarError {
    #[error("Unknown error with input: `{0}`")]
    UnknownError(String),
}

impl ParserError {
    pub fn dispatch(ctxerr: ContextError, input: &mut &str) -> Self {
        let mut context_list = ctxerr.context().collect::<Vec<_>>();
        // This now contains in order all the steps that led to this error
        context_list.reverse();
        log::debug!("Raw error context: {context_list:?}");
        let context = SyntaxContext::new(&context_list);
        log::debug!("Derived context: {context:#?}");

        // Get the next token in the input
        let next = String::parse.parse_next(input).unwrap_or(String::from(""));

        match (context.test, context.action, context.description) {
            (Some(t), _, Some(d)) => SyntaxError::InvalidTestArgument(t, next, d),
            (_, Some(a), Some(d)) => SyntaxError::InvalidActionArgument(a, next, d),
            _ => SyntaxError::InvalidToken(next),
        }
        .into()
    }
}
