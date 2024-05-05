#![allow(dead_code)]
use crate::find_parser::prelude::Parseable;
use thiserror::Error;
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::Parser;

//file_and_format
//filename
//format_string
//invalid_permission_comparison
//invalid_token
//missing_and_clause
//missing_closing_parenthesis
//missing_expression
//missing_list_clause
//missing_not_clause
//missing_or_clause
//permission_comparison
//string
//unexpected_token

/// Get a human-friendly string from an error reference
///
/// We use [winnow::ContextError] to pass an error 'code' but it only allows `&'static str`. This
/// method transforms the reference back into a string.
fn explain(error_reference: &str) -> String {
    match error_reference {
        "invalid_comparison" => "Invalid comparison operator",
        "invalid_format_specifier" => "Found an invalid format specifier",
        "invalid_permission_format" => "Invalid permission format",
        "invalid_size_specifier" => "Invalid size specifier",
        "invalid_type_specifier" => "Found an invalid type specifier",
        "invalid_time_specifier" => "Found an invalid time specifier",
        "symbolic_permission_level" => "Found invalid symbolic permission level",
        "symbolic_permission_symbol" => "Enountered an invalid permission symbol",
        "unsigned_integer" => "Expected an unsigned integer",
        unknown => unknown,
    }
    .into()
}

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
    #[error("Unexpected token: `{0}`")]
    InvalidToken(String),
    #[error("Failed to parse argument `{1}` of test `{0}`: {2}")]
    InvalidTestArgument(String, String, String),
    #[error("Failed to parse argument `{1}` of test `{0}`")]
    InvalidTestUnknown(String, String),
    #[error("Failed to parse argument `{1}` of action `{0}`: {2}")]
    InvalidActionArgument(String, String, String),
    #[error("Failed to parse argument `{1}` of global option `{0}`: {2}")]
    InvalidGlobalArgument(String, String, String),
    #[error("Unknown error with input: `{0}`")]
    UnknownError(String),
}

#[derive(Default, Debug)]
struct SyntaxContext {
    test: Option<String>,
    action: Option<String>,
    global: Option<String>,
    description: Option<String>,
}

impl SyntaxContext {
    fn expecting_test(&self) -> bool {
        self.test.as_ref().is_some_and(|t| t.is_empty())
    }

    fn expecting_action(&self) -> bool {
        self.action.as_ref().is_some_and(|t| t.is_empty())
    }

    fn expecting_global(&self) -> bool {
        self.global.as_ref().is_some_and(|t| t.is_empty())
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
                StrContext::Label(s) if *s == "global_option" => acc.global = Some(String::new()),
                StrContext::Label(s) if acc.expecting_global() => {
                    acc.global = Some(String::from(*s))
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

        match (
            context.test,
            context.action,
            context.global,
            context.description,
        ) {
            (Some(t), _, _, Some(d)) => SyntaxError::InvalidTestArgument(t, next, explain(&d)),
            (Some(t), _, _, None) => SyntaxError::InvalidTestUnknown(t, next),
            (_, Some(a), _, Some(d)) => SyntaxError::InvalidActionArgument(a, next, explain(&d)),
            (_, _, Some(g), Some(d)) => SyntaxError::InvalidGlobalArgument(g, next, explain(&d)),
            _ => SyntaxError::InvalidToken(next),
        }
        .into()
    }
}
