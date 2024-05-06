use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Although this expression is valid, LiPE does not support this test: {0}")]
    UnsupportedTest(String),
    #[error("Although this expression is valid, LiPE does not support this action: {0}")]
    UnsupportedAction(String),
    #[error("Although this expression is valid, LiPE does not support this action: {0}")]
    UnsupportedOption(String),
    #[error("Although this format string is valid, LiPE does not support this formatting: {0}")]
    UnsupportedFormat(String),
}
