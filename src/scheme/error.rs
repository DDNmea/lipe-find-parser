#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("Although this expression is valid, LiPE does not support this test: {0}")]
    Test(String),
    #[error("Although this expression is valid, LiPE does not support this action: {0}")]
    Action(String),
    #[error("Although this expression is valid, LiPE does not support this action: {0}")]
    Option(String),
    #[error("Although this format string is valid, LiPE does not support this formatting: {0}")]
    Format(String),
}
