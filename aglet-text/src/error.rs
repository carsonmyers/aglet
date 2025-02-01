#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("invalid codepoint: {0:x}")]
    InvalidCodepoint(u32),
    #[error("invalid input filename: {0}")]
    InvalidFilename(std::path::PathBuf),
    #[error("unsupported unicode context: {0}")]
    UnsupportedUnicodeContext(String),
    #[error("io error: {0}")]
    IOError(#[from] std::io::Error),
}
