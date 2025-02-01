#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UcdParseError {
    UnsupportedCaseFoldingStatus(String),
    CommonCaseFoldingChangesWidth(u32, Vec<u32>),
    SimpleCaseFoldingChangesWidth(u32, Vec<u32>),
    TurkicCaseFoldingChangesWidth(u32, Vec<u32>),
}
