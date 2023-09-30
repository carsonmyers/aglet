use std::collections::HashMap;
use std::fs;
use std::path::{Component::Normal, Path};

use crate::span::{FilePosition, FileSpan, Span};
use crate::Error;

pub type FileId = u32;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub prefix: String,
    pub filename: String,
    pub src: String,
    pub lines: Vec<usize>,
}

impl SourceFile {
    pub fn load_file(path: &Path) -> Result<Self, Error> {
        let prefix = String::from(
            path.canonicalize()?
                .parent()
                .map(|p| p.to_path_buf())
                .unwrap_or_default()
                .to_str()
                .ok_or_else(|| Error::InvalidFilename(path.to_path_buf()))?,
        );

        let filename = String::from(match path.components().last() {
            Some(Normal(p)) => p
                .to_str()
                .ok_or_else(|| Error::InvalidFilename(path.to_path_buf())),
            _ => Err(Error::InvalidFilename(path.to_path_buf())),
        }?);

        let src = fs::read_to_string(path)?;

        Ok(SourceFile::new_from_source(prefix, filename, src))
    }

    pub fn new_from_source(prefix: String, filename: String, src: String) -> Self {
        let lines = SourceFile::map_lines(&src);

        Self {
            prefix,
            filename,
            src,
            lines,
        }
    }

    pub fn file_position_from_offset(&self, offset: &usize) -> Option<FilePosition> {
        self.lines
            .iter()
            .enumerate()
            .rev()
            .find(|(_, start_offset)| start_offset <= &offset)
            .map(|(line, start_offset)| FilePosition {
                offset: *offset,
                line: line + 1,
                column: offset - start_offset + 1,
            })
    }

    fn map_lines(src: &str) -> Vec<usize> {
        src.lines()
            .scan(0, |state, line| {
                let res = Some(*state);
                *state = *state + line.len() + 1;
                res
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct SourceMap {
    next_id: FileId,
    files: HashMap<FileId, SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            files: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, file: SourceFile) -> FileId {
        let id = self.next_id;
        self.next_id += 1;

        self.files.insert(id, file);
        id
    }

    pub fn get_span(&self, file_id: &FileId, span: &Span) -> Option<FileSpan> {
        let source = self.files.get(file_id)?;
        let start = source.file_position_from_offset(&span.start)?;
        let end = source.file_position_from_offset(&span.end)?;

        Some(FileSpan {
            file_id: *file_id,
            start,
            end,
        })
    }

    pub fn get_file(&self, file_id: &FileId) -> Option<&SourceFile> {
        self.files.get(file_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_init() {
        let input = vec!["abcd", "", "efgh"].join("\n");
        let file =
            SourceFile::new_from_source("<test>".to_string(), "test_file".to_string(), input.clone());

        assert_eq!(file.prefix, "<test>".to_string());
        assert_eq!(file.filename, "test_file".to_string());
        assert_eq!(file.src, input);
        assert_eq!(file.lines, vec![0, 5, 6]);
    }

    #[test]
    fn test_file_positions() {
        let input = vec![
            "01234", // 0-5
            "67", // 6-8
            "", // 9
            "012", // 10-12
        ].join("\n");
        let file = SourceFile::new_from_source("".to_string(), "<test>".to_string(), input);

        let pos = file.file_position_from_offset(&3).expect("pos in bounds");
        assert_eq!(pos.offset, 3);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 4);

        let pos = file.file_position_from_offset(&5).expect("pos in bounds");
        assert_eq!(pos.offset, 5);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 6);

        let pos = file.file_position_from_offset(&7).expect("pos in bounds");
        assert_eq!(pos.offset, 7);
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 2);

        let pos = file.file_position_from_offset(&11).expect("pos in bounds");
        assert_eq!(pos.offset, 11);
        assert_eq!(pos.line, 4);
        assert_eq!(pos.column, 2);
    }
}
