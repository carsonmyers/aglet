use std::collections::HashMap;
use std::fs;
use std::path::{Component::Normal, Path};

use crate::span::{FilePosition, FileSpan, Span};
use crate::Error;

pub type FileId = u32;

pub struct SourceFile {
    pub prefix:   String,
    pub filename: String,
    pub src:      String,
    pub lines:    Vec<usize>,
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

    fn map_lines(src: &str) -> Vec<usize> {
        src.lines()
            .scan(0, |state, line| {
                let res = Some(*state);
                *state = *state + line.len();
                res
            })
            .collect()
    }

    fn file_position_from_offset(&self, offset: &usize) -> Option<FilePosition> {
        self.lines
            .iter()
            .enumerate()
            .find(|(_, start_offset)| start_offset <= &offset)
            .map(|(line, start_offset)| FilePosition {
                offset: *offset,
                line:   line,
                column: offset - start_offset,
            })
    }
}

pub struct SourceMap {
    next_id: FileId,
    files:   HashMap<FileId, SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            files:   HashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: &Path) -> Result<FileId, Error> {
        let file = SourceFile::load_file(path)?;
        let id = self.next_id;
        self.next_id += 1;

        self.files.insert(id, file);
        Ok(id)
    }

    pub fn add_source(&mut self, filename: String, src: String) -> FileId {
        let file = SourceFile::new_from_source(String::new(), filename, src);
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
}
