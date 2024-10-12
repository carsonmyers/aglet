use std::borrow::Borrow;
use std::collections::VecDeque;
use std::ops::Deref;
use std::str::FromStr;

use crate::parse;
use eyre::eyre;
use eyre::WrapErr;
use nom::Parser;

#[derive(Debug, Clone)]
pub struct RecursiveFiles {
    root: Vec<String>,
    files: VecDeque<RemoteFile>,
}

impl RecursiveFiles {
    pub fn new<S, V>(root: S, files: V) -> eyre::Result<Self>
    where
        S: AsRef<Vec<String>>,
        V: IntoIterator<Item = RemoteFile>,
    {
        let root = root.as_ref().to_vec();

        let files = files
            .into_iter()
            .map(|remote_file| {
                if remote_file.path.starts_with(&root) {
                    Ok(remote_file)
                } else {
                    Err(eyre!(
                        "remote file {} is not within the specified root {}",
                        remote_file.path.join("/"),
                        root.join("/")
                    ))
                }
            })
            .collect::<Result<_, _>>()?;

        Ok(Self { root, files })
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn size(&self) -> usize {
        self.files.iter().map(|file| file.size).sum()
    }
}

impl Iterator for RecursiveFiles {
    type Item = ListingFile<RemoteFile>;

    fn next(&mut self) -> Option<Self::Item> {
        let remote_file = self.files.pop_front()?;
        let root_len = self.root.len();

        Some(ListingFile {
            root_len,
            remote_file,
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecursiveFilesRefIter<'a> {
    listing: &'a RecursiveFiles,
    index: usize,
}

impl<'a> Iterator for RecursiveFilesRefIter<'a> {
    type Item = ListingFile<&'a RemoteFile>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.listing.files.len() {
            None
        } else {
            let root_len = self.listing.root.len();
            let remote_file = &self.listing.files[self.index];

            self.index += 1;

            Some(ListingFile {
                root_len,
                remote_file,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListingFile<T> {
    root_len: usize,
    remote_file: T,
}

impl<T: Borrow<RemoteFile>> ListingFile<T> {
    pub fn relative_segments(&self) -> &[String] {
        &self.remote_file.borrow().path[self.root_len..]
    }

    pub fn relative_parent_segments(&self) -> &[String] {
        let path = &self.remote_file.borrow().path;
        &path[self.root_len..path.len() - 1]
    }

    pub fn size(&self) -> usize {
        self.remote_file.borrow().size
    }
}

impl ListingFile<RemoteFile> {
    // pub fn take(self) -> RemoteFile {
    //     self.remote_file
    // }
}

impl<T> Deref for ListingFile<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.remote_file
    }
}

/// A file stored on a server
///
/// The difference between `RemoteFile` and the `File` used in `Entry::File` is that `File` only
/// contains the filename (with no path) and `RemoteFile` contains the full path to the file's
/// location on the server.
#[derive(Debug, Clone)]
pub struct RemoteFile {
    pub path: Vec<String>,
    pub size: usize,
}

impl RemoteFile {
    pub fn new<P, Q>(prefix: P, name: Q, size: usize) -> Self
    where
        P: AsRef<[String]>,
        Q: AsRef<str>,
    {
        let mut path = prefix.as_ref().to_vec();
        path.push(name.as_ref().to_string());

        Self { path, size }
    }
}

/// An entry in a remote directory listing
///
/// Entries are parsed from lines of the FTP `ls` command as specified by
/// [this specification](https://files.stairways.com/other/ftp-list-specs-info.txt)
///
/// An entry contains no metadata about the server or directory it was listed from
#[derive(Debug, Clone)]
pub enum Entry {
    File(File),
    Directory(Directory),
    Link(Link),
}

impl Entry {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::combinator::map;
        use nom::sequence::preceded;

        alt((
            preceded(tag("-"), map(File::parse, Entry::File)),
            preceded(tag("d"), map(Directory::parse, Entry::Directory)),
            preceded(tag("l"), map(Link::parse, Entry::Link)),
        ))
        .parse(input)
    }

    pub fn file(self) -> Option<File> {
        match self {
            Entry::File(file) => Some(file),
            _ => None,
        }
    }

    pub fn directory(self) -> Option<Directory> {
        match self {
            Entry::Directory(directory) => Some(directory),
            _ => None,
        }
    }

    pub fn link(self) -> Option<Link> {
        match self {
            Entry::Link(link) => Some(link),
            _ => None,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Entry::File(file) => file.name.as_str(),
            Entry::Directory(dir) => dir.name.as_str(),
            Entry::Link(link) => link.name.as_str(),
        }
    }
}

impl FromStr for Entry {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::finish(Self::parse(s)).wrap_err("failed to parse entry")
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub name: String,
    pub size: usize,
}

impl File {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::combinator::map;
        use nom::sequence::preceded;
        use parse::{non_spaces, spaces};

        map(
            (
                parse_common,
                preceded(spaces, map(non_spaces, String::from)),
            ),
            |(size, name)| Self { name, size },
        )
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Directory {
    pub name: String,
}

impl Directory {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::combinator::map;
        use nom::sequence::preceded;
        use parse::{non_spaces, spaces};

        map(
            preceded(
                parse_common,
                preceded(spaces, map(non_spaces, String::from)),
            ),
            |name| Self { name },
        )
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Link {
    pub name: String,
    pub target: String,
}

impl Link {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::character::complete::space0;
        use nom::combinator::map;
        use nom::sequence::{preceded, separated_pair};
        use parse::non_spaces;

        map(
            preceded(
                parse_common,
                preceded(
                    space0,
                    separated_pair(
                        map(non_spaces, str::to_string),
                        tag(" -> "),
                        map(non_spaces, str::to_string),
                    ),
                ),
            ),
            |(name, target)| Self { name, target },
        )
        .parse(input)
    }
}

pub fn parse_common(input: &str) -> parse::Result<usize> {
    use nom::branch::alt;
    use nom::character::complete::{digit1, space0};
    use nom::combinator::map_res;
    use nom::sequence::delimited;
    use parse::ftp::{month, time};
    use parse::non_digits;

    delimited(
        (non_digits, digit1, non_digits),
        map_res(digit1, str::parse::<usize>),
        (space0, month, space0, digit1, space0, alt((time, digit1))),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directory() {
        use nom::branch::alt;
        use nom::character::complete::{digit1, space0};
        use nom::combinator::map_res;
        use nom::sequence::delimited;
        use parse::ftp::{month, time};
        use parse::non_digits;

        let line = "dr-xr-xr-x   5 ftp      ftp          4096 Jun 19  2017 10.0.0";

        /*
        delimited(
            (non_digits, digit1, non_digits),
            map_res(digit1, str::parse::<usize>),
            (space0, month, space0, digit1, space0, alt((time, digit1))),
        );
         */

        dbg!(Directory::parse(line));
    }
}
