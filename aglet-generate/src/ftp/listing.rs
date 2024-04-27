use std::borrow::Borrow;
use std::collections::VecDeque;
use std::ops::Deref;
use std::str::FromStr;

use eyre::eyre;

use crate::parse;

#[derive(Debug, Clone)]
pub struct RecursiveFiles {
    root:  String,
    files: VecDeque<RemoteFile>,
}

impl RecursiveFiles {
    pub fn new<S, V>(root: S, files: V) -> eyre::Result<Self>
    where
        S: Into<String>,
        V: IntoIterator<Item = RemoteFile>,
    {
        // remote files' paths are built from
        let mut root = root.into();
        root.push('/');

        let files = files
            .into_iter()
            .map(|remote_file| {
                if remote_file.path.starts_with(&root) {
                    Ok(remote_file)
                } else {
                    Err(eyre!(
                        "remote file {} is not within the specified root {}",
                        remote_file.path,
                        &root
                    ))
                }
            })
            .collect::<Result<_, _>>()?;

        Ok(Self { root, files })
    }

    pub fn iter(&self) -> RecursiveFilesRefIter {
        RecursiveFilesRefIter {
            listing: self,
            index:   0,
        }
    }
}

impl Iterator for RecursiveFiles {
    type Item = ListingFile<RemoteFile>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(remote_file) = self.files.pop_front() else {
            return None;
        };

        let root_len = self.root.len();
        let filename_offset = remote_file.path.rfind('/').unwrap_or(0);

        Some(ListingFile {
            root_len,
            filename_offset,
            remote_file,
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecursiveFilesRefIter<'a> {
    listing: &'a RecursiveFiles,
    index:   usize,
}

impl<'a> Iterator for RecursiveFilesRefIter<'a> {
    type Item = ListingFile<&'a RemoteFile>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.listing.files.len() {
            None
        } else {
            let root_len = self.listing.root.len();
            let remote_file = &self.listing.files[self.index];
            let filename_offset = remote_file
                .path
                .rfind('/')
                .expect("entry should have a parent");

            self.index += 1;

            Some(ListingFile {
                root_len,
                filename_offset,
                remote_file,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListingFile<T> {
    root_len:        usize,
    filename_offset: usize,
    remote_file:     T,
}

impl<T: Borrow<RemoteFile>> ListingFile<T> {
    pub fn relative_path(&self) -> &str {
        &self.remote_file.borrow().path[self.root_len..]
    }

    pub fn relative_parent(&self) -> &str {
        &self.remote_file.borrow().path[self.root_len..self.filename_offset]
    }
}

impl ListingFile<RemoteFile> {
    pub fn take(self) -> RemoteFile {
        self.remote_file
    }
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
    pub path: String,
    pub size: usize,
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
        ))(input)
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
        use nom::Finish;

        match Self::parse(s).finish() {
            Ok((_, entry)) => Ok(entry),
            Err(nom::error::Error { input, code }) => {
                Err(eyre!("invalid line (...{}): {:?}", input, code))
            },
        }
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
        use nom::sequence::{preceded, tuple};
        use parse::{non_space, skip_space};

        map(
            tuple((
                parse_common,
                preceded(skip_space, map(non_space, str::to_string)),
            )),
            |(size, name)| Self { name, size },
        )(input)
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
        use parse::{non_space, skip_space};

        map(
            preceded(
                parse_common,
                preceded(skip_space, map(non_space, str::to_string)),
            ),
            |name| Self { name },
        )(input)
    }
}

#[derive(Debug, Clone)]
pub struct Link {
    pub name:   String,
    pub target: String,
}

impl Link {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::combinator::map;
        use nom::sequence::{preceded, separated_pair};
        use parse::{non_space, skip_space};

        map(
            preceded(
                parse_common,
                preceded(
                    skip_space,
                    separated_pair(
                        map(non_space, str::to_string),
                        tag(" -> "),
                        map(non_space, str::to_string),
                    ),
                ),
            ),
            |(name, target)| Self { name, target },
        )(input)
    }
}

pub fn parse_common(input: &str) -> parse::Result<usize> {
    use nom::branch::alt;
    use nom::character::complete::digit1;
    use nom::combinator::map_res;
    use nom::sequence::{delimited, tuple};
    use parse::{find_digit, month, skip_space, time};

    delimited(
        tuple((find_digit, digit1, find_digit)),
        map_res(digit1, str::parse::<usize>),
        tuple((
            skip_space,
            month,
            skip_space,
            digit1,
            skip_space,
            alt((time, digit1)),
        )),
    )(input)
}
