use std::{
    io::{BufRead, BufReader, Write},
    path::PathBuf,
    process::{Child, Command, Stdio},
};

use rustc_hash::FxHashSet;

use crate::{tag::TagError, NameRef, Root, Tag};

#[derive(Debug, thiserror::Error)]
#[error("Failed to execute `.tag/tags.sh`: {0}")]
pub struct NewError(#[from] std::io::Error);

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum TagsError {
    Write(#[from] WriteError),
    Read(#[from] ReadError),
    InvalidTag(#[from] TagForNameError),
}

#[derive(Debug, thiserror::Error)]
#[error("Failed to write to `.tag/tags.sh`: {0}")]
pub struct WriteError(std::io::Error);

#[derive(Debug, thiserror::Error)]
#[error("Failed to read from `.tag/tags.sh`: {0}")]
pub struct ReadError(std::io::Error);

#[derive(Debug, thiserror::Error)]
#[error("{0}. Tag is for `{1}`.")]
pub struct TagForNameError(TagError, PathBuf);

pub struct TagsScript {
    script: Child,
}

impl TagsScript {
    pub fn new(root: &Root) -> Result<Self, NewError> {
        Ok(Self {
            script: Command::new(root.tags())
                .current_dir(root)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()?,
        })
    }

    pub fn tags<N>(&mut self, name: N) -> Result<FxHashSet<Tag>, TagsError>
    where
        N: AsRef<NameRef>,
    {
        writeln!(
            self.script.stdin.as_mut().unwrap(),
            "{}",
            name.as_ref().as_str()
        )
        .map_err(WriteError)?;

        let mut reader = BufReader::new(self.script.stdout.as_mut().unwrap());
        let mut line = String::new();

        let mut tags = FxHashSet::default();
        loop {
            reader.read_line(&mut line).map_err(ReadError)?;
            line.pop(); // `read_line` includes the `\n`.
            if line.is_empty() {
                break;
            } else {
                tags.insert(
                    Tag::new(std::mem::take(&mut line))
                        .map_err(|e| TagForNameError(e, name.as_ref().as_path().to_path_buf()))?,
                );
            }
        }
        Ok(tags)
    }
}

impl Drop for TagsScript {
    fn drop(&mut self) {
        let _ = self.script.kill();
    }
}
