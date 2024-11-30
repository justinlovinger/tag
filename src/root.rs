use std::path::{Path, PathBuf};

use crate::{NameRef, TagRef, TaggedPath, FILES_DIR, METADATA_DIR, PROGRAM_TAGS_DIR, TAGS_DIR};

#[derive(Debug, Clone)]
pub struct Root {
    path: PathBuf,
    metadata: PathBuf,
    files: PathBuf,
    tags: PathBuf,
}

impl Root {
    pub fn new(path: PathBuf) -> std::io::Result<Option<Self>> {
        let metadata = path.join(METADATA_DIR);
        let files = metadata.join(FILES_DIR);
        let tags = metadata.join(TAGS_DIR);
        if metadata.is_dir() && files.is_dir() && tags.is_dir() {
            Ok(Some(Self {
                path,
                metadata,
                files,
                tags,
            }))
        } else {
            Ok(None)
        }
    }

    pub fn from_child<P>(path: P) -> std::io::Result<Option<Self>>
    where
        P: AsRef<Path>,
    {
        for path in path.as_ref().ancestors() {
            if let Some(root) = Self::new(path.to_owned())? {
                return Ok(Some(root));
            }
            if path.parent().map_or(false, |parent| {
                parent.ends_with(PathBuf::from(METADATA_DIR).join(FILES_DIR))
            }) {
                return Ok(None);
            }
            if TaggedPath::from_path(path.to_owned()).is_ok() {
                return Ok(None);
            }
        }
        Ok(None)
    }

    pub fn into_path(self) -> PathBuf {
        self.path
    }

    pub fn as_path(&self) -> &Path {
        &self.path
    }

    pub fn join<P>(&self, path: P) -> PathBuf
    where
        P: AsRef<Path>,
    {
        self.as_path().join(path)
    }

    pub fn metadata(&self) -> &Path {
        &self.metadata
    }

    pub fn files(&self) -> &Path {
        &self.files
    }

    pub fn tags(&self) -> &Path {
        &self.tags
    }

    pub fn file<N>(&self, name: N) -> PathBuf
    where
        N: AsRef<NameRef>,
    {
        self.files.join(name.as_ref().as_path())
    }

    pub fn file_tags<N>(&self, name: N) -> PathBuf
    where
        N: AsRef<NameRef>,
    {
        self.tags.join(name.as_ref().as_path())
    }

    pub fn program_tags<N>(&self, name: N) -> PathBuf
    where
        N: AsRef<NameRef>,
    {
        self.file_tags(name).join(PROGRAM_TAGS_DIR)
    }

    pub fn tag<N, T>(&self, name: N, tag: T) -> PathBuf
    where
        N: AsRef<NameRef>,
        T: AsRef<TagRef>,
    {
        self.program_tags(name).join(tag.as_ref().as_path())
    }
}

impl AsRef<Path> for Root {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}
