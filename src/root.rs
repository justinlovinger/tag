use std::path::{Path, PathBuf};

use crate::{NameRef, TaggedPath, FILES_DIR, METADATA_DIR, TAGS_SCRIPT};

#[derive(Debug, Clone)]
pub struct Root {
    path: PathBuf,
    metadata: PathBuf,
    files: PathBuf,
    tags: PathBuf,
}

impl Root {
    pub fn new<P>(path: P) -> std::io::Result<Option<Self>>
    where
        P: Into<PathBuf>,
    {
        let path = path.into();

        let metadata = path.join(METADATA_DIR);
        let files = metadata.join(FILES_DIR);
        let tags = metadata.join(TAGS_SCRIPT);
        if metadata.is_dir() && files.is_dir() && tags.is_file() {
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

    pub fn from_child<P>(start: P) -> std::io::Result<Option<Self>>
    where
        P: AsRef<Path>,
    {
        for path in start.as_ref().ancestors() {
            if let Some(root) = Self::new(path)? {
                for path in start
                    .as_ref()
                    .ancestors()
                    .map_while(|path| path.strip_prefix(root.as_path()).ok())
                    .take_while(|path| path != &PathBuf::new().as_path())
                {
                    if path.parent().is_some_and(|parent| {
                        parent.ends_with(PathBuf::from(METADATA_DIR).join(FILES_DIR))
                    }) {
                        return Ok(None);
                    }
                    if TaggedPath::from_path(path.to_owned()).is_ok() {
                        return Ok(None);
                    }
                }

                return Ok(Some(root));
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
}

impl AsRef<Path> for Root {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}
