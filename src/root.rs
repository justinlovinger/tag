use std::path::{Path, PathBuf};

use crate::{TaggedPath, METADATA_DIR};

#[derive(Debug, Clone)]
pub struct Root {
    path: PathBuf,
    metadata: PathBuf,
}

impl Root {
    pub fn new<P>(path: P) -> std::io::Result<Option<Self>>
    where
        P: Into<PathBuf>,
    {
        let path = path.into();

        let metadata = path.join(METADATA_DIR);
        if metadata.is_dir() {
            Ok(Some(Self { path, metadata }))
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
                    if path == PathBuf::from(METADATA_DIR) {
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
}

impl AsRef<Path> for Root {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

#[cfg(test)]
mod tests {
    use std::fs::create_dir;

    use crate::testing::with_temp_dir;

    use super::*;

    #[test]
    fn new_returns_some_on_root() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            assert!(Root::new(dir).unwrap().is_some());
        })
    }

    #[test]
    fn new_returns_none_on_empty() {
        with_temp_dir(|dir| {
            assert!(Root::new(dir).unwrap().is_none());
        })
    }

    #[test]
    fn new_returns_none_on_child_of_root() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo")).unwrap();
            assert!(Root::new(dir.join("foo")).unwrap().is_none());
        })
    }

    #[test]
    fn from_child_returns_some_on_root() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            assert!(Root::from_child(dir).unwrap().is_some());
        })
    }

    #[test]
    fn from_child_returns_some_on_child_of_root() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo")).unwrap();
            assert!(Root::from_child(dir.join("foo")).unwrap().is_some());
        })
    }

    #[test]
    fn from_child_returns_none_on_tagged_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo.dir")).unwrap();
            assert!(Root::from_child(dir.join("foo.dir")).unwrap().is_none());
        })
    }

    #[test]
    fn from_child_returns_none_on_child_of_tagged_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo.dir")).unwrap();
            create_dir(dir.join("foo.dir").join("bar")).unwrap();
            assert!(Root::from_child(dir.join("foo.dir").join("bar"))
                .unwrap()
                .is_none());
        })
    }

    #[test]
    fn from_child_returns_some_on_root_in_tagged_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo.dir")).unwrap();
            create_dir(dir.join("foo.dir").join(METADATA_DIR)).unwrap();
            assert!(Root::from_child(dir.join("foo.dir")).unwrap().is_some());
        })
    }

    #[test]
    fn from_child_returns_some_on_child_of_root_in_tagged_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo.dir")).unwrap();
            create_dir(dir.join("foo.dir").join(METADATA_DIR)).unwrap();
            create_dir(dir.join("foo.dir").join("bar")).unwrap();
            assert!(Root::from_child(dir.join("foo.dir").join("bar"))
                .unwrap()
                .is_some());
        })
    }

    #[test]
    fn from_child_returns_none_on_metadata_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            assert!(Root::from_child(dir.join(METADATA_DIR)).unwrap().is_none());
        })
    }

    #[test]
    fn from_child_returns_none_on_child_of_metadata_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join(METADATA_DIR).join("foo")).unwrap();
            assert!(Root::from_child(dir.join(METADATA_DIR).join("foo"))
                .unwrap()
                .is_none());
        })
    }

    #[test]
    fn from_child_returns_none_on_root_in_metadata_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join(METADATA_DIR).join(METADATA_DIR)).unwrap();
            assert!(Root::from_child(dir.join(METADATA_DIR)).unwrap().is_some());
        })
    }

    #[test]
    fn from_child_returns_none_on_child_of_root_in_metadata_directory() {
        with_temp_dir(|dir| {
            create_dir(dir.join(METADATA_DIR)).unwrap();
            create_dir(dir.join(METADATA_DIR).join(METADATA_DIR)).unwrap();
            create_dir(dir.join(METADATA_DIR).join("foo")).unwrap();
            assert!(Root::from_child(dir.join(METADATA_DIR).join("foo"))
                .unwrap()
                .is_some());
        })
    }
}
