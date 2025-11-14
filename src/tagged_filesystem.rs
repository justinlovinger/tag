mod find;

#[cfg(test)]
mod testing;

use std::path::{Path, PathBuf};

use itertools::Itertools;

use crate::{Root, Tag, TaggedPath, METADATA_DIR};

#[derive(Debug)]
pub struct TaggedFilesystemBuilder {
    root: PathBuf,
}

#[derive(Debug)]
pub struct TaggedFilesystem {
    root: Root,
}

impl TaggedFilesystemBuilder {
    #[allow(clippy::new_without_default)]
    pub fn new<P>(root: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self { root: root.into() }
    }

    pub fn build(self) -> std::io::Result<Option<TaggedFilesystem>> {
        TaggedFilesystem::new(self.root)
    }
}

impl TaggedFilesystem {
    #[allow(clippy::new_without_default)]
    pub fn new(root: PathBuf) -> std::io::Result<Option<TaggedFilesystem>> {
        if let Some(root) = Root::from_child(root)? {
            Ok(Some(Self { root }))
        } else {
            Ok(None)
        }
    }

    #[allow(dead_code)]
    fn tagged_paths(&self) -> impl Iterator<Item = TaggedPath> {
        let (sender, receiver) = crossbeam_channel::unbounded();
        self.for_each_tagged_path(move |path| sender.send(path).unwrap());
        receiver.into_iter()
    }

    fn filtered_tagged_paths(
        &self,
        predicate: impl Fn(&TaggedPath) -> bool + Send + Sync + 'static,
    ) -> impl Iterator<Item = TaggedPath> {
        let (sender, receiver) = crossbeam_channel::unbounded();
        self.for_each_tagged_path(move |path| {
            if predicate(&path) {
                sender.send(path).unwrap()
            }
        });
        receiver.into_iter()
    }

    fn for_each_tagged_path<C>(&self, callback: C)
    where
        C: Fn(TaggedPath) + Send + Sync + 'static,
    {
        let root = self.root.as_path().to_owned();
        rayon::spawn(move || {
            rayon::scope(|s| {
                Self::for_each_tagged_path_(
                    &root,
                    s,
                    &callback,
                    read_paths_relative(&root)
                        .unwrap()
                        .filter(|path| path != &PathBuf::from(METADATA_DIR)),
                )
            })
        });
    }

    fn for_each_tagged_path_<'scope, C>(
        root: &'scope Path,
        scope: &rayon::Scope<'scope>,
        callback: &'scope C,
        paths: impl Iterator<Item = PathBuf>,
    ) where
        C: Fn(TaggedPath) + Send + Sync,
    {
        for path in paths {
            scope.spawn(|scope| match TaggedPath::from_path(path) {
                Ok(path) => callback(path),
                Err(e) => {
                    let path = root.join(e.into_path());
                    if path.is_dir() {
                        Self::for_each_tagged_path_(
                            root,
                            scope,
                            callback,
                            read_paths_relative_to(path, root).unwrap(),
                        )
                    }
                }
            })
        }
    }
}

fn read_paths_relative(absolute: &Path) -> std::io::Result<impl Iterator<Item = PathBuf> + '_> {
    read_paths_relative_to(absolute.to_owned(), absolute)
}

fn read_paths_relative_to<'a, P>(
    dir: P,
    root: &'a Path,
) -> std::io::Result<impl Iterator<Item = PathBuf> + 'a>
where
    P: 'a + AsRef<Path>,
{
    Ok(read_paths(dir)?.map(move |path| {
        path.strip_prefix(root)
            .unwrap_or_else(|_| panic!("path should start with `{}`", root.to_string_lossy()))
            .to_owned()
    }))
}

fn read_paths<P>(dir: P) -> std::io::Result<impl Iterator<Item = PathBuf>>
where
    P: AsRef<Path>,
{
    Ok(std::fs::read_dir(dir)?.map(|res| res.unwrap().path()))
}
