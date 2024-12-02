mod build;
mod errors;
mod find;
mod init;
mod r#mod;
mod new_file;
mod op;
mod rename;
mod rm;

use std::{
    collections::BTreeSet,
    env::current_dir,
    fs::{create_dir, create_dir_all, remove_dir, remove_dir_all, remove_file, rename, File},
    path::{Path, PathBuf},
    sync::Arc,
};

use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    fs::{copy_dir, symlink_dir, symlink_file},
    organize::organize,
    tagged_path::TaggedPathError,
    Name, NameRef, Root, Tag, TagRef, TaggedPath, FILES_DIR, METADATA_DIR, PROGRAM_TAGS_DIR,
    TAGS_DIR,
};

pub use self::errors::*;
pub(crate) use self::op::*;

#[derive(Debug)]
pub struct TaggedFilesystemBuilder {
    root: PathBuf,
    dry_run: bool,
    verbose: bool,
}

#[derive(Debug)]
pub struct TaggedFilesystem {
    root: Root,
    dry_run: bool,
    verbose: bool,
}

impl TaggedFilesystemBuilder {
    #[allow(clippy::new_without_default)]
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            dry_run: false,
            verbose: false,
        }
    }

    pub fn dry_run(mut self, value: bool) -> Self {
        self.dry_run = value;
        self
    }

    pub fn verbose(mut self, value: bool) -> Self {
        self.verbose = value;
        self
    }

    pub fn build(self) -> std::io::Result<Option<TaggedFilesystem>> {
        TaggedFilesystem::new(self.root, self.dry_run, self.verbose)
    }
}

impl TaggedFilesystem {
    #[allow(clippy::new_without_default)]
    pub fn new(
        root: PathBuf,
        dry_run: bool,
        verbose: bool,
    ) -> std::io::Result<Option<TaggedFilesystem>> {
        if let Some(root) = Root::from_child(root)? {
            Ok(Some(Self {
                root,
                dry_run,
                verbose,
            }))
        } else {
            Ok(None)
        }
    }

    fn tags<N>(&self, name: N) -> Result<FxHashSet<Tag>, TagsError>
    where
        N: AsRef<NameRef>,
    {
        let mut tags = FxHashSet::default();
        for namespace in read_paths(self.root.file_tags(name))? {
            for entry in std::fs::read_dir(namespace)? {
                let entry = entry?;
                match entry.file_name().into_string() {
                    Ok(s) => match Tag::new(s) {
                        Ok(tag) => {
                            tags.insert(tag);
                        }
                        Err(e) => return Err(TagFromPathError(e, entry.path()).into()),
                    },
                    Err(_) => {
                        return Err(StringFromPathError(entry.path()).into());
                    }
                }
            }
        }
        Ok(tags)
    }

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

fn unique_tags(tags: impl IntoIterator<Item = Tag>) -> Result<FxHashSet<Tag>, NonUniqueTagError> {
    let mut tag_set = FxHashSet::default();
    for tag in tags.into_iter() {
        if !tag_set.insert(tag) {
            return Err(NonUniqueTagError);
        }
    }
    Ok(tag_set)
}

fn relevant_paths(mut tags: FxHashSet<Tag>, mut paths: Vec<TaggedPath>) -> Vec<TaggedPath> {
    let mut relevant_paths = Vec::new();
    loop {
        let prev_tags_len = tags.len();
        let mut i = 0;
        while i < paths.len() {
            if paths[i].tags().any(|tag| tags.contains(tag)) {
                let path = paths.swap_remove(i);
                tags.extend(path.tags().map(|tag| tag.to_owned()));
                relevant_paths.push(path);
            } else {
                i += 1;
            }
        }
        if tags.len() == prev_tags_len {
            return relevant_paths;
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

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        testing::{tagged_filesystem_with, with_tempdir, TaggedPaths},
        Tag,
    };

    use super::*;

    #[proptest]
    fn rm_inverses_touch(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);

            let expected = list_files(&filesystem.root);

            filesystem
                .touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            filesystem.rm(path.name().to_owned()).unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[proptest]
    fn rm_inverses_mkdir(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);

            let expected = list_files(&filesystem.root);

            filesystem
                .mkdir(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            filesystem.rm(path.name().to_owned()).unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[proptest(cases = 20)]
    fn add_inverses_del(paths: TaggedPaths, path: TaggedPath, tag: Tag) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);
            filesystem
                .touch(
                    path.tags()
                        .filter(|path_tag| path_tag != &tag.as_ref())
                        .map(|x| x.to_owned())
                        .chain([tag.clone()]),
                    path.name().to_owned(),
                )
                .unwrap();
            let expected = list_files(&filesystem.root);

            let path = filesystem
                .del(
                    tag.clone(),
                    filesystem
                        .filtered_tagged_paths(move |x| x.name() == path.name())
                        .collect(),
                )
                .unwrap()
                .into_iter()
                .next()
                .unwrap();
            filesystem
                .add(
                    tag,
                    [TaggedPath::from_path(path).unwrap()].into_iter().collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[proptest(cases = 20)]
    fn del_inverses_add(paths: TaggedPaths, path: TaggedPath, tag: Tag) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);
            filesystem
                .touch(
                    path.tags()
                        .filter(|path_tag| path_tag != &tag.as_ref())
                        .map(|x| x.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            let expected = list_files(&filesystem.root);

            let path = filesystem
                .add(
                    tag.clone(),
                    filesystem
                        .filtered_tagged_paths(move |x| x.name() == path.name())
                        .collect(),
                )
                .unwrap()
                .into_iter()
                .next()
                .unwrap();
            filesystem
                .del(
                    tag,
                    [TaggedPath::from_path(path).unwrap()].into_iter().collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    pub fn list_tagged_paths<P>(path: P) -> Vec<PathBuf>
    where
        P: AsRef<Path>,
    {
        list_files_(
            path.as_ref(),
            read_paths(path.as_ref())
                .unwrap()
                .filter(|path| !path.ends_with(METADATA_DIR))
                .collect(),
        )
    }

    pub fn list_files<P>(path: P) -> Vec<PathBuf>
    where
        P: AsRef<Path>,
    {
        list_files_(path.as_ref(), read_paths(path.as_ref()).unwrap().collect())
    }

    fn list_files_(root: &Path, mut queue: Vec<PathBuf>) -> Vec<PathBuf> {
        let mut files = Vec::new();
        while let Some(file) = queue.pop() {
            if file.is_dir() && std::fs::read_dir(&file).unwrap().next().is_some() {
                queue.extend(read_paths(file).unwrap());
            } else {
                files.push(file);
            }
        }
        for file in &mut files {
            *file = file.strip_prefix(root).unwrap().to_owned();
        }
        files.sort();
        files
    }
}
