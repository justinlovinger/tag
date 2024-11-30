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
    tagged_path::NewError,
    types::MoveOp,
    Name, NameRef, Root, Tag, TagRef, TaggedPath, FILES_DIR, METADATA_DIR, PROGRAM_TAGS_DIR,
    TAGS_DIR,
};

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

#[derive(Clone, Debug, PartialEq)]
enum Op {
    EnsureDirectory(PathBuf),
    Move(MoveOp),
    DeleteDirectoryIfEmpty(PathBuf),
}

impl From<MoveOp> for Op {
    fn from(value: MoveOp) -> Self {
        Op::Move(value)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InitError {
    #[error(
        "The working directory already has a `{}` directory. It may already be the root of a tagging system. If not, please move the directory before initializing.",
        METADATA_DIR
    )]
    AlreadyTagged,
    #[error(
        "The working directory has a file named `{}`. Please move it before initializing.",
        METADATA_DIR
    )]
    FileExists,
    #[error(
        "The working directory is part of a tagged directory, `{}`.",
        METADATA_DIR
    )]
    InTagged(PathBuf),
    #[error("The working directory has files. Please move them before initializing.")]
    NotEmpty,
    #[error("{0}")]
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum NewFileError {
    FileExists(#[from] FileExistsError),
    MetadataExists(#[from] MetadataExistsError),
    NonUniqueTag(#[from] NonUniqueTagError),
    New(#[from] NewError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RmError {
    NoFile(#[from] NoFileError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RenameError {
    #[error("New and old name must be different.")]
    SameName,
    NoFile(#[from] NoFileError),
    FileExists(#[from] FileExistsError),
    NoMetadataExists(#[from] NoMetadataExistsError),
    MetadataExists(#[from] MetadataExistsError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("No file or directory with this name exists.")]
pub struct NoFileError;

#[derive(Debug, thiserror::Error)]
#[error("A file or directory with this name already exists.")]
pub struct FileExistsError;

#[derive(Debug, thiserror::Error)]
#[error("No metadata for this name exists. Please run `tag build`.")]
pub struct NoMetadataExistsError;

#[derive(Debug, thiserror::Error)]
#[error("Metadata for this name already exists. An error may have previously occured. Please move `{0}` or run `tag build`.")]
pub struct MetadataExistsError(PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum ModError {
    HasTag(#[from] HasTagError),
    LacksTag(#[from] LacksTagError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}`")]
pub struct HasTagError(TaggedPath, String);

impl HasTagError {
    fn new<T>(path: TaggedPath, tag: T) -> Self
    where
        T: AsRef<TagRef>,
    {
        Self(path, tag.as_ref().to_string())
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` lacks `{1}`")]
pub struct LacksTagError(TaggedPath, String);

impl LacksTagError {
    fn new<T>(path: TaggedPath, tag: T) -> Self
    where
        T: AsRef<TagRef>,
    {
        Self(path, tag.as_ref().to_string())
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum PathError {
    NonUniqueTag(#[from] NonUniqueTagError),
    New(#[from] NewError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("Tag is not unique")]
pub struct NonUniqueTagError;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum BuildError {
    Tags(#[from] TagsError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum TagsError {
    InvalidString(#[from] InvalidStringError),
    InvalidTag(#[from] InvalidTagError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("`{0}` is not a valid Unicode string")]
pub struct InvalidStringError(PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("`{0}` is not a valid tag")]
pub struct InvalidTagError(PathBuf);

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

    pub fn init() -> Result<Self, InitError> {
        if PathBuf::from(METADATA_DIR).is_dir() {
            Err(InitError::AlreadyTagged)
        } else if PathBuf::from(METADATA_DIR).is_file() {
            Err(InitError::FileExists)
        } else if let Some(root) = Root::from_child(current_dir()?)? {
            Err(InitError::InTagged(root.into_path()))
        } else if std::fs::read_dir(".")?.next().is_some() {
            Err(InitError::NotEmpty)
        } else {
            create_dir(METADATA_DIR)?;
            create_dir(PathBuf::from(METADATA_DIR).join(FILES_DIR))?;
            create_dir(PathBuf::from(METADATA_DIR).join(TAGS_DIR))?;
            Ok(TaggedFilesystemBuilder::new(current_dir()?)
                .build()?
                .unwrap())
        }
    }

    pub fn touch(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<PathBuf, NewFileError> {
        let (tagged_path, file_path, path_to_print) = self.new_file(tags, name)?;
        symlink_file(&file_path, tagged_path)?;
        File::create(&file_path)?;
        Ok(path_to_print)
    }

    pub fn mkdir(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<PathBuf, NewFileError> {
        let (tagged_path, file_path, path_to_print) = self.new_file(tags, name)?;
        crate::fs::symlink_dir(&file_path, tagged_path)?;
        create_dir(&file_path)?;
        Ok(path_to_print)
    }

    /// Return (tagged_path, file_path, path_to_print)
    fn new_file(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<(PathBuf, PathBuf, PathBuf), NewFileError> {
        let file_path = self.root.file(&name);
        if file_path.try_exists()? {
            return Err(FileExistsError.into());
        }

        let file_tags_path = self.root.file_tags(&name);
        if file_tags_path.try_exists()? {
            return Err(MetadataExistsError(file_tags_path).into());
        }

        let tag_set = unique_tags(tags)?;
        let tagged_path = TaggedPath::from_tags(tag_set.iter(), name)?;

        create_dir(&file_tags_path)?;
        let tags_path = file_tags_path.join(PROGRAM_TAGS_DIR);
        create_dir(&tags_path)?;
        for tag in tag_set.iter() {
            File::create(tags_path.join(tag.as_path()))?;
        }

        let mut paths = relevant_paths(tag_set, self.tagged_paths().collect());
        let tagged_path_buf = tagged_path.as_path().to_owned();
        paths.push(tagged_path);

        let ops = from_move_ops(organize(&paths));

        let mut to_path = None;
        self.apply_all(ops.filter(|op| match op {
            Op::EnsureDirectory(_) => true,
            Op::Move(MoveOp { from, to }) => {
                if from == &tagged_path_buf {
                    to_path = Some(to.clone());
                    false
                } else {
                    true
                }
            }
            Op::DeleteDirectoryIfEmpty(_) => true,
        }))?;

        let to_path = self.root.join(match to_path {
            Some(to_path) => to_path,
            None => tagged_path_buf,
        });
        let path_to_print = to_path
            .strip_prefix(current_dir()?)
            .map(|path| path.to_owned())
            .unwrap_or_else(|_| to_path.clone());
        Ok((to_path, file_path, path_to_print))
    }

    pub fn rm(&self, name: Name) -> Result<(), RmError> {
        let file_path = self.root.file(&name);

        if !file_path.try_exists()? {
            return Err(NoFileError.into());
        }
        if file_path.is_file() {
            remove_file(file_path)?;
        } else {
            remove_dir_all(file_path)?
        }

        remove_dir_all(self.root.file_tags(&name))?;

        let tagged_path = self
            .filtered_tagged_paths(move |path| path.name() == name.as_ref())
            .next()
            .expect("a tagged path for the file should exist");
        let tags = tagged_path.tags().map(|tag| tag.to_owned()).collect();
        remove_file(tagged_path)?;
        self.apply_all(from_move_ops(organize(&relevant_paths(
            tags,
            self.tagged_paths().collect(),
        ))))?;

        Ok(())
    }

    pub fn rename(&self, old_name: Name, new_name: Name) -> Result<PathBuf, RenameError> {
        if old_name == new_name {
            return Err(RenameError::SameName);
        }

        let old_file_path = self.root.file(&old_name);
        if !old_file_path.try_exists()? {
            return Err(NoFileError.into());
        }

        let new_file_path = self.root.file(&new_name);
        if new_file_path.try_exists()? {
            return Err(FileExistsError.into());
        }

        let old_file_tags_path = self.root.file_tags(&old_name);
        if !old_file_tags_path.try_exists()? {
            return Err(NoMetadataExistsError.into());
        }

        let new_file_tags_path = self.root.file_tags(&new_name);
        if new_file_tags_path.try_exists()? {
            return Err(MetadataExistsError(new_file_tags_path).into());
        }

        // Copying ensures no data is lost if the rename fails.
        copy_dir(&old_file_tags_path, new_file_tags_path)?;
        rename(old_file_path, &new_file_path)?;
        std::fs::remove_dir_all(old_file_tags_path)?;

        if let Some(old_tagged_path) = self
            .filtered_tagged_paths(move |path| path.name() == old_name.as_ref())
            .next()
        {
            let new_tagged_path =
                TaggedPath::from_tags(old_tagged_path.tags().map(|tag| tag.to_owned()), new_name)
                    .unwrap();
            remove_file(old_tagged_path.as_path())?;
            if new_file_path.is_dir() {
                symlink_dir(new_file_path, &new_tagged_path)?;
            } else {
                symlink_file(new_file_path, &new_tagged_path)?;
            }
            Ok(new_tagged_path.into_path())
        } else {
            Ok(PathBuf::new())
        }
    }

    pub fn add(&self, tag: Tag, paths: FxHashSet<TaggedPath>) -> Result<Vec<PathBuf>, ModError> {
        self.modify([tag].into_iter().collect(), FxHashSet::default(), paths)
    }

    pub fn del(&self, tag: Tag, paths: FxHashSet<TaggedPath>) -> Result<Vec<PathBuf>, ModError> {
        self.modify(FxHashSet::default(), [tag].into_iter().collect(), paths)
    }

    pub fn modify(
        &self,
        add: FxHashSet<Tag>,
        del: FxHashSet<Tag>,
        paths: FxHashSet<TaggedPath>,
    ) -> Result<Vec<PathBuf>, ModError> {
        if add.is_empty() && del.is_empty() {
            return Ok(vec![]);
        }

        for name in paths.iter().map(|path| path.name()) {
            for tag in &add {
                File::create(self.root.tag(name, tag))?;
            }
            for tag in &del {
                if let Err(e) = remove_file(self.root.tag(name, tag)) {
                    // The tag not existing is fine.
                    // The file not existing is _not_.
                    if !self.root.program_tags(name).try_exists()? {
                        Err(e)?;
                    }
                }
            }
        }

        let paths = Arc::new(paths);
        let mut relevant_paths = relevant_paths(
            paths
                .iter()
                .flat_map(|path| path.tags().map(|tag| tag.to_owned()))
                .chain(add.iter().cloned())
                .collect::<FxHashSet<_>>(),
            self.filtered_tagged_paths({
                let paths = Arc::clone(&paths);
                move |path| !paths.contains(path)
            })
            .collect(),
        );
        let paths = Arc::try_unwrap(paths).unwrap_or_else(|arc| (*arc).clone());

        let mut move_ops = Vec::new();
        for path in paths {
            for tag in path.tags() {
                if add.contains(tag) {
                    return Err(HasTagError::new(path.clone(), tag).into());
                }
            }

            let mut deleted_count = 0;
            // This method is safe
            // because `tags` were already checked for duplicates,
            // and `name` is from a valid `TaggedPath`.
            let new_path = unsafe {
                TaggedPath::from_tags_unchecked(
                    path.tags()
                        .filter(|tag| {
                            if del.contains(*tag) {
                                deleted_count += 1;
                                false
                            } else {
                                true
                            }
                        })
                        .chain(add.iter().map(|tag| tag.as_ref())),
                    path.name(),
                )
            };
            if deleted_count != del.len() {
                let mut del = del;
                for tag in path.tags() {
                    del.remove(tag);
                }
                return Err(LacksTagError::new(path, del.into_iter().next().unwrap()).into());
            }

            move_ops.push(MoveOp {
                from: path.into_path(),
                to: new_path.as_path().to_owned(),
            });
            relevant_paths.push(new_path);
        }

        let mut organized_move_ops = organize(&relevant_paths);
        let mut new_paths = Vec::new();
        for op in move_ops {
            match organized_move_ops
                .iter_mut()
                .find(|other| other.from == op.to)
            {
                Some(other) => {
                    other.from = op.from;
                    new_paths.push(other.to.clone());
                }
                None => {
                    let to = op.to.clone();
                    organized_move_ops.push(op);
                    new_paths.push(to);
                }
            }
        }

        self.apply_all(from_move_ops(organized_move_ops))?;
        Ok(new_paths)
    }

    pub fn build(&self) -> Result<(), BuildError> {
        let names = std::fs::read_dir(self.root.files())?
            .map(|entry| {
                entry.map(|entry| {
                    Name::new(
                        entry
                            .file_name()
                            .into_string()
                            .expect("file name should be a valid Unicode string"),
                    )
                    .expect("file name should be a valid name")
                })
            })
            .collect::<std::io::Result<FxHashSet<_>>>()?;

        let tag_directories = {
            let mut tag_directories = [].into_iter().collect::<FxHashSet<_>>();
            for name in std::fs::read_dir(self.root.tags())?.map(|entry| {
                entry.map(|entry| {
                    Name::new(
                        entry
                            .file_name()
                            .into_string()
                            .expect("tag-directory should be a valid Unicode string"),
                    )
                    .expect("tag-directory should be a valid name")
                })
            }) {
                let name = name?;
                if !names.contains(&name) {
                    remove_dir_all(self.root.file_tags(name))?;
                } else {
                    tag_directories.insert(name);
                }
            }
            tag_directories
        };

        for name in &names {
            if !tag_directories.contains(name) {
                create_dir(self.root.file_tags(name))?;
                create_dir(self.root.program_tags(name))?;
            } else {
                let program_tags = self.root.program_tags(name);
                if !program_tags.try_exists()? {
                    create_dir(program_tags)?;
                }
            }
        }

        let (mut tagged_paths, path_names, del_move_ops) = {
            let mut tagged_paths = self.tagged_paths().collect_vec();
            let mut path_names = FxHashSet::default();
            let mut removed = Vec::new();
            for (i, path) in tagged_paths.iter().enumerate() {
                if !names.contains(path.name()) || !path_names.insert(path.name().to_owned()) {
                    remove_file(path)?;
                    removed.push(i);
                }
            }
            // We can use fake move-ops to generate `DeleteDirectoryIfEmpty` ops.
            // A real move-op will never have an empty `to`.
            let fake_move_ops = removed
                .into_iter()
                .rev()
                .map(|i| {
                    let path = tagged_paths.swap_remove(i);
                    MoveOp {
                        from: path.into_path(),
                        to: PathBuf::new(),
                    }
                })
                .collect_vec();
            (tagged_paths, path_names, fake_move_ops)
        };

        let new_paths = {
            let mut new_paths = Vec::new();
            for name in &names {
                if !path_names.contains(name) {
                    new_paths.push(TaggedPath::from_tags(self.tags(name)?, name).unwrap());
                }
            }
            new_paths
        };

        let mut modified_paths_to_from = {
            let mut modified_paths_to_from = FxHashMap::default();
            for path in &mut tagged_paths {
                let tags = self.tags(path.name())?;
                let path_tags = path.tags().map(|tag| tag.to_owned()).collect();
                if tags != path_tags {
                    let original_path = path.as_path().to_path_buf();
                    *path = TaggedPath::from_tags(tags, path.name()).unwrap();
                    modified_paths_to_from.insert(path.as_path().to_path_buf(), original_path);
                }
            }
            modified_paths_to_from
        };

        tagged_paths.extend(new_paths.iter().cloned());
        let mut move_ops = organize(&tagged_paths);

        for move_op in &mut move_ops {
            if let Some(from) = modified_paths_to_from.remove(&move_op.from) {
                move_op.from = from;
            }
        }
        move_ops.extend(
            modified_paths_to_from
                .into_iter()
                .map(|(to, from)| MoveOp { from, to }),
        );

        let ops = from_move_ops(move_ops.into_iter().chain(del_move_ops).collect())
            // Move-ops with no target are fake,
            // and should be removed.
            .filter(|op| match op {
                Op::EnsureDirectory(_) => true,
                Op::Move(MoveOp { from: _, to }) => to != &PathBuf::new(),
                Op::DeleteDirectoryIfEmpty(_) => true,
            });

        // New paths cannot be moved,
        // we will create them directly instead.
        let (ops, new_paths) = {
            let mut unmoved_new_paths = new_paths
                .into_iter()
                .map(|path| path.into_path())
                .collect::<FxHashSet<_>>();
            let mut new_paths = Vec::new();
            let ops = ops
                .filter(|op| match op {
                    Op::EnsureDirectory(_) => true,
                    Op::Move(MoveOp { from, to }) => {
                        if unmoved_new_paths.remove(from) {
                            new_paths.push(to.clone());
                            false
                        } else {
                            true
                        }
                    }
                    Op::DeleteDirectoryIfEmpty(_) => true,
                })
                .collect_vec();
            new_paths.extend(unmoved_new_paths);
            (ops, new_paths)
        };

        self.apply_all(ops)?;

        for path in new_paths
            .into_iter()
            .map(|path| TaggedPath::from_path(path).unwrap())
        {
            let file_path = self.root.file(path.name());
            if file_path.is_dir() {
                symlink_dir(file_path, path)?;
            } else {
                symlink_file(file_path, path)?;
            }
        }

        Ok(())
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
                    Ok(s) => {
                        if let Some(tag) = Tag::new(s) {
                            tags.insert(tag);
                        } else {
                            return Err(InvalidTagError(entry.path()).into());
                        }
                    }
                    Err(_) => {
                        return Err(InvalidStringError(entry.path()).into());
                    }
                }
            }
        }
        Ok(tags)
    }

    pub fn find(
        &self,
        include: Vec<Tag>,
        exclude: Vec<Tag>,
    ) -> std::io::Result<impl Iterator<Item = TaggedPath>> {
        Ok(self.filtered_tagged_paths(move |path| {
            include
                .iter()
                .all(|tag| path.tags().contains(&tag.as_ref()))
                && !exclude
                    .iter()
                    .any(|tag| path.tags().contains(&tag.as_ref()))
        }))
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

    fn apply_all(&self, ops: impl IntoIterator<Item = Op>) -> std::io::Result<()> {
        let ops = self.canonicalize_all(ops).collect_vec();

        if let Err(e) = self.apply_all_(&ops) {
            if self.verbose {
                println!("Error occured: {e}");
                println!("Cleaning up");
            }
            ops.into_iter()
                .filter_map(|op| match op {
                    Op::EnsureDirectory(path) => Some(Op::DeleteDirectoryIfEmpty(path)),
                    Op::Move(MoveOp { .. }) => None,
                    Op::DeleteDirectoryIfEmpty(path) => Some(Op::DeleteDirectoryIfEmpty(path)),
                })
                .for_each(|op| {
                    if let Err(e) = self.apply_(&op) {
                        eprintln!("{e}");
                    }
                });
            Err(e)
        } else {
            Ok(())
        }
    }

    fn canonicalize_all<'a>(
        &'a self,
        ops: impl IntoIterator<Item = Op> + 'a,
    ) -> impl Iterator<Item = Op> + 'a {
        ops.into_iter().map(|op| self.canonicalize(op))
    }

    fn canonicalize(&self, op: Op) -> Op {
        match op {
            Op::EnsureDirectory(path) => Op::EnsureDirectory(self.root.join(path)),
            Op::Move(MoveOp { from, to }) => Op::Move(MoveOp {
                from: self.root.join(from),
                to: self.root.join(to),
            }),
            Op::DeleteDirectoryIfEmpty(path) => Op::DeleteDirectoryIfEmpty(self.root.join(path)),
        }
    }

    fn apply_all_<'a>(&self, ops: impl IntoIterator<Item = &'a Op>) -> std::io::Result<()> {
        for op in ops.into_iter() {
            self.apply_(op)?
        }
        Ok(())
    }

    fn apply_(&self, op: &Op) -> std::io::Result<()> {
        if self.verbose {
            match op {
                Op::EnsureDirectory(path) => {
                    println!("Ensuring directory `{}` exists", path.display());
                }
                Op::Move(MoveOp { from, to }) => {
                    println!("Moving `{}` to `{}`", from.display(), to.display());
                }
                Op::DeleteDirectoryIfEmpty(path) => {
                    println!("Deleting directory `{}` if empty", path.display());
                }
            }
        }

        if !self.dry_run {
            match op {
                Op::EnsureDirectory(path) => create_dir_all(path),
                Op::Move(MoveOp { from, to }) => {
                    // This utility should only organize data,
                    // never delete it.
                    if to.is_file() {
                        Err(std::io::Error::new(
                            std::io::ErrorKind::AlreadyExists,
                            format!(
                                "cannot move `{}` to `{}`, destination already exists",
                                from.display(),
                                to.display()
                            ),
                        ))
                    } else {
                        rename(from, to)
                    }
                }
                Op::DeleteDirectoryIfEmpty(path) => {
                    // Note,
                    // we can do the following with nightly Rust,
                    // which may be more efficient than `self.fs.read_dir(&path)?.next().is_none()`:
                    // ```
                    // if let Err(e) = self.fs.remove_dir(path) {
                    //     if e.kind() != std::io::ErrorKind::DirectoryNotEmpty {
                    //         return Err(e);
                    //     }
                    // }
                    // ```
                    if std::fs::read_dir(path)?.next().is_none() {
                        remove_dir(path)
                    } else {
                        Ok(())
                    }
                }
            }
        } else {
            Ok(())
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

fn from_move_ops(ops: Vec<MoveOp>) -> impl Iterator<Item = Op> {
    // We know a directory exists
    // if a file is moving from it.
    let from_ancestors = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from, to: _ }| from.ancestors().skip(1)),
    );
    let ensure_dirs = BTreeSet::from_iter(
        ops.iter()
            .filter_map(|MoveOp { from: _, to }| to.parent())
            .filter(|to| !from_ancestors.contains(to)),
    );
    // We do not need to ensure a parent exists
    // if its child is already being ensured.
    let ensure_dir_ancestors =
        BTreeSet::from_iter(ensure_dirs.iter().flat_map(|path| path.ancestors().skip(1)));
    let ensure_dirs = BTreeSet::from_iter(
        ensure_dirs
            .into_iter()
            .filter(|to| !ensure_dir_ancestors.contains(to))
            .map(|x| x.to_owned()),
    );

    // We know a directory will not be empty
    // if a file is moving to it.
    let to_ancestors = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from: _, to }| to.ancestors().skip(1)),
    );
    let del_dirs = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from, to: _ }| {
                // If a child will not be empty,
                // neither will its parents.
                from.ancestors()
                    .skip(1)
                    .take_while(|from| !to_ancestors.contains(from))
            })
            .map(|x| x.to_owned()),
    );

    ensure_dirs
        .into_iter()
        .map(Op::EnsureDirectory)
        .chain(ops.into_iter().map_into())
        // `rev()` to check child directories before parents.
        .chain(del_dirs.into_iter().rev().map(Op::DeleteDirectoryIfEmpty))
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
    use std::{
        env::set_current_dir,
        fs::{read_link, File},
        io::Read,
    };

    use proptest::prelude::{prop::collection::vec, *};
    use test_strategy::proptest;

    use crate::{
        testing::{
            make_file_and_parent, tagged_filesystem, tagged_filesystem_with, with_tempdir,
            TaggedPaths,
        },
        Tag,
    };

    use super::*;

    #[test]
    fn init_initializes_an_empty_directory() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_initializes_a_nested_tagged_filesystem_from_files_dir() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            filesystem
                .mkdir(
                    [Tag::new("foo".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap(),
                )
                .unwrap();
            set_current_dir(".tag/files/bar").unwrap();
            assert!(TaggedFilesystem::init().is_ok());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar/.tag/files",
                    ".tag/files/bar/.tag/tags",
                    ".tag/tags/bar/tag/foo",
                    "foo-_bar/.tag/files",
                    "foo-_bar/.tag/tags",
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn init_initializes_a_nested_tagged_filesystem_from_tagged_path() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            filesystem
                .mkdir(
                    [Tag::new("foo".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap(),
                )
                .unwrap();
            set_current_dir("foo-_bar").unwrap();
            assert!(TaggedFilesystem::init().is_ok());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar/.tag/files",
                    ".tag/files/bar/.tag/tags",
                    ".tag/tags/bar/tag/foo",
                    "foo-_bar/.tag/files",
                    "foo-_bar/.tag/tags",
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn init_errors_on_already_initialized_directory() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_directory_with_dot_tag_file() {
        with_tempdir(|| {
            File::create(METADATA_DIR).unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(current_dir().unwrap()),
                [".tag"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_sub_directory_of_tagged_filesystem() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with([
                TaggedPath::new("foo/_bar".to_owned()).unwrap(),
                TaggedPath::new("foo/_baz".to_owned()).unwrap(),
            ]);
            set_current_dir("foo").unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    "foo/_bar",
                    "foo/_baz"
                ]
                .map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_directory_with_files() {
        with_tempdir(|| {
            File::create("foo").unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(current_dir().unwrap()),
                ["foo"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_creates_an_empty_file_with_metadata_and_a_link_if_name_is_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            let name = Name::new("bar".to_owned()).unwrap();
            let tag = Tag::new("foo".to_owned()).unwrap();
            assert_eq!(
                filesystem.touch([tag], name.clone()).unwrap(),
                PathBuf::from("foo-_bar")
            );
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
            assert_eq!(
                File::open(filesystem.root.file(&name))
                    .unwrap()
                    .bytes()
                    .map(|x| x.unwrap())
                    .collect_vec(),
                "".as_bytes()
            );
            assert_eq!(
                read_link(filesystem.root.join("foo-_bar")).unwrap(),
                filesystem.root.file(name)
            );
        })
    }

    #[test]
    fn touch_works_in_subdir_of_root() {
        with_tempdir(|| {
            let _ = tagged_filesystem_with(["foo-_bar", "foo-_baz"]);

            set_current_dir("foo").unwrap();
            let filesystem = TaggedFilesystemBuilder::new(current_dir().unwrap())
                .build()
                .unwrap()
                .unwrap();

            let name = Name::new("biz".to_owned()).unwrap();
            let tag = Tag::new("foo".to_owned()).unwrap();
            assert_eq!(
                filesystem.touch([tag], name.clone()).unwrap(),
                PathBuf::from("_biz")
            );
            assert_eq!(
                list_files(&filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/files/biz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    ".tag/tags/biz/tag/foo",
                    "foo/_bar",
                    "foo/_baz",
                    "foo/_biz",
                ]
                .map(PathBuf::from)
            );
            assert_eq!(
                File::open(filesystem.root.file(&name))
                    .unwrap()
                    .bytes()
                    .map(|x| x.unwrap())
                    .collect_vec(),
                "".as_bytes()
            );
            assert_eq!(
                read_link(filesystem.root.join("foo/_biz")).unwrap(),
                filesystem.root.file(name)
            );
        })
    }

    #[test]
    fn touch_errors_if_file_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert!(filesystem
                .touch(
                    [Tag::new("baz".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_dir_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert!(filesystem
                .touch(
                    [Tag::new("baz".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_metadata_exists_for_file() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            let name = Name::new("bar".to_owned()).unwrap();
            create_dir(filesystem.root.file_tags(&name)).unwrap();
            assert!(filesystem.touch([], name).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags/bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_tags_are_not_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .touch(
                    [
                        Tag::new("foo".to_owned()).unwrap(),
                        Tag::new("foo".to_owned()).unwrap()
                    ],
                    Name::new("bar".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_creates_an_empty_directory_with_metadata_and_a_link_if_name_is_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            let name = Name::new("bar".to_owned()).unwrap();
            let tag = Tag::new("foo".to_owned()).unwrap();
            assert_eq!(
                filesystem.mkdir([tag], name.clone()).unwrap(),
                PathBuf::from("foo-_bar")
            );
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
            assert!(filesystem.root.file(&name).is_dir());
            assert!(std::fs::read_dir(filesystem.root.file(&name))
                .unwrap()
                .next()
                .is_none());
            assert_eq!(
                read_link(filesystem.root.join("foo-_bar")).unwrap(),
                filesystem.root.file(name)
            );
        })
    }

    #[test]
    fn mkdir_works_in_subdir_of_root() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            filesystem
                .mkdir(
                    [Tag::new("foo".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap(),
                )
                .unwrap();
            filesystem
                .mkdir(
                    [Tag::new("foo".to_owned()).unwrap()],
                    Name::new("baz".to_owned()).unwrap(),
                )
                .unwrap();

            set_current_dir("foo").unwrap();
            let filesystem = TaggedFilesystemBuilder::new(current_dir().unwrap())
                .build()
                .unwrap()
                .unwrap();

            let name = Name::new("biz".to_owned()).unwrap();
            let tag = Tag::new("foo".to_owned()).unwrap();
            assert_eq!(
                filesystem.mkdir([tag], name.clone()).unwrap(),
                PathBuf::from("_biz")
            );
            assert_eq!(
                list_files(&filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/files/biz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    ".tag/tags/biz/tag/foo",
                    "foo/_bar",
                    "foo/_baz",
                    "foo/_biz",
                ]
                .map(PathBuf::from)
            );
            assert!(filesystem.root.file(&name).is_dir());
            assert!(std::fs::read_dir(filesystem.root.file(&name))
                .unwrap()
                .next()
                .is_none());
            assert_eq!(
                read_link(filesystem.root.join("foo/_biz")).unwrap(),
                filesystem.root.file(name)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_file_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert!(filesystem
                .mkdir(
                    [Tag::new("baz".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_dir_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            filesystem
                .mkdir(
                    [Tag::new("foo".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap(),
                )
                .unwrap();
            assert!(filesystem
                .mkdir(
                    [Tag::new("baz".to_owned()).unwrap()],
                    Name::new("bar".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_metadata_exists_for_file() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            create_dir(PathBuf::from(METADATA_DIR).join(TAGS_DIR).join("bar")).unwrap();
            assert!(filesystem
                .mkdir([], Name::new("bar".to_owned()).unwrap())
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags/bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_tags_are_not_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .mkdir(
                    [
                        Tag::new("foo".to_owned()).unwrap(),
                        Tag::new("foo".to_owned()).unwrap()
                    ],
                    Name::new("bar".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn rm_removes_file_if_it_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["_foo"]);
            assert!(filesystem.rm(Name::new("foo".to_owned()).unwrap()).is_ok());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        });
    }

    #[test]
    fn rm_removes_dir_if_it_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            let name = Name::new("foo".to_owned()).unwrap();
            filesystem.mkdir([], name.clone()).unwrap();

            assert!(filesystem.rm(name).is_ok());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        });
    }

    #[test]
    fn rm_errors_on_missing_file() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem.rm(Name::new("foo".to_owned()).unwrap()).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        });
    }

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

    #[test]
    fn rename_renames_file_and_tagged_path_if_file_exists_and_no_file_with_new_name() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert_eq!(
                filesystem
                    .rename(
                        Name::new("bar".to_owned()).unwrap(),
                        Name::new("baz".to_owned()).unwrap()
                    )
                    .unwrap(),
                PathBuf::from("foo-_baz")
            );
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/baz", ".tag/tags/baz/tag/foo", "foo-_baz"].map(PathBuf::from)
            );
            assert_eq!(
                read_link(filesystem.root.join("foo-_baz")).unwrap(),
                filesystem.root.file(Name::new("baz".to_owned()).unwrap())
            );
        })
    }

    #[proptest]
    fn rename_errors_if_names_are_same(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain([&path]));

            let expected = list_files(&filesystem.root);

            assert!(filesystem
                .rename(path.name().to_owned(), path.name().to_owned())
                .is_err());
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn rename_errors_if_file_does_not_exist() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .rename(
                    Name::new("bar".to_owned()).unwrap(),
                    Name::new("baz".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn rename_errors_if_file_exists_with_new_name() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["fiz-_bar", "foo-_baz"]);
            assert!(filesystem
                .rename(
                    Name::new("bar".to_owned()).unwrap(),
                    Name::new("baz".to_owned()).unwrap()
                )
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/tags/bar/tag/fiz",
                    ".tag/tags/baz/tag/foo",
                    "fiz-_bar",
                    "foo-_baz"
                ]
                .map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mod_adds_a_tag() {
        with_tempdir(|| {
            let path = TaggedPath::new("foo-_baz".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            filesystem
                .modify(
                    [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                    [].into_iter().collect(),
                    [path].into_iter().collect(),
                )
                .unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/baz",
                    ".tag/tags/baz/tag/bar",
                    ".tag/tags/baz/tag/foo",
                    "bar-foo-_baz"
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn mod_deletes_a_tag() {
        with_tempdir(|| {
            let path = TaggedPath::new("foo-_baz".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            filesystem
                .modify(
                    [].into_iter().collect(),
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    [path].into_iter().collect(),
                )
                .unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files/baz", ".tag/tags/baz/tag", "_baz"].map(PathBuf::from)
            );
        });
    }

    #[test]
    fn mod_adds_a_tag_and_deletes_a_tag() {
        with_tempdir(|| {
            let path = TaggedPath::new("foo-_baz".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            filesystem
                .modify(
                    [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    [path].into_iter().collect(),
                )
                .unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files/baz", ".tag/tags/baz/tag/bar", "bar-_baz"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mod_modifies_all_files() {
        with_tempdir(|| {
            let paths = ["foo/_bar", "foo/_foo"];
            let filesystem = tagged_filesystem_with(paths);
            filesystem
                .modify(
                    [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    paths
                        .into_iter()
                        .map(|path| TaggedPath::new(path.to_owned()).unwrap())
                        .collect(),
                )
                .unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/foo",
                    ".tag/tags/bar/tag/bar",
                    ".tag/tags/foo/tag/bar",
                    "bar/_bar",
                    "bar/_foo"
                ]
                .map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mod_adds_multiple_tags() {
        with_tempdir(|| {
            let path = TaggedPath::new("foo-_baz".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            filesystem
                .modify(
                    [
                        Tag::new("bar".to_owned()).unwrap(),
                        Tag::new("baz".to_owned()).unwrap(),
                    ]
                    .into_iter()
                    .collect(),
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    [path].into_iter().collect(),
                )
                .unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/baz",
                    ".tag/tags/baz/tag/bar",
                    ".tag/tags/baz/tag/baz",
                    "bar-baz-_baz"
                ]
                .map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mod_deletes_multiple_tags() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-baz-_baz"]);
            filesystem
                .modify(
                    [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                    [
                        Tag::new("foo".to_owned()).unwrap(),
                        Tag::new("baz".to_owned()).unwrap(),
                    ]
                    .into_iter()
                    .collect(),
                    filesystem
                        .find(
                            vec![
                                Tag::new("foo".to_owned()).unwrap(),
                                Tag::new("baz".to_owned()).unwrap(),
                            ],
                            vec![],
                        )
                        .unwrap()
                        .collect(),
                )
                .unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files/baz", ".tag/tags/baz/tag/bar", "bar-_baz"].map(PathBuf::from)
            );
        })
    }

    #[proptest(cases = 20)]
    fn mod_changes_nothing_if_no_tags_given(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.into_iter().chain([path.clone()]));

            let expected = list_files(&filesystem.root);

            filesystem
                .modify(
                    [].into_iter().collect(),
                    [].into_iter().collect(),
                    filesystem
                        .filtered_tagged_paths(move |x| x.name() == path.name())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[proptest(cases = 20)]
    fn mod_builds(paths: TaggedPaths, path: TaggedPath, tag_to_add: Tag, tag_to_del: Tag) {
        prop_assume!(tag_to_add != tag_to_del);

        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);

            filesystem
                .touch(
                    path.tags()
                        .filter(|path_tag| path_tag != &tag_to_add.as_ref())
                        .filter(|path_tag| path_tag != &tag_to_del.as_ref())
                        .map(|x| x.to_owned())
                        .chain([tag_to_del.clone()]),
                    path.name().to_owned(),
                )
                .unwrap();
            filesystem
                .modify(
                    [tag_to_add].into_iter().collect(),
                    [tag_to_del].into_iter().collect(),
                    filesystem
                        .filtered_tagged_paths(move |x| x.name() == path.name())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let expected = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn mod_returns_error_if_file_already_has_tag() {
        with_tempdir(|| {
            let path = TaggedPath::new("foo-_bar".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            assert!(filesystem
                .modify(
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    [].into_iter().collect(),
                    [path].into_iter().collect()
                )
                .is_err());
        })
    }

    #[test]
    fn mod_returns_error_if_file_lacks_tag() {
        with_tempdir(|| {
            let path = TaggedPath::new("_bar".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            assert!(filesystem
                .modify(
                    [].into_iter().collect(),
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    [path].into_iter().collect()
                )
                .is_err());
        })
    }

    #[test]
    fn mod_returns_error_if_file_does_not_exist() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .modify(
                    [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                    [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                    [TaggedPath::new("foo-_baz".to_owned()).unwrap()]
                        .into_iter()
                        .collect()
                )
                .is_err());
        })
    }

    #[proptest(cases = 20)]
    fn build_removes_tags_for_missing_files(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);
            let expected = list_files(&filesystem.root);

            for tag in path.tags() {
                make_file_and_parent(filesystem.root.tag(path.name(), tag));
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_a_tags_directory_for_files_without(paths: TaggedPaths, name: Name) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);
            filesystem.touch([], name.clone()).unwrap();
            let expected = list_files(&filesystem.root);

            remove_dir_all(filesystem.root.file_tags(name)).unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_a_program_tags_directory_for_files_without(paths: TaggedPaths, name: Name) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);
            filesystem.touch([], name.clone()).unwrap();
            let expected = list_files(&filesystem.root);

            remove_dir(filesystem.root.program_tags(name)).unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_removes_tagged_paths_for_missing_files(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);

            let expected = list_files(&filesystem.root);

            filesystem
                .touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            remove_file(filesystem.root.file(path.name())).unwrap();
            remove_dir_all(filesystem.root.file_tags(path.name())).unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_tagged_paths_for_files_without(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain([&path]));

            let expected = list_files(&filesystem.root);

            remove_file(
                filesystem.root.join(
                    filesystem
                        .filtered_tagged_paths(move |x| x.name() == path.name())
                        .next()
                        .unwrap(),
                ),
            )
            .unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_deletes_extra_tags_from_tagged_paths(paths: TaggedPaths, path: TaggedPath, tag: Tag) {
        let path = TaggedPath::from_tags(
            path.tags().filter(|path_tag| path_tag != &tag.as_ref()),
            path.name(),
        )
        .unwrap();

        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain([&path]));

            let expected = list_files(&filesystem.root);

            filesystem.rm(path.name().to_owned()).unwrap();
            filesystem
                .touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    path.name().to_owned(),
                )
                .unwrap();
            remove_file(filesystem.root.tag(path.name(), tag)).unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_missing_tags_to_tagged_paths(paths: TaggedPaths, path: TaggedPath, tag: Tag) {
        let path = TaggedPath::from_tags(
            path.tags().filter(|path_tag| path_tag != &tag.as_ref()),
            path.name(),
        )
        .unwrap();

        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);
            filesystem
                .touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    path.name().to_owned(),
                )
                .unwrap();

            let expected = list_files(&filesystem.root);

            filesystem.rm(path.name().to_owned()).unwrap();
            filesystem
                .touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            File::create(filesystem.root.tag(path.name(), tag)).unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_deletes_duplicate_tagged_paths(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain([&path]));
            let expected = list_files(&filesystem.root);

            symlink_file(
                filesystem.root.file(path.name()),
                filesystem
                    .root
                    .as_path()
                    .join(format!("foobar-_{}", path.name())),
            )
            .unwrap();
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[test]
    fn build_organizes_paths_into_optimal_tag_directories() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with([
                "a/b/c/_foo",
                "a-b-_bar",
                "d/e-_baz",
                "🙂/🙁/_fez",
                "_fiz",
            ]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["_fiz", "a-b/_bar", "a-b/c-_foo", "d-e-_baz", "🙁-🙂-_fez"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_organizes_paths_into_optimal_tag_directories_when_all_have_same_tags() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["a-b-c-_bar", "a/b/c/_foo"]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a-b-c/_bar", "a-b-c/_foo"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_breaks_ties_in_favor_of_increasing_length() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["a/bb/_1", "bb/_2", "a/_3", "dd-ccc-_4"]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a-_3", "bb/_2", "bb/a-_1", "ccc-dd-_4"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_ignores_untagged_paths() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["a-_foo"]);
            create_dir(filesystem.root.join("a")).unwrap();
            rename(
                filesystem.root.join("a-_foo"),
                filesystem.root.join("a/_foo"),
            )
            .unwrap();
            File::create(filesystem.root.join("a/not-tagged")).unwrap();
            filesystem.build().unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/foo",
                    ".tag/tags/foo/tag/a",
                    "a/not-tagged",
                    "a-_foo"
                ]
                .map(PathBuf::from),
            )
        })
    }

    #[proptest(cases = 20)]
    fn build_ignores_nested_tagged_paths(
        #[strategy(
            TaggedPaths::arbitrary()
                .prop_flat_map(|paths| (
                    vec(TaggedPaths::arbitrary(), paths.len()),
                    Just(paths)
                ))
                .prop_map(|(x, y)| (y, x))
        )]
        args: (TaggedPaths, Vec<TaggedPaths>),
    ) {
        let (dirs, dirs_paths) = args;

        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem();
            for dir in dirs.0.iter() {
                filesystem
                    .mkdir(dir.tags().map(|tag| tag.to_owned()), dir.name().to_owned())
                    .unwrap();
            }
            for (dir, paths) in filesystem
                .find(Vec::new(), Vec::new())
                .unwrap()
                .zip(dirs_paths)
            {
                set_current_dir(filesystem.root.file(dir.name())).unwrap();
                let filesystem = TaggedFilesystem::init().unwrap();
                for path in paths {
                    filesystem
                        .touch(
                            path.tags().map(|tag| tag.to_owned()),
                            path.name().to_owned(),
                        )
                        .unwrap();
                }
            }
            let expected = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let actual = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[proptest(cases = 20)]
    fn build_is_idempotent(paths: TaggedPaths) {
        let (first_pass, second_pass) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);

            filesystem.build().unwrap();
            let first_pass = list_files(&filesystem.root);

            filesystem.build().unwrap();
            (list_files(filesystem.root), first_pass)
        });

        prop_assert_eq!(second_pass, first_pass);
    }

    #[proptest(cases = 20)]
    fn build_ignores_hidden_paths(paths: TaggedPaths, hidden_paths: TaggedPaths) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths);

            for path in hidden_paths {
                make_file_and_parent(format!(".{path}"));
            }
            let expected = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[test]
    #[ignore] // This test is slow.
    fn build_does_not_panic_on_many_non_unique_tags() {
        with_tempdir(|| {
            // `organize` is prone to stack-overflow
            // if recursion is not handled carefully.
            // This tests we support at least `BREADTH * DEPTH` non-unique tags.
            let filesystem = tagged_filesystem();
            const BREADTH: usize = 200;
            const DEPTH: usize = 50;
            let paths = (0..BREADTH)
                .flat_map(|n| {
                    let x = n * DEPTH;
                    let prefix = PathBuf::from((x..x + DEPTH).map(|x| format!("{x:04}")).join("-"));
                    [prefix.join("_bar"), prefix.join("_foo")]
                })
                .collect_vec();
            for path in &paths {
                make_file_and_parent(path);
            }

            filesystem.build().unwrap();
            assert_eq!(list_files(filesystem.root), paths)
        })
    }

    #[test]
    fn build_uses_directories_if_filename_gets_too_long() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);

        // We want to test both unique and non-unique tags.
        // They may be handled differently.
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with([format!("{a}/{b}/{c}/_foo")]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/{c}-_foo")].map(PathBuf::from),
            );
        });

        with_tempdir(|| {
            let filesystem =
                tagged_filesystem_with([format!("{a}/{b}/{c}-_bar"), format!("{a}/{b}-{c}-_foo")]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/{c}/_bar"), format!("{a}-{b}/{c}/_foo")].map(PathBuf::from),
            );
        });

        // The name could make the last inline tag too long.
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with([format!("{a}/{b}/_{c}")]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/_{c}")].map(PathBuf::from),
            );
        });
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

    #[test]
    fn find_returns_paths_with_tag() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_1", "bar-_2"]);
            assert_eq!(
                filesystem
                    .find(vec![Tag::new("foo".into()).unwrap()], vec![])
                    .unwrap()
                    .collect_vec(),
                [TaggedPath::new("foo-_1".into()).unwrap()]
            );
        })
    }

    #[test]
    fn find_returns_paths_with_all_tags() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo/bar-_1", "foo/_2"]);
            assert_eq!(
                filesystem
                    .find(
                        vec![
                            Tag::new("foo".into()).unwrap(),
                            Tag::new("bar".into()).unwrap()
                        ],
                        vec![],
                    )
                    .unwrap()
                    .collect_vec(),
                [TaggedPath::new("foo/bar-_1".into()).unwrap()]
            );
        })
    }

    #[test]
    fn find_does_not_return_paths_with_excluded_tags() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo/bar-_1", "foo/_2"]);
            assert_eq!(
                filesystem
                    .find(
                        vec![Tag::new("foo".into()).unwrap(),],
                        vec![Tag::new("bar".into()).unwrap()],
                    )
                    .unwrap()
                    .collect_vec(),
                [TaggedPath::new("foo/_2".into()).unwrap()]
            );
        })
    }

    fn list_tagged_paths<P>(path: P) -> Vec<PathBuf>
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

    fn list_files<P>(path: P) -> Vec<PathBuf>
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
