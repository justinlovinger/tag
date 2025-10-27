use std::ffi::OsStr;

use crate::{
    fs::{remove_symlink, RemoveSymlinkError},
    name::NameError,
    tags_script::{TagsError, TagsScript},
    ExtRef, TagRef, EXT_SEPARATOR, INLINE_SEPARATOR, TAG_IGNORE,
};

use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum BuildError {
    Names(#[from] NamesError),
    TagsScript(#[from] crate::tags_script::NewError),
    Tags(#[from] TagsError),
    Apply(#[from] ApplyError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum NamesError {
    InvalidString(#[from] StringFromPathError),
    InvalidName(#[from] NameFromPathError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum ApplyError {
    RemoveTaggedPath(#[from] RemovePathError),
    Op(#[from] OpError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}. Name is from `{1}`.")]
pub struct NameFromPathError(NameError, PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("`{0}` is not a valid Unicode string")]
pub struct StringFromPathError(PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("Error removing tagged path `{0}`: {1}")]
pub struct RemovePathError(PathBuf, RemoveSymlinkError);

#[derive(Debug)]
struct BuildPaths {
    paths: Vec<TaggedPath>,
    excluded_paths: Vec<TaggedPath>,
    path_modifications_rev: FxHashMap<PathBuf, PathBuf>,
    new_paths: FxHashMap<PathBuf, Name>,
}

impl TaggedFilesystem {
    pub fn build(&self) -> Result<(), BuildError> {
        self.apply_build_paths(self.all_build_paths(&self.all_names(self.root.files())?)?)
    }

    fn all_names<P>(&self, dir: P) -> Result<FxHashSet<Name>, NamesError>
    where
        P: AsRef<Path>,
    {
        let mut names = FxHashSet::default();
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            match entry.file_name().into_string() {
                Ok(s) => match Name::new(s) {
                    Ok(name) => {
                        names.insert(name);
                    }
                    Err(e) => return Err(NameFromPathError(e, entry.path()).into()),
                },
                Err(_) => return Err(StringFromPathError(entry.path()).into()),
            }
        }
        Ok(names)
    }

    fn all_build_paths(&self, names: &FxHashSet<Name>) -> Result<BuildPaths, BuildError> {
        let mut tags_script = TagsScript::new(&self.root)?;

        let mut path_names = FxHashSet::default();
        let (mut paths, excluded_paths) =
            self.tagged_paths_with_names()
                .partition::<Vec<_>, _>(|(_, name)| {
                    if let Some(name) = name {
                        // We still have to check if `names` contains `name`,
                        // because a symlink could target a file outside `.tag/files/`.
                        names.contains(name) && path_names.insert(name.to_owned())
                    } else {
                        false
                    }
                });

        let mut path_modifications_rev = FxHashMap::default();
        for (i, (path, name)) in paths.iter_mut().enumerate() {
            let name = name.as_ref().expect("included paths should have names");
            let tags = tags_script.tags(name)?;
            let path_tags = path.tags().map(|tag| tag.to_owned()).collect();
            if tags != path_tags || name.ext() != path.ext() {
                let original_path = path.as_path().to_owned();
                *path = from_tags_with_ignored(
                    &tags,
                    [Tag::new(format!("mod{i}")).unwrap()],
                    name.ext(),
                );
                path_modifications_rev.insert(path.as_path().to_owned(), original_path);
            }
        }

        let mut paths = paths.into_iter().map(|(path, _)| path).collect::<Vec<_>>();
        let excluded_paths = excluded_paths.into_iter().map(|(path, _)| path).collect();

        let mut new_paths = FxHashMap::default();
        for (i, name) in names.iter().enumerate() {
            if !path_names.contains(name) {
                let path = from_tags_with_ignored(
                    &tags_script.tags(name)?,
                    [Tag::new(format!("new{i}")).unwrap()],
                    name.ext(),
                );
                new_paths.insert(path.as_path().to_owned(), name.to_owned());
                paths.push(path);
            }
        }

        Ok(BuildPaths {
            paths,
            excluded_paths,
            path_modifications_rev,
            new_paths,
        })
    }

    pub fn build_some(&self, considered_names: Vec<Name>) -> Result<(), BuildError> {
        self.apply_build_paths(self.some_build_paths(
            &self.some_names(self.root.files(), &considered_names)?,
            &considered_names,
        )?)
    }

    fn some_names<'a, P>(
        &self,
        dir: P,
        considered_names: &'a [Name],
    ) -> Result<FxHashSet<&'a Name>, NamesError>
    where
        P: AsRef<Path>,
    {
        let mut names = FxHashSet::default();
        for name in considered_names {
            let path = dir.as_ref().join(name.as_path());
            if path.try_exists()? {
                names.insert(name);
            }
        }
        Ok(names)
    }

    fn some_build_paths(
        &self,
        names: &FxHashSet<&Name>,
        considered_names: &[Name],
    ) -> Result<BuildPaths, BuildError> {
        let mut tags_script = TagsScript::new(&self.root)?;

        let considered_names = considered_names.iter().collect::<FxHashSet<_>>();

        let mut path_names = FxHashSet::default();
        let (paths, other_paths) =
            self.tagged_paths_with_names()
                .partition::<Vec<_>, _>(|(_, name)| {
                    name.as_ref()
                        .is_some_and(|name| considered_names.contains(name))
                });
        let (paths, excluded_paths) = paths.into_iter().partition::<Vec<_>, _>(|(_, name)| {
            let name = name
                .as_ref()
                .expect("paths should have names by this point");
            names.contains(name) && path_names.insert(name.to_owned())
        });

        let mut path_modifications = FxHashMap::default();
        let mut path_modifications_rev = FxHashMap::default();
        for (i, (path, name)) in paths.iter().enumerate() {
            let name = name.as_ref().expect("included paths should have names");
            let tags = tags_script.tags(name)?;
            let path_tags = path.tags().map(|tag| tag.to_owned()).collect();
            if tags != path_tags || name.ext() != path.ext() {
                let modified_path = from_tags_with_ignored(
                    &tags,
                    [Tag::new(format!("mod{i}")).unwrap()],
                    name.ext(),
                );
                path_modifications_rev.insert(
                    modified_path.as_path().to_owned(),
                    path.as_path().to_owned(),
                );
                path_modifications.insert(path.to_owned(), modified_path);
            }
        }

        let mut paths = paths.into_iter().map(|(path, _)| path).collect::<Vec<_>>();
        let other_paths = other_paths
            .into_iter()
            .map(|(path, _)| path)
            .collect::<Vec<_>>();
        let excluded_paths = excluded_paths
            .into_iter()
            .map(|(path, _)| path)
            .collect::<Vec<_>>();

        let mut new_paths = Vec::new();
        for (i, name) in names.iter().enumerate() {
            if !path_names.contains(name.as_ref()) {
                new_paths.push((
                    from_tags_with_ignored(
                        &tags_script.tags(name)?,
                        [Tag::new(format!("new{i}")).unwrap()],
                        name.ext(),
                    ),
                    *name,
                ));
            }
        }

        paths.extend(relevant_paths(
            paths
                .iter()
                .chain(excluded_paths.iter())
                .chain(path_modifications.values())
                .chain(new_paths.iter().map(|(path, _)| path)),
            other_paths,
        ));

        for path in &mut paths {
            if let Some(modified_path) = path_modifications.remove(path) {
                *path = modified_path;
            }
        }
        debug_assert!(path_modifications.is_empty());

        paths.extend(new_paths.iter().map(|(path, _)| path).cloned());

        Ok(BuildPaths {
            paths,
            excluded_paths,
            path_modifications_rev,
            new_paths: new_paths
                .into_iter()
                .map(|(path, name)| (path.into_path(), name.to_owned()))
                .collect(),
        })
    }

    fn apply_build_paths(&self, build_paths: BuildPaths) -> Result<(), BuildError> {
        let move_ops = with_swap_ops(build_move_ops(
            build_paths.paths,
            build_paths.path_modifications_rev,
        ));
        let (ops, new_paths) =
            build_ops(move_ops, &build_paths.excluded_paths, build_paths.new_paths);
        self.apply_build(ops, build_paths.excluded_paths, new_paths)?;
        Ok(())
    }

    fn apply_build(
        &self,
        ops: impl IntoIterator<Item = Op>,
        excluded_paths: impl IntoIterator<Item = TaggedPath>,
        new_paths: impl IntoIterator<Item = (PathBuf, Name)>,
    ) -> Result<(), ApplyError> {
        for path in excluded_paths {
            remove_symlink(self.root.join(&path)).map_err(|e| RemovePathError(path.into(), e))?;
        }

        self.apply_all(ops)?;

        for (path, name) in new_paths
            .into_iter()
            .map(|(path, name)| (TaggedPath::from_path(path).unwrap(), name))
        {
            let file_path = self.root.file(name);
            let link_path = self.root.join(path);
            if file_path.is_dir() {
                symlink_dir(file_path, link_path)?;
            } else {
                symlink_file(file_path, link_path)?;
            }
        }

        Ok(())
    }
}

fn from_tags_with_ignored<T, J, E>(
    tags: &FxHashSet<T>,
    ignored_tags: impl IntoIterator<Item = J>,
    ext: E,
) -> TaggedPath
where
    T: AsRef<TagRef>,
    J: AsRef<TagRef>,
    E: AsRef<ExtRef>,
{
    let sep = if tags.is_empty() {
        String::new()
    } else {
        INLINE_SEPARATOR.to_string()
    };
    TaggedPath::new(format!(
        "{}{sep}{}{EXT_SEPARATOR}{}",
        tags.iter()
            .map(|tag| tag.as_ref())
            .format(INLINE_SEPARATOR.to_string().as_str()),
        ignored_tags
            .into_iter()
            .format_with(INLINE_SEPARATOR.to_string().as_str(), |s, f| {
                f(&TAG_IGNORE)?;
                f(&s.as_ref())
            }),
        ext.as_ref(),
    ))
    .unwrap()
}

fn build_move_ops(
    paths: Vec<TaggedPath>,
    mut path_modifications_rev: FxHashMap<PathBuf, PathBuf>,
) -> Vec<MoveOp> {
    let mut move_ops = organize(&paths);
    for move_op in &mut move_ops {
        if let Some(original_path) = path_modifications_rev.remove(&move_op.from) {
            move_op.from = original_path;
        }
    }
    move_ops.extend(
        path_modifications_rev
            .into_iter()
            .map(|(to, from)| MoveOp { from, to }),
    );
    move_ops
}

fn with_swap_ops(move_ops: impl IntoIterator<Item = MoveOp>) -> impl Iterator<Item = MoveOp> {
    let move_ops = move_ops
        .into_iter()
        .map(|op| (op.from, op.to))
        .collect::<FxHashMap<_, _>>();
    let mut swaps = FxHashSet::default();
    for (from, to) in move_ops.iter() {
        if let Some(other_to) = move_ops.get(to) {
            if other_to == from {
                let from = from.clone();
                let to = to.clone();
                // Each pair should only be in `swap_ops` once.
                // We can accomplish that by ordering pairs.
                swaps.insert(if from < to { (from, to) } else { (to, from) });
            }
        }
    }
    let swap_ops = swaps
        .iter()
        .flat_map(|(x, y)| {
            // Theoretically,
            // a file could have the below extension,
            // but that is highly unlikely.
            // Adding a UUID would avoid that issue.
            let tmp = add_extension(y, "tagswap");
            [
                MoveOp {
                    from: x.clone(),
                    to: tmp.clone(),
                },
                MoveOp {
                    from: y.clone(),
                    to: x.clone(),
                },
                MoveOp {
                    from: tmp,
                    to: y.clone(),
                },
            ]
        })
        .collect::<Vec<_>>();
    move_ops
        .into_iter()
        .filter_map(move |(from, to)| {
            let first_order = (from, to);
            let contains_first = swaps.contains(&first_order);
            let (from, to) = first_order;
            let second_order = (to, from);
            let contains_second = swaps.contains(&second_order);
            let (to, from) = second_order;
            if contains_first || contains_second {
                None
            } else {
                Some(MoveOp { from, to })
            }
        })
        .chain(swap_ops)
}

fn add_extension<P>(path: P, extension: impl AsRef<OsStr>) -> PathBuf
where
    P: AsRef<Path>,
{
    let new_ext = match path.as_ref().extension() {
        Some(existing) => {
            let mut ext = existing.to_os_string();
            ext.push(".");
            ext.push(extension);
            ext
        }
        None => extension.as_ref().to_os_string(),
    };
    path.as_ref().with_extension(new_ext)
}

fn build_ops<'a>(
    move_ops: impl IntoIterator<Item = MoveOp>,
    excluded_paths: impl IntoIterator<Item = &'a TaggedPath>,
    new_paths: FxHashMap<PathBuf, Name>,
) -> (Vec<Op>, Vec<(PathBuf, Name)>) {
    let ops = from_move_ops(
        move_ops
            .into_iter()
            // We can use fake move-ops to generate `DeleteDirectoryIfEmpty` ops.
            // A real move-op will never have an empty `to`.
            .chain(excluded_paths.into_iter().map(|path| MoveOp {
                from: path.as_path().to_owned(),
                to: PathBuf::new(),
            }))
            .collect(),
    )
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
        let mut unmoved_new_paths = new_paths;
        let mut new_paths = Vec::new();
        let ops = ops
            .filter(|op| match op {
                Op::EnsureDirectory(_) => true,
                Op::Move(MoveOp { from, to }) => {
                    if let Some(name) = unmoved_new_paths.remove(from) {
                        new_paths.push((to.clone(), name));
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

    (ops, new_paths)
}

#[cfg(test)]
mod tests {
    use std::fs::{remove_file, File};

    use proptest::prelude::{prop::collection::vec, *};
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::testing::{
            list_files, list_tagged_paths, tagged_filesystem, tagged_filesystem_with,
            TaggedPathsWithNames,
        },
        testing::{
            create_file_and_parent, name, tag, with_temp_dir, TaggedPathParams, TaggedPaths,
            TaggedPathsParams,
        },
        Tag,
    };

    use super::*;

    #[proptest(cases = 20)]
    fn build_removes_tagged_paths_for_missing_files(
        paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, other_paths);
            filesystem.build().unwrap();
            let expected = list_files(&filesystem.root);

            for (path, name) in &paths {
                filesystem.symlink_file(path, name);
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_tagged_paths_for_files_without(
        paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));
            filesystem.build().unwrap();
            let expected = list_files(&filesystem.root);

            let tagged_paths: FxHashMap<_, _> = filesystem
                .tagged_paths_with_names()
                .filter_map(|(path, name)| name.map(|name| (name, path)))
                .collect();
            for (_, name) in &paths {
                remove_file(filesystem.root.join(tagged_paths.get(name).unwrap())).unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_deletes_extra_tags_from_tagged_paths(
        #[strategy(
            TaggedPathsWithNames::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        paths_extra_tags: (TaggedPathsWithNames, Vec<Tag>),
        other_paths: TaggedPathsWithNames,
    ) {
        let (paths, extra_tags) = paths_extra_tags;
        let paths = paths
            .into_iter()
            .zip(&extra_tags)
            .map(|((path, name), tag)| {
                (
                    from_tags_with_ignored(
                        &path
                            .tags()
                            .filter(|path_tag| path_tag != &tag.as_ref())
                            .collect(),
                        path.ignored_tags().map(TagRef::new),
                        path.ext(),
                    ),
                    name,
                )
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));
            filesystem.build().unwrap();
            let expected = list_files(&filesystem.root);

            for ((_, name), tag) in paths.iter().zip(&extra_tags) {
                filesystem.add_tag(name, tag);
            }
            filesystem.build().unwrap();
            for ((_, name), tag) in paths.iter().zip(&extra_tags) {
                filesystem.del_tag(name, tag);
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_missing_tags_to_tagged_paths(
        #[strategy(
            TaggedPathsWithNames::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        paths_missing_tags: (TaggedPathsWithNames, Vec<Tag>),
        other_paths: TaggedPathsWithNames,
    ) {
        let (paths, missing_tags) = paths_missing_tags;
        let paths = paths
            .into_iter()
            .zip(&missing_tags)
            .map(|((path, name), tag)| {
                (
                    from_tags_with_ignored(
                        &path
                            .tags()
                            .filter(|path_tag| path_tag != &tag.as_ref())
                            .collect(),
                        path.ignored_tags().map(TagRef::new),
                        path.ext(),
                    ),
                    name,
                )
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, other_paths);
            for ((path, name), tag) in paths.iter().zip(&missing_tags) {
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    name,
                );
            }
            filesystem.build().unwrap();
            let expected = list_files(&filesystem.root);

            for (path, name) in &paths {
                filesystem.rm(name);
                filesystem.touch(path.tags(), name);
            }
            filesystem.build().unwrap();
            for ((_, name), tag) in paths.iter().zip(&missing_tags) {
                filesystem.add_tag(name, tag);
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_deletes_duplicate_tagged_paths(
        paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));
            filesystem.build().unwrap();
            let expected = list_files(&filesystem.root);

            for (_, name) in paths {
                filesystem.symlink_file(&TaggedPath::new(format!("foobar-{name}")).unwrap(), name);
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_fixes_extensions(paths: TaggedPathsWithNames, other_paths: TaggedPathsWithNames) {
        let expected = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));
            filesystem.build().unwrap();
            list_files(&filesystem.root)
        });

        let actual = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                paths
                    .into_iter()
                    .map(|(path, name)| (path.into_path().with_extension("foobar"), name))
                    .chain(
                        other_paths
                            .into_iter()
                            .map(|(path, name)| (path.into_path(), name)),
                    ),
            );
            filesystem.build().unwrap();
            list_files(&filesystem.root)
        });

        prop_assert_eq!(actual, expected);
    }

    #[test]
    fn build_organizes_paths_into_optimal_tag_directories() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [
                    ("a/b/c/_.x", name("foo.x")),
                    ("a-b.x", name("bar.x")),
                    ("d/e.x", name("baz.x")),
                    ("🙂/🙁/_.x", name("fez.x")),
                    ("_.x", name("fiz.x")),
                ],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["_.x", "a-b/_.x", "a-b/c.x", "d-e.x", "🙁-🙂.x"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_organizes_paths_into_optimal_tag_directories_when_all_have_same_tags() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [("a-b-c.x", name("bar.x")), ("a/b/c/_.x", name("foo.x"))],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a-b-c/_1.x", "a-b-c/_2.x"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_breaks_ties_in_favor_of_increasing_length() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [
                    ("a/bb/_.x", name("1.x")),
                    ("bb/_.x", name("2.x")),
                    ("a/_.x", name("3.x")),
                    ("dd-ccc.x", name("4.x")),
                ],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a.x", "bb/_.x", "bb/a.x", "ccc-dd.x"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_ignores_untagged_paths() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, [("foo.x", name("foo.x"))]);
            create_dir(filesystem.root.join("foo")).unwrap();
            rename(
                filesystem.root.join("foo.x"),
                filesystem.root.join("foo/_.x"),
            )
            .unwrap();
            File::create(filesystem.root.join("foo/not-tagged")).unwrap();
            filesystem.build().unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/foo.x",
                    ".tag/tags/foo.x/tag/foo",
                    ".tag/tags.sh",
                    "foo/not-tagged",
                    "foo.x"
                ]
                .map(PathBuf::from),
            )
        })
    }

    #[proptest(cases = 20)]
    fn build_ignores_nested_tagged_paths(
        #[strategy(
            TaggedPathsWithNames::arbitrary()
                .prop_flat_map(|paths| (
                    vec(TaggedPathsWithNames::arbitrary(), paths.len()),
                    Just(paths)
                ))
                .prop_map(|(x, y)| (y, x))
        )]
        args: (TaggedPathsWithNames, Vec<TaggedPathsWithNames>),
    ) {
        let (dirs, dirs_paths) = args;

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            for (dir, name) in dirs.iter() {
                filesystem.mkdir(dir.tags(), name);
            }
            filesystem.build().unwrap();
            for (dir, paths) in read_paths(filesystem.root.files()).unwrap().zip(dirs_paths) {
                let filesystem = TaggedFilesystem::init(dir).unwrap();
                for (path, name) in paths {
                    filesystem.touch(path.tags(), name);
                }
            }
            let expected = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let actual = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn build_errors_on_non_link_paths() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, [("foo.x", name("foo.x"))]);
            File::create(filesystem.root.join("bar.x")).unwrap();
            assert!(filesystem.build().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/foo.x",
                    ".tag/tags/foo.x/tag/foo",
                    ".tag/tags.sh",
                    "bar.x",
                    "foo.x"
                ]
                .map(PathBuf::from),
            )
        });

        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, [("foo.x", name("foo.x"))]);
            create_dir(filesystem.root.join("bar.x")).unwrap();
            assert!(filesystem.build().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/foo.x",
                    ".tag/tags/foo.x/tag/foo",
                    ".tag/tags.sh",
                    "bar.x",
                    "foo.x"
                ]
                .map(PathBuf::from),
            )
        });
    }

    // Note,
    // this test doesn't check if two files with the same tags and extension swapped.
    #[proptest(cases = 20)]
    fn build_is_idempotent(paths: TaggedPathsWithNames) {
        let (first_pass, second_pass) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);

            filesystem.build().unwrap();
            let first_pass = list_files(&filesystem.root);

            filesystem.build().unwrap();
            (list_files(filesystem.root), first_pass)
        });

        prop_assert_eq!(second_pass, first_pass);
    }

    #[proptest(cases = 20)]
    fn build_ignores_hidden_paths(paths: TaggedPathsWithNames, hidden_paths: TaggedPaths) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            filesystem.build().unwrap();
            for path in hidden_paths {
                create_file_and_parent(filesystem.root.join(format!(".{path}")));
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
        with_temp_dir(|dir| {
            // `organize` is prone to stack-overflow
            // if recursion is not handled carefully.
            // This tests we support at least `BREADTH * DEPTH` non-unique tags.
            const BREADTH: usize = 200;
            const DEPTH: usize = 50;
            let names = (0..BREADTH).map(|i| name(format!("{i}.x")));
            let tags = (0..(BREADTH / 2)).flat_map(|n| {
                let x = n * DEPTH;
                let tags = (x..x + DEPTH)
                    .map(|x| Tag::new(format!("{x:04}")).unwrap())
                    .collect::<Vec<_>>();
                [tags.clone(), tags]
            });
            let paths: Vec<_> = tags
                .zip((1..=2).cycle())
                .map(|(tags, i)| {
                    PathBuf::from(tags.iter().map(|tag| tag.to_string()).join("-"))
                        .join(format!("_{i}.x"))
                })
                .collect();
            let filesystem = tagged_filesystem_with(dir, paths.iter().zip(names));

            filesystem.build().unwrap();

            assert_eq!(list_tagged_paths(filesystem.root), paths)
        })
    }

    #[test]
    fn build_uses_directories_if_filename_gets_too_long() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);

        with_temp_dir(|dir| {
            let filesystem =
                tagged_filesystem_with(dir, [(format!("{a}/{b}/{c}/_.x"), name("foo.x"))]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/{c}.x")].map(PathBuf::from),
            );
        });

        // We want to test both unique and non-unique tags.
        // They may be handled differently.
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [
                    (format!("{a}/{b}/{c}.x"), name("bar.x")),
                    (format!("{a}/{b}-{c}.x"), name("foo.x")),
                ],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/{c}/_1.x"), format!("{a}-{b}/{c}/_2.x")].map(PathBuf::from),
            );
        });

        // The extension could make the last inline tag too long.
        with_temp_dir(|dir| {
            let filesystem =
                tagged_filesystem_with(dir, [(format!("{a}/{b}/_.{c}"), name(format!("foo.{c}")))]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/_.{c}")].map(PathBuf::from),
            );
        });
    }

    #[test]
    fn build_handles_paths_with_no_tag_after_slash() {
        with_temp_dir(|dir| {
            let filesystem =
                tagged_filesystem_with(dir, [("a.x", name("foo.x")), ("a-b.x", name("bar.x"))]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a/_.x", "a/b.x"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_handles_paths_with_same_tags_and_ext() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [
                    ("a-b.x", name("foo.x")),
                    ("a/b.x", name("bar.x")),
                    ("a-b-c.x", name("baz.x")),
                    ("a-b/c.x", name("bin.x")),
                ],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a-b/_1.x", "a-b/_2.x", "a-b/c/_1.x", "a-b/c/_2.x"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_multiple_new_files_with_same_name() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            for (tags, name) in [
                (vec![tag("a")], name("foo.x")),
                (vec![tag("a")], name("bar.x")),
            ] {
                filesystem.touch(tags, name);
            }
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a/_1.x", "a/_2.x"].map(PathBuf::from),
            )
        })
    }

    #[proptest(cases = 20)]
    fn build_can_swap_tags(
        #[strategy(TaggedPathsWithNames::arbitrary_with(TaggedPathsParams {
            tagged_path_params: TaggedPathParams::default(),
            min_paths: 2,
            max_paths: 10,
        }))]
        paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, &paths);
            filesystem.build().unwrap();
            let expected = list_tagged_paths(&filesystem.root);

            filesystem.del_tags(&paths.0[0].1);
            filesystem.del_tags(&paths.0[1].1);
            filesystem.add_tags(&paths.0[0].1, paths.0[1].0.tags());
            filesystem.add_tags(&paths.0[1].1, paths.0[0].0.tags());
            let actual = list_tagged_paths(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_equals_build_when_given_all_paths(paths: TaggedPathsWithNames) {
        let expected = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, &paths);
            filesystem.build().unwrap();
            list_files(&filesystem.root)
        });

        let actual = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, &paths);
            filesystem
                .build_some(paths.into_iter().map(|(_, name)| name).collect())
                .unwrap();
            list_files(&filesystem.root)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_removes_tagged_paths_for_considered_names(
        considered_paths: TaggedPathsWithNames,
        ignored_paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, other_paths);
            for (path, name) in &ignored_paths {
                filesystem.touch(path.tags(), name);
            }
            filesystem.build().unwrap();
            for (_, name) in &ignored_paths {
                filesystem.rm(name);
            }
            let expected = list_files(&filesystem.root);

            for (path, name) in &considered_paths {
                filesystem.symlink_file(path, name);
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_adds_tagged_paths_for_considered_names(
        considered_paths: TaggedPathsWithNames,
        ignored_paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem =
                tagged_filesystem_with(dir, considered_paths.iter().chain(&other_paths));
            filesystem.build().unwrap();
            for (path, name) in ignored_paths {
                filesystem.touch(path.tags(), name);
            }
            let expected = list_files(&filesystem.root);

            let tagged_paths: FxHashMap<_, _> = filesystem
                .tagged_paths_with_names()
                .filter_map(|(path, name)| name.map(|name| (name, path)))
                .collect();
            for (_, name) in &considered_paths {
                remove_file(filesystem.root.join(tagged_paths.get(name).unwrap())).unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_deletes_extra_tags_for_considered_names(
        #[strategy(
            TaggedPathsWithNames::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        considered_paths_extra_tags: (TaggedPathsWithNames, Vec<Tag>),
        #[strategy(
            TaggedPathsWithNames::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        ignored_paths_extra_tags: (TaggedPathsWithNames, Vec<Tag>),
        other_paths: TaggedPathsWithNames,
    ) {
        let (considered_paths, considered_extra_tags) = considered_paths_extra_tags;
        let considered_paths = considered_paths
            .into_iter()
            .zip(&considered_extra_tags)
            .map(|((path, name), tag)| {
                (
                    from_tags_with_ignored(
                        &path
                            .tags()
                            .filter(|path_tag| path_tag != &tag.as_ref())
                            .collect(),
                        path.ignored_tags().map(TagRef::new),
                        path.ext(),
                    ),
                    name,
                )
            })
            .collect_vec();

        let (ignored_paths, ignored_extra_tags) = ignored_paths_extra_tags;
        let ignored_paths = ignored_paths
            .into_iter()
            .zip(&ignored_extra_tags)
            .map(|((path, name), tag)| {
                (
                    from_tags_with_ignored(
                        &path.tags().chain([tag.as_ref()]).collect(),
                        path.ignored_tags().map(TagRef::new),
                        path.ext(),
                    ),
                    name,
                )
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                considered_paths
                    .iter()
                    .chain(&ignored_paths)
                    .chain(&other_paths),
            );
            filesystem.build().unwrap();
            for ((_, name), tag) in ignored_paths.into_iter().zip(ignored_extra_tags) {
                filesystem.del_tag(name, tag);
            }
            let expected = list_files(&filesystem.root);

            for ((_, name), tag) in considered_paths.iter().zip(&considered_extra_tags) {
                filesystem.add_tag(name, tag);
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            for ((_, name), tag) in considered_paths.iter().zip(&considered_extra_tags) {
                filesystem.del_tag(name, tag);
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_adds_missing_tags_for_considered_names(
        #[strategy(
            TaggedPathsWithNames::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        considered_paths_missing_tags: (TaggedPathsWithNames, Vec<Tag>),
        #[strategy(
            TaggedPathsWithNames::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        ignored_paths_missing_tags: (TaggedPathsWithNames, Vec<Tag>),
        other_paths: TaggedPathsWithNames,
    ) {
        let (considered_paths, considered_missing_tags) = considered_paths_missing_tags;
        let considered_paths = considered_paths
            .into_iter()
            .zip(&considered_missing_tags)
            .map(|((path, name), tag)| {
                (
                    from_tags_with_ignored(
                        &path
                            .tags()
                            .filter(|path_tag| path_tag != &tag.as_ref())
                            .collect(),
                        path.ignored_tags().map(TagRef::new),
                        path.ext(),
                    ),
                    name,
                )
            })
            .collect_vec();

        let (ignored_paths, ignored_missing_tags) = ignored_paths_missing_tags;
        let ignored_paths = ignored_paths
            .into_iter()
            .zip(&ignored_missing_tags)
            .map(|((path, name), tag)| {
                (
                    from_tags_with_ignored(
                        &path
                            .tags()
                            .filter(|path_tag| path_tag != &tag.as_ref())
                            .collect(),
                        path.ignored_tags().map(TagRef::new),
                        path.ext(),
                    ),
                    name,
                )
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                considered_paths
                    .iter()
                    .chain(&ignored_paths)
                    .chain(&other_paths),
            );
            for ((_, name), tag) in considered_paths.iter().zip(&considered_missing_tags) {
                filesystem.add_tag(name, tag);
            }
            filesystem.build().unwrap();
            for ((_, name), tag) in ignored_paths.iter().zip(&ignored_missing_tags) {
                filesystem.add_tag(name, tag);
            }
            let expected = list_files(&filesystem.root);

            for ((_, name), tag) in considered_paths.iter().zip(&considered_missing_tags) {
                filesystem.del_tag(name, tag);
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            for ((_, name), tag) in considered_paths.iter().zip(&considered_missing_tags) {
                filesystem.add_tag(name, tag);
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_deletes_duplicate_tagged_paths_for_considered_names(
        considered_paths: TaggedPathsWithNames,
        ignored_paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                considered_paths
                    .iter()
                    .chain(&ignored_paths)
                    .chain(&other_paths),
            );
            filesystem.build().unwrap();
            for (_, name) in ignored_paths {
                filesystem.symlink_file(&TaggedPath::new(name.to_string()).unwrap(), name);
            }
            let expected = list_files(&filesystem.root);

            for (_, name) in &considered_paths {
                filesystem.symlink_file(&TaggedPath::new(name.to_string()).unwrap(), name);
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_fixes_extensions_for_considered_names(
        considered_paths: TaggedPathsWithNames,
        ignored_paths: TaggedPathsWithNames,
        other_paths: TaggedPathsWithNames,
    ) {
        // Testing changing extensions on paths with the same tags
        // is difficult.
        let add_tag = |paths: TaggedPathsWithNames| {
            paths
                .into_iter()
                .map(|(path, name)| {
                    let tags_str = path
                        .as_path()
                        .as_os_str()
                        .to_str()
                        .unwrap()
                        .split_once(EXT_SEPARATOR)
                        .unwrap()
                        .0;
                    let path =
                        TaggedPath::new(format!("{}{INLINE_SEPARATOR}{}", tags_str, name)).unwrap();
                    (path, name)
                })
                .collect::<Vec<_>>()
        };
        let considered_paths = add_tag(considered_paths);
        let other_paths = add_tag(other_paths);

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                considered_paths
                    .iter()
                    .chain(&ignored_paths)
                    .chain(&other_paths),
            );
            filesystem.build().unwrap();
            let tagged_paths: FxHashMap<_, _> = filesystem
                .tagged_paths_with_names()
                .filter_map(|(path, name)| name.map(|name| (name, path)))
                .collect();
            for (_, name) in &ignored_paths {
                let path = tagged_paths.get(name).unwrap();
                rename(
                    filesystem.root.join(path),
                    filesystem
                        .root
                        .join(path.as_path().with_extension("foobar")),
                )
                .unwrap();
            }
            let expected = list_files(&filesystem.root);

            for (_, name) in &considered_paths {
                let path = tagged_paths.get(name).unwrap();
                rename(
                    filesystem.root.join(path),
                    filesystem
                        .root
                        .join(path.as_path().with_extension("foobar")),
                )
                .unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|(_, name)| name.to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }
}
