use crate::{name::NameError, tag::TagError};

use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum BuildError {
    Names(#[from] NamesError),
    Tags(#[from] TagsError),
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
pub enum TagsError {
    InvalidString(#[from] StringFromPathError),
    InvalidTag(#[from] TagFromPathError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}. Name is from `{1}`.")]
pub struct NameFromPathError(NameError, PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("{0}. Tag is from `{1}`.")]
pub struct TagFromPathError(TagError, PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("`{0}` is not a valid Unicode string")]
pub struct StringFromPathError(PathBuf);

impl TaggedFilesystem {
    pub fn build(&self) -> Result<(), BuildError> {
        let names = self.names(self.root.files())?;
        let tag_names = self.names(self.root.tags())?;

        self.clean_tag_directories(&names, &tag_names)?;

        self.clean_tagged_paths(&names)?;

        Ok(())
    }

    fn names<P>(&self, dir: P) -> Result<FxHashSet<Name>, NamesError>
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

    fn clean_tag_directories(
        &self,
        names: &FxHashSet<Name>,
        tag_names: &FxHashSet<Name>,
    ) -> Result<(), BuildError> {
        self.remove_tag_directories(names, tag_names)?;
        self.create_tag_directories(names, tag_names)?;
        Ok(())
    }

    fn remove_tag_directories(
        &self,
        names: &FxHashSet<Name>,
        tag_names: &FxHashSet<Name>,
    ) -> Result<(), BuildError> {
        for name in tag_names {
            if !names.contains(name) {
                remove_dir_all(self.root.file_tags(name))?;
            }
        }
        Ok(())
    }

    fn create_tag_directories(
        &self,
        names: &FxHashSet<Name>,
        tag_names: &FxHashSet<Name>,
    ) -> Result<(), BuildError> {
        for name in names {
            if !tag_names.contains(name) {
                create_dir(self.root.file_tags(name))?;
                create_dir(self.root.program_tags(name))?;
            } else {
                let program_tags = self.root.program_tags(name);
                if !program_tags.try_exists()? {
                    create_dir(program_tags)?;
                }
            }
        }
        Ok(())
    }

    fn clean_tagged_paths(&self, names: &FxHashSet<Name>) -> Result<(), BuildError> {
        let (paths, excluded_paths, modified_paths, new_paths) = self.build_paths(names)?;
        let move_ops = build_move_ops(paths, modified_paths);
        let (ops, new_paths) = build_ops(move_ops, &excluded_paths, new_paths);
        self.apply_build(ops, excluded_paths, new_paths)?;
        Ok(())
    }

    #[allow(clippy::type_complexity)]
    fn build_paths(
        &self,
        names: &FxHashSet<Name>,
    ) -> Result<
        (
            Vec<TaggedPath>,
            Vec<TaggedPath>,
            FxHashMap<PathBuf, PathBuf>,
            FxHashSet<PathBuf>,
        ),
        BuildError,
    > {
        let mut path_names = FxHashSet::default();
        let (excluded_paths, mut paths) = self.tagged_paths().partition::<Vec<_>, _>(|path| {
            !names.contains(path.name()) || !path_names.insert(path.name().to_owned())
        });

        let mut modified_paths = FxHashMap::default();
        for path in &mut paths {
            let tags = self.tags(path.name())?;
            let path_tags = path.tags().map(|tag| tag.to_owned()).collect();
            if tags != path_tags {
                let original_path = path.as_path().to_owned();
                *path = TaggedPath::from_tags(&tags, path.name());
                modified_paths.insert(path.as_path().to_owned(), original_path);
            }
        }

        let mut new_paths = FxHashSet::default();
        for name in names {
            if !path_names.contains(name) {
                let path = TaggedPath::from_tags(&self.tags(name)?, name);
                new_paths.insert(path.as_path().to_owned());
                paths.push(path);
            }
        }

        Ok((paths, excluded_paths, modified_paths, new_paths))
    }

    fn apply_build(
        &self,
        ops: impl IntoIterator<Item = Op>,
        excluded_paths: impl IntoIterator<Item = TaggedPath>,
        new_paths: impl IntoIterator<Item = PathBuf>,
    ) -> std::io::Result<()> {
        for path in excluded_paths {
            remove_file(self.root.join(path))?
        }

        self.apply_all(ops)?;

        for path in new_paths
            .into_iter()
            .map(|path| TaggedPath::from_path(path).unwrap())
        {
            let file_path = self.root.file(path.name());
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

fn build_move_ops(
    paths: Vec<TaggedPath>,
    mut modified_paths: FxHashMap<PathBuf, PathBuf>,
) -> Vec<MoveOp> {
    let mut move_ops = organize(&paths);
    for move_op in &mut move_ops {
        if let Some(original_path) = modified_paths.remove(&move_op.from) {
            move_op.from = original_path;
        }
    }
    move_ops.extend(
        modified_paths
            .into_iter()
            .map(|(to, from)| MoveOp { from, to }),
    );
    move_ops
}

fn build_ops<'a>(
    move_ops: impl IntoIterator<Item = MoveOp>,
    excluded_paths: impl IntoIterator<Item = &'a TaggedPath>,
    new_paths: FxHashSet<PathBuf>,
) -> (Vec<Op>, Vec<PathBuf>) {
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

    (ops, new_paths)
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use proptest::prelude::{prop::collection::vec, *};
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::{list_files, list_tagged_paths},
        testing::{
            make_file_and_parent, tagged_filesystem, tagged_filesystem_with, with_temp_dir,
            TaggedPaths,
        },
        Tag,
    };

    use super::*;

    #[proptest(cases = 20)]
    fn build_removes_tags_for_missing_files(paths: TaggedPaths, other_paths: TaggedPaths) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);

            let expected = list_files(&filesystem.root);

            for path in other_paths {
                for tag in path.tags() {
                    make_file_and_parent(filesystem.root.tag(path.name(), tag));
                }
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_a_tags_directory_for_files_without(paths: TaggedPaths, names: Vec<Name>) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for name in &names {
                filesystem.touch([], name.clone()).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for name in &names {
                remove_dir_all(filesystem.root.file_tags(name)).unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_a_program_tags_directory_for_files_without(paths: TaggedPaths, names: Vec<Name>) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for name in &names {
                filesystem.touch([], name.clone()).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for name in &names {
                remove_dir(filesystem.root.program_tags(name)).unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_removes_tagged_paths_for_missing_files(paths: TaggedPaths, other_paths: TaggedPaths) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);

            let expected = list_files(&filesystem.root);

            for path in other_paths {
                filesystem
                    .touch(
                        path.tags().map(|tag| tag.to_owned()),
                        path.name().to_owned(),
                    )
                    .unwrap();
                remove_file(filesystem.root.file(path.name())).unwrap();
                remove_dir_all(filesystem.root.file_tags(path.name())).unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_tagged_paths_for_files_without(paths: TaggedPaths, other_paths: TaggedPaths) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));

            let expected = list_files(&filesystem.root);

            for path in other_paths {
                remove_file(
                    filesystem.root.join(
                        filesystem
                            .filtered_tagged_paths(move |x| x.name() == path.name())
                            .next()
                            .unwrap(),
                    ),
                )
                .unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_deletes_extra_tags_from_tagged_paths(
        paths: TaggedPaths,
        #[strategy(
            TaggedPaths::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        other_paths_extra_tags: (TaggedPaths, Vec<Tag>),
    ) {
        let (other_paths, extra_tags) = other_paths_extra_tags;
        let other_paths = other_paths
            .into_iter()
            .zip(&extra_tags)
            .map(|(path, tag)| {
                TaggedPath::from_tags(
                    &path
                        .tags()
                        .filter(|path_tag| path_tag != &tag.as_ref())
                        .collect(),
                    path.name(),
                )
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));

            let expected = list_files(&filesystem.root);

            for (path, tag) in other_paths.into_iter().zip(extra_tags) {
                filesystem.rm(path.name().to_owned()).unwrap();
                filesystem
                    .touch(
                        path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                        path.name().to_owned(),
                    )
                    .unwrap();
                remove_file(filesystem.root.tag(path.name(), tag)).unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_adds_missing_tags_to_tagged_paths(
        paths: TaggedPaths,
        #[strategy(
            TaggedPaths::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        other_paths_missing_tags: (TaggedPaths, Vec<Tag>),
    ) {
        let (other_paths, missing_tags) = other_paths_missing_tags;
        let other_paths = other_paths
            .into_iter()
            .zip(&missing_tags)
            .map(|(path, tag)| {
                TaggedPath::from_tags(
                    &path
                        .tags()
                        .filter(|path_tag| path_tag != &tag.as_ref())
                        .collect(),
                    path.name(),
                )
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for (path, tag) in other_paths.iter().zip(&missing_tags) {
                filesystem
                    .touch(
                        path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                        path.name().to_owned(),
                    )
                    .unwrap();
            }

            let expected = list_files(&filesystem.root);

            for (path, tag) in other_paths.iter().zip(&missing_tags) {
                filesystem.rm(path.name().to_owned()).unwrap();
                filesystem
                    .touch(
                        path.tags().map(|tag| tag.to_owned()),
                        path.name().to_owned(),
                    )
                    .unwrap();
                File::create(filesystem.root.tag(path.name(), tag)).unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_deletes_duplicate_tagged_paths(paths: TaggedPaths, other_paths: TaggedPaths) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&other_paths));
            let expected = list_files(&filesystem.root);

            for path in other_paths {
                symlink_file(
                    filesystem.root.file(path.name()),
                    filesystem.root.join(format!("foobar-_{}", path.name())),
                )
                .unwrap();
            }
            filesystem.build().unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[test]
    fn build_organizes_paths_into_optimal_tag_directories() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                ["a/b/c/_foo", "a-b-_bar", "d/e-_baz", "🙂/🙁/_fez", "_fiz"],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["_fiz", "a-b/_bar", "a-b/c-_foo", "d-e-_baz", "🙁-🙂-_fez"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_organizes_paths_into_optimal_tag_directories_when_all_have_same_tags() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["a-b-c-_bar", "a/b/c/_foo"]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a-b-c/_bar", "a-b-c/_foo"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_breaks_ties_in_favor_of_increasing_length() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["a/bb/_1", "bb/_2", "a/_3", "dd-ccc-_4"]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                ["a-_3", "bb/_2", "bb/a-_1", "ccc-dd-_4"].map(PathBuf::from),
            )
        })
    }

    #[test]
    fn build_ignores_untagged_paths() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["a-_foo"]);
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

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            for dir in dirs.0.iter() {
                filesystem
                    .mkdir(dir.tags().map(|tag| tag.to_owned()), dir.name().to_owned())
                    .unwrap();
            }
            for (dir, paths) in read_paths(filesystem.root.files()).unwrap().zip(dirs_paths) {
                let filesystem = TaggedFilesystem::init(dir).unwrap();
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
    fn build_ignores_hidden_paths(paths: TaggedPaths, hidden_paths: TaggedPaths) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for path in hidden_paths {
                make_file_and_parent(filesystem.root.join(format!(".{path}")));
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
            let filesystem = tagged_filesystem(dir);
            const BREADTH: usize = 200;
            const DEPTH: usize = 50;
            let names = (0..(BREADTH * 2))
                .map(|i| Name::new(i.to_string()).unwrap())
                .collect_vec();
            let names_tags = (0..BREADTH)
                .flat_map(|n| {
                    let x = n * DEPTH;
                    let tags = (x..x + DEPTH)
                        .map(|x| Tag::new(format!("{x:04}")).unwrap())
                        .collect_vec();
                    [tags.clone(), tags]
                })
                .collect_vec();
            debug_assert_eq!(names.len(), names_tags.len());
            let paths = names
                .iter()
                .zip(&names_tags)
                .map(|(name, tags)| {
                    PathBuf::from(tags.iter().map(|tag| tag.to_string()).join("-"))
                        .join(format!("_{name}"))
                })
                .collect_vec();
            for ((name, tags), path) in names.iter().zip(&names_tags).zip(&paths) {
                File::create(filesystem.root.file(name)).unwrap();
                create_dir_all(filesystem.root.program_tags(name)).unwrap();
                for tag in tags {
                    File::create(filesystem.root.tag(name, tag)).unwrap();
                }
                make_file_and_parent(filesystem.root.join(path));
            }

            filesystem.build().unwrap();
            assert_eq!(list_tagged_paths(filesystem.root), paths)
        })
    }

    #[test]
    fn build_uses_directories_if_filename_gets_too_long() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);

        // We want to test both unique and non-unique tags.
        // They may be handled differently.
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, [format!("{a}/{b}/{c}/_foo")]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/{c}-_foo")].map(PathBuf::from),
            );
        });

        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [format!("{a}/{b}/{c}-_bar"), format!("{a}/{b}-{c}-_foo")],
            );
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/{c}/_bar"), format!("{a}-{b}/{c}/_foo")].map(PathBuf::from),
            );
        });

        // The name could make the last inline tag too long.
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, [format!("{a}/{b}/_{c}")]);
            filesystem.build().unwrap();
            assert_eq!(
                list_tagged_paths(filesystem.root),
                [format!("{a}-{b}/_{c}")].map(PathBuf::from),
            );
        });
    }
}
