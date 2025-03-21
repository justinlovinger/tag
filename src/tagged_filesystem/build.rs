use crate::{
    fs::{remove_symlink, RemoveSymlinkError},
    name::NameError,
    tag::TagError,
};

use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum BuildError {
    Names(#[from] NamesError),
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
pub enum TagsError {
    InvalidString(#[from] StringFromPathError),
    InvalidTag(#[from] TagFromPathError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum ApplyError {
    RemoveTaggedPath(#[from] RemovePathError),
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

#[derive(Debug, thiserror::Error)]
#[error("Error removing tagged path `{0}`: {1}")]
pub struct RemovePathError(PathBuf, RemoveSymlinkError);

struct BuildPaths {
    paths: Vec<TaggedPath>,
    excluded_paths: Vec<TaggedPath>,
    path_modifications_rev: FxHashMap<PathBuf, PathBuf>,
    new_paths: FxHashSet<PathBuf>,
}

impl TaggedFilesystem {
    pub fn build(&self) -> Result<(), BuildError> {
        let names = self.all_names(self.root.files())?;
        self.clean_tag_directories(&names, &self.all_names(self.root.tags())?)?;
        self.clean_tagged_paths(self.all_build_paths(&names)?)
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
        let mut path_names = FxHashSet::default();
        let (mut paths, excluded_paths) = self.tagged_paths().partition::<Vec<_>, _>(|path| {
            names.contains(path.name()) && path_names.insert(path.name().to_owned())
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

        Ok(BuildPaths {
            paths,
            excluded_paths,
            path_modifications_rev: modified_paths,
            new_paths,
        })
    }

    pub fn build_some(&self, considered_names: Vec<Name>) -> Result<(), BuildError> {
        let names = self.some_names(self.root.files(), &considered_names)?;
        self.clean_tag_directories(
            &names,
            &self.some_names(self.root.tags(), &considered_names)?,
        )?;
        self.clean_tagged_paths(self.some_build_paths(&names, &considered_names)?)
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

    fn some_build_paths<'a>(
        &self,
        names: &FxHashSet<&'a Name>,
        considered_names: &[Name],
    ) -> Result<BuildPaths, BuildError> {
        let considered_names = considered_names.iter().collect::<FxHashSet<_>>();

        let mut path_names = FxHashSet::default();
        let (paths, other_paths) = self
            .tagged_paths()
            .partition::<Vec<_>, _>(|path| considered_names.contains(path.name()));
        let (mut paths, excluded_paths) = paths.into_iter().partition::<Vec<_>, _>(|path| {
            names.contains(path.name()) && path_names.insert(path.name().to_owned())
        });

        let mut path_modifications = FxHashMap::default();
        let mut path_modifications_rev = FxHashMap::default();
        for path in &paths {
            let tags = self.tags(path.name())?;
            let path_tags = path.tags().map(|tag| tag.to_owned()).collect();
            if tags != path_tags {
                let modified_path = TaggedPath::from_tags(&tags, path.name());
                path_modifications_rev.insert(
                    modified_path.as_path().to_owned(),
                    path.as_path().to_owned(),
                );
                path_modifications.insert(path.to_owned(), modified_path);
            }
        }

        let mut new_paths = Vec::new();
        for name in names {
            if !path_names.contains(name.as_ref()) {
                new_paths.push(TaggedPath::from_tags(&self.tags(name)?, name));
            }
        }

        paths.extend(relevant_paths(
            paths
                .iter()
                .chain(excluded_paths.iter())
                .chain(path_modifications.values())
                .chain(new_paths.iter())
                .flat_map(|path| path.tags())
                .map(|tag| tag.to_owned())
                .collect(),
            other_paths,
        ));

        for path in &mut paths {
            if let Some(modified_path) = path_modifications.remove(path) {
                *path = modified_path;
            }
        }
        debug_assert!(path_modifications.is_empty());

        paths.extend(new_paths.iter().cloned());

        Ok(BuildPaths {
            paths,
            excluded_paths,
            path_modifications_rev,
            new_paths: new_paths.into_iter().map(|path| path.into_path()).collect(),
        })
    }

    fn clean_tag_directories<N>(
        &self,
        names: &FxHashSet<N>,
        tag_names: &FxHashSet<N>,
    ) -> Result<(), BuildError>
    where
        N: AsRef<NameRef> + std::hash::Hash + Eq,
    {
        self.remove_tag_directories(names, tag_names)?;
        self.create_tag_directories(names, tag_names)?;
        Ok(())
    }

    fn remove_tag_directories<N>(
        &self,
        names: &FxHashSet<N>,
        tag_names: &FxHashSet<N>,
    ) -> Result<(), BuildError>
    where
        N: AsRef<NameRef> + std::hash::Hash + Eq,
    {
        for name in tag_names {
            if !names.contains(name) {
                remove_dir_all(self.root.file_tags(name))?;
            }
        }
        Ok(())
    }

    fn create_tag_directories<N>(
        &self,
        names: &FxHashSet<N>,
        tag_names: &FxHashSet<N>,
    ) -> Result<(), BuildError>
    where
        N: AsRef<NameRef> + std::hash::Hash + Eq,
    {
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

    fn clean_tagged_paths(&self, build_paths: BuildPaths) -> Result<(), BuildError> {
        let move_ops = build_move_ops(build_paths.paths, build_paths.path_modifications_rev);
        let (ops, new_paths) =
            build_ops(move_ops, &build_paths.excluded_paths, build_paths.new_paths);
        self.apply_build(ops, build_paths.excluded_paths, new_paths)?;
        Ok(())
    }

    fn apply_build(
        &self,
        ops: impl IntoIterator<Item = Op>,
        excluded_paths: impl IntoIterator<Item = TaggedPath>,
        new_paths: impl IntoIterator<Item = PathBuf>,
    ) -> Result<(), ApplyError> {
        for path in excluded_paths {
            remove_symlink(self.root.join(&path)).map_err(|e| RemovePathError(path.into(), e))?;
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
    use std::fs::{remove_file, File};

    use proptest::prelude::{prop::collection::vec, *};
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::testing::{
            list_files, list_tagged_paths, tagged_filesystem, tagged_filesystem_with,
        },
        testing::{create_file_and_parent, with_temp_dir, TaggedPaths},
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
                    create_file_and_parent(filesystem.root.tag(path.name(), tag));
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
                filesystem.touch([], name.clone());
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
                filesystem.touch([], name.clone());
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
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                );
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
                filesystem.rm(path.name().to_owned());
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    path.name().to_owned(),
                );
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
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    path.name().to_owned(),
                );
            }

            let expected = list_files(&filesystem.root);

            for (path, tag) in other_paths.iter().zip(&missing_tags) {
                filesystem.rm(path.name().to_owned());
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                );
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
                filesystem.mkdir(dir.tags().map(|tag| tag.to_owned()), dir.name().to_owned());
            }
            for (dir, paths) in read_paths(filesystem.root.files()).unwrap().zip(dirs_paths) {
                let filesystem = TaggedFilesystem::init(dir).unwrap();
                for path in paths {
                    filesystem.touch(
                        path.tags().map(|tag| tag.to_owned()),
                        path.name().to_owned(),
                    );
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
            let filesystem = tagged_filesystem_with(dir, ["a-_foo"]);
            File::create(filesystem.root.join("_bar")).unwrap();
            assert!(filesystem.build().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files/foo", ".tag/tags/foo/tag/a", "_bar", "a-_foo"].map(PathBuf::from),
            )
        });

        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["a-_foo"]);
            create_dir(filesystem.root.join("_bar")).unwrap();
            assert!(filesystem.build().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files/foo", ".tag/tags/foo/tag/a", "_bar", "a-_foo"].map(PathBuf::from),
            )
        });

        // The following two cases can erroneously succeed
        // if the link is deleted and the non-link is renamed.
        // However,
        // this is difficult to check for,
        // and no data is deleted.
        // So,
        // it is considered acceptable.
        //
        // ```
        // with_temp_dir(|dir| {
        //     let filesystem = tagged_filesystem_with(dir, ["a-_foo"]);
        //     File::create(filesystem.root.join("_foo")).unwrap();
        //     assert!(filesystem.build().is_err());
        //     assert_eq!(
        //         list_files(filesystem.root),
        //         [".tag/files/foo", ".tag/tags/foo/tag/a", "_foo", "a-_foo"].map(PathBuf::from),
        //     )
        // });
        //
        // with_temp_dir(|dir| {
        //     let filesystem = tagged_filesystem_with(dir, ["a-_foo"]);
        //     create_dir(filesystem.root.join("_foo")).unwrap();
        //     assert!(filesystem.build().is_err());
        //     assert_eq!(
        //         list_files(filesystem.root),
        //         [".tag/files/foo", ".tag/tags/foo/tag/a", "_foo", "a-_foo"].map(PathBuf::from),
        //     )
        // });
        // ```
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
                create_file_and_parent(filesystem.root.join(path));
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

    #[proptest(cases = 20)]
    fn build_some_only_removes_tags_for_considered_names(
        paths: TaggedPaths,
        considered_paths: TaggedPaths,
        ignored_paths: TaggedPaths,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for path in ignored_paths {
                for tag in path.tags() {
                    create_file_and_parent(filesystem.root.tag(path.name(), tag));
                }
            }

            let expected = list_files(&filesystem.root);

            for path in &considered_paths {
                for tag in path.tags() {
                    create_file_and_parent(filesystem.root.tag(path.name(), tag));
                }
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|path| path.name().to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_adds_a_tags_directory_for_considered_names(
        paths: TaggedPaths,
        considered_names: Vec<Name>,
        ignored_names: Vec<Name>,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for name in &considered_names {
                filesystem.touch([], name.clone());
            }
            for name in &ignored_names {
                filesystem.touch([], name.clone());
                remove_dir_all(filesystem.root.file_tags(name)).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for name in &considered_names {
                remove_dir_all(filesystem.root.file_tags(name)).unwrap();
            }
            filesystem.build_some(considered_names).unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_adds_a_program_tags_directory_for_considered_names(
        paths: TaggedPaths,
        considered_names: Vec<Name>,
        ignored_names: Vec<Name>,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for name in &considered_names {
                filesystem.touch([], name.clone());
            }
            for name in &ignored_names {
                filesystem.touch([], name.clone());
                remove_dir(filesystem.root.program_tags(name)).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for name in &considered_names {
                remove_dir(filesystem.root.program_tags(name)).unwrap();
            }
            filesystem.build_some(considered_names).unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }

    #[proptest(cases = 20)]
    fn build_some_only_removes_tagged_paths_for_considered_names(
        paths: TaggedPaths,
        considered_paths: TaggedPaths,
        ignored_paths: TaggedPaths,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);
            for path in ignored_paths {
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                );
                remove_file(filesystem.root.file(path.name())).unwrap();
                remove_dir_all(filesystem.root.file_tags(path.name())).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for path in &considered_paths {
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                );
                remove_file(filesystem.root.file(path.name())).unwrap();
                remove_dir_all(filesystem.root.file_tags(path.name())).unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|path| path.name().to_owned())
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
        paths: TaggedPaths,
        considered_paths: TaggedPaths,
        ignored_paths: TaggedPaths,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&considered_paths));
            for path in ignored_paths {
                File::create(filesystem.root.file(path.name())).unwrap();
                create_dir_all(filesystem.root.program_tags(path.name())).unwrap();
                for tag in path.tags() {
                    File::create(filesystem.root.tag(path.name(), tag)).unwrap();
                }
            }

            let expected = list_files(&filesystem.root);

            for path in &considered_paths {
                let name = path.name().to_owned();
                remove_file(
                    filesystem.root.join(
                        filesystem
                            .filtered_tagged_paths(move |x| x.name() == name.as_ref())
                            .next()
                            .unwrap(),
                    ),
                )
                .unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|path| path.name().to_owned())
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
        paths: TaggedPaths,
        #[strategy(
            TaggedPaths::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        considered_paths_extra_tags: (TaggedPaths, Vec<Tag>),
        #[strategy(
            TaggedPaths::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        ignored_paths_extra_tags: (TaggedPaths, Vec<Tag>),
    ) {
        let (considered_paths, considered_extra_tags) = considered_paths_extra_tags;
        let considered_paths = considered_paths
            .into_iter()
            .zip(&considered_extra_tags)
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

        let (ignored_paths, ignored_extra_tags) = ignored_paths_extra_tags;
        let ignored_paths = ignored_paths
            .into_iter()
            .zip(&ignored_extra_tags)
            .map(|(path, tag)| {
                TaggedPath::from_tags(&path.tags().chain([tag.as_ref()]).collect(), path.name())
            })
            .collect_vec();

        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                paths.iter().chain(&considered_paths).chain(&ignored_paths),
            );
            for (path, tag) in ignored_paths.into_iter().zip(ignored_extra_tags) {
                remove_file(filesystem.root.tag(path.name(), tag)).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for (path, tag) in considered_paths.iter().zip(considered_extra_tags) {
                filesystem.rm(path.name().to_owned());
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    path.name().to_owned(),
                );
                remove_file(filesystem.root.tag(path.name(), tag)).unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|path| path.name().to_owned())
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
        paths: TaggedPaths,
        #[strategy(
            TaggedPaths::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        considered_paths_missing_tags: (TaggedPaths, Vec<Tag>),
        #[strategy(
            TaggedPaths::arbitrary().prop_flat_map(|paths| {
                let len = paths.len();
                (Just(paths), vec(Tag::arbitrary(), len))
            })
        )]
        ignored_paths_missing_tags: (TaggedPaths, Vec<Tag>),
    ) {
        let (considered_paths, considered_missing_tags) = considered_paths_missing_tags;
        let considered_paths = considered_paths
            .into_iter()
            .zip(&considered_missing_tags)
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

        let (ignored_paths, ignored_missing_tags) = ignored_paths_missing_tags;
        let ignored_paths = ignored_paths
            .into_iter()
            .zip(&ignored_missing_tags)
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
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain(&ignored_paths));
            for (path, tag) in considered_paths.iter().zip(&considered_missing_tags) {
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()).chain([tag.clone()]),
                    path.name().to_owned(),
                );
            }
            for (path, tag) in ignored_paths.iter().zip(&ignored_missing_tags) {
                File::create(filesystem.root.tag(path.name(), tag)).unwrap();
            }

            let expected = list_files(&filesystem.root);

            for (path, tag) in considered_paths.iter().zip(&considered_missing_tags) {
                filesystem.rm(path.name().to_owned());
                filesystem.touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                );
                File::create(filesystem.root.tag(path.name(), tag)).unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|path| path.name().to_owned())
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
        paths: TaggedPaths,
        considered_paths: TaggedPaths,
        ignored_paths: TaggedPaths,
    ) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                paths.iter().chain(&considered_paths).chain(&ignored_paths),
            );
            let sep = if ignored_paths.len() > 1 {
                create_dir(filesystem.root.join("foobar")).unwrap();
                "/"
            } else {
                "-"
            };
            for path in ignored_paths {
                symlink_file(
                    filesystem.root.file(path.name()),
                    filesystem.root.join(format!("foobar{sep}_{}", path.name())),
                )
                .unwrap();
            }

            let expected = list_files(&filesystem.root);

            for path in &considered_paths {
                symlink_file(
                    filesystem.root.file(path.name()),
                    filesystem.root.join(format!("foobar{sep}_{}", path.name())),
                )
                .unwrap();
            }
            filesystem
                .build_some(
                    considered_paths
                        .iter()
                        .map(|path| path.name().to_owned())
                        .collect(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected);
    }
}
