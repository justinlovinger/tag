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

        for name in &tag_names {
            if !names.contains(name) {
                remove_dir_all(self.root.file_tags(name))?;
            }
        }

        for name in &names {
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
                    new_paths.push(TaggedPath::from_tags(&self.tags(name)?, name));
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
                    *path = TaggedPath::from_tags(&tags, path.name());
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
}

#[cfg(test)]
mod tests {
    use std::{env::set_current_dir, fs::File};

    use proptest::prelude::{prop::collection::vec, *};
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::{list_files, list_tagged_paths},
        testing::{
            make_file_and_parent, tagged_filesystem, tagged_filesystem_with, with_tempdir,
            TaggedPaths,
        },
        Tag,
    };

    use super::*;

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
            &path
                .tags()
                .filter(|path_tag| path_tag != &tag.as_ref())
                .collect(),
            path.name(),
        );

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
            &path
                .tags()
                .filter(|path_tag| path_tag != &tag.as_ref())
                .collect(),
            path.name(),
        );

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
}
