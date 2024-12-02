use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum ModError {
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn add(&self, tag: Tag, names: FxHashSet<Name>) -> Result<Vec<PathBuf>, ModError> {
        self.r#mod([tag].into_iter().collect(), FxHashSet::default(), names)
    }

    pub fn del(&self, tag: Tag, names: FxHashSet<Name>) -> Result<Vec<PathBuf>, ModError> {
        self.r#mod(FxHashSet::default(), [tag].into_iter().collect(), names)
    }

    pub fn r#mod(
        &self,
        add: FxHashSet<Tag>,
        del: FxHashSet<Tag>,
        names: FxHashSet<Name>,
    ) -> Result<Vec<PathBuf>, ModError> {
        if add.is_empty() && del.is_empty() {
            return Ok(vec![]);
        }

        for name in &names {
            for tag in &del {
                if let Err(e) = remove_file(self.root.tag(name, tag)) {
                    // The tag not existing is fine.
                    // The file not existing is _not_.
                    if !self.root.program_tags(name).try_exists()? {
                        Err(e)?;
                    }
                }
            }
            for tag in &add {
                File::create(self.root.tag(name, tag))?;
            }
        }

        let (paths, other_paths) = self
            .tagged_paths()
            .partition::<Vec<_>, _>(|path| names.contains(path.name()));
        let mut relevant_paths = relevant_paths(
            paths
                .iter()
                .flat_map(|path| path.tags().map(|tag| tag.to_owned()))
                .chain(add.iter().cloned())
                .collect(),
            other_paths,
        );

        let mut mod_move_ops = Vec::new();
        for path in paths {
            let new_path = TaggedPath::from_tags(
                &path
                    .tags()
                    .filter(|tag| !del.contains(*tag))
                    .chain(add.iter().map(|tag| tag.as_ref()))
                    .collect(),
                path.name(),
            );

            mod_move_ops.push(MoveOp {
                from: path.into_path(),
                to: new_path.as_path().to_owned(),
            });
            relevant_paths.push(new_path);
        }

        let mut organized_move_ops = organize(&relevant_paths);
        let mut new_paths = Vec::new();
        for op in mod_move_ops {
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
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{
            name, tag, tagged_filesystem, tagged_filesystem_with, with_tempdir, TaggedPaths,
        },
        Tag,
    };

    use super::*;

    #[test]
    fn mod_adds_a_tag() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_baz"]);
            filesystem
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [].into_iter().collect(),
                    [name("baz")].into_iter().collect(),
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
            let filesystem = tagged_filesystem_with(["foo-_baz"]);
            filesystem
                .r#mod(
                    [].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
                    [name("baz")].into_iter().collect(),
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
            let filesystem = tagged_filesystem_with(["foo-_baz"]);
            filesystem
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
                    [name("baz")].into_iter().collect(),
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
            let filesystem = tagged_filesystem_with(["foo/_bar", "foo/_foo"]);
            filesystem
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
                    [name("bar"), name("foo")].into_iter().collect(),
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
            let filesystem = tagged_filesystem_with(["foo-_baz"]);
            filesystem
                .r#mod(
                    [tag("bar"), tag("baz")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
                    [name("baz")].into_iter().collect(),
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
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo"), tag("baz")].into_iter().collect(),
                    [name("baz")].into_iter().collect(),
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
                .r#mod(
                    [].into_iter().collect(),
                    [].into_iter().collect(),
                    [path.name().to_owned()].into_iter().collect(),
                )
                .unwrap();
            let actual = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[proptest(cases = 20)]
    fn mod_builds(
        paths: TaggedPaths,
        paths_to_mod: TaggedPaths,
        tags_to_add: HashSet<Tag>,
        tags_to_del: HashSet<Tag>,
    ) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain(&paths_to_mod));

            filesystem
                .r#mod(
                    tags_to_add.into_iter().collect(),
                    tags_to_del.into_iter().collect(),
                    paths_to_mod
                        .into_iter()
                        .map(|path| path.name().to_owned())
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

    #[proptest(cases = 20)]
    fn mod_is_idempotent(
        paths: TaggedPaths,
        paths_to_mod: TaggedPaths,
        tags_to_add: HashSet<Tag>,
        tags_to_del: HashSet<Tag>,
    ) {
        let tags_to_add = tags_to_add.into_iter().collect::<FxHashSet<_>>();
        let tags_to_del = tags_to_del.into_iter().collect::<FxHashSet<_>>();
        let names_to_mod = paths_to_mod
            .iter()
            .map(|path| path.name().to_owned())
            .collect::<FxHashSet<_>>();

        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain(&paths_to_mod));

            filesystem
                .r#mod(
                    tags_to_add.clone(),
                    tags_to_del.clone(),
                    names_to_mod.clone(),
                )
                .unwrap();
            let expected = list_files(&filesystem.root);

            filesystem
                .r#mod(tags_to_add, tags_to_del, names_to_mod)
                .unwrap();
            let actual = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn mod_returns_error_if_file_does_not_exist() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
                    [name("baz")].into_iter().collect()
                )
                .is_err());
        })
    }
}
