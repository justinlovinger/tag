use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum ModError {
    HasTag(#[from] HasTagError),
    LacksTag(#[from] LacksTagError),
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn add(&self, tag: Tag, paths: FxHashSet<TaggedPath>) -> Result<Vec<PathBuf>, ModError> {
        self.r#mod([tag].into_iter().collect(), FxHashSet::default(), paths)
    }

    pub fn del(&self, tag: Tag, paths: FxHashSet<TaggedPath>) -> Result<Vec<PathBuf>, ModError> {
        self.r#mod(FxHashSet::default(), [tag].into_iter().collect(), paths)
    }

    pub fn r#mod(
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
            let new_path = TaggedPath::from_tags(
                &path
                    .tags()
                    .filter(|tag| {
                        if del.contains(*tag) {
                            deleted_count += 1;
                            false
                        } else {
                            true
                        }
                    })
                    .chain(add.iter().map(|tag| tag.as_ref()))
                    .collect(),
                path.name(),
            );
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
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{tag, tagged_filesystem, tagged_filesystem_with, with_tempdir, TaggedPaths},
        Tag,
    };

    use super::*;

    #[test]
    fn mod_adds_a_tag() {
        with_tempdir(|| {
            let path = TaggedPath::new("foo-_baz".to_owned()).unwrap();
            let filesystem = tagged_filesystem_with([&path]);
            filesystem
                .r#mod(
                    [tag("bar")].into_iter().collect(),
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
                .r#mod(
                    [].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
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
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
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
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
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
                .r#mod(
                    [tag("bar"), tag("baz")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
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
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo"), tag("baz")].into_iter().collect(),
                    filesystem
                        .find(vec![tag("foo"), tag("baz")], vec![])
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
                .r#mod(
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
                .r#mod(
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
                .r#mod(
                    [tag("foo")].into_iter().collect(),
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
                .r#mod(
                    [].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
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
                .r#mod(
                    [tag("bar")].into_iter().collect(),
                    [tag("foo")].into_iter().collect(),
                    [TaggedPath::new("foo-_baz".to_owned()).unwrap()]
                        .into_iter()
                        .collect()
                )
                .is_err());
        })
    }
}
