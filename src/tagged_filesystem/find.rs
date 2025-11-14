use super::*;

impl TaggedFilesystem {
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
}

#[cfg(test)]
mod tests {
    use crate::{
        tagged_filesystem::testing::tagged_filesystem_with,
        testing::{tag, with_temp_dir},
    };

    use super::*;

    #[test]
    fn find_returns_paths_with_tag() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo.x", "bar.x"]);
            assert_eq!(
                filesystem
                    .find(vec![tag("foo")], vec![])
                    .unwrap()
                    .collect_vec(),
                [TaggedPath::new("foo.x").unwrap()]
            );
        })
    }

    #[test]
    fn find_returns_paths_with_all_tags() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo/bar.x", "foo.x"]);
            assert_eq!(
                filesystem
                    .find(vec![tag("foo"), tag("bar")], vec![])
                    .unwrap()
                    .collect_vec(),
                [TaggedPath::new("foo/bar.x").unwrap()]
            );
        })
    }

    #[test]
    fn find_does_not_return_paths_with_excluded_tags() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo/bar.x", "foo/_.x"]);
            assert_eq!(
                filesystem
                    .find(vec![tag("foo"),], vec![tag("bar")],)
                    .unwrap()
                    .collect_vec(),
                [TaggedPath::new("foo/_.x").unwrap()]
            );
        })
    }
}
