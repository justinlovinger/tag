use std::{
    path::{Path, PathBuf},
    sync::mpsc,
};

use rustc_hash::FxHashSet;

use crate::{Tag, TaggedPath, METADATA_DIR};

pub fn find<P>(
    dir: P,
    include: Vec<Tag>,
    exclude: Vec<Tag>,
) -> std::io::Result<impl Iterator<Item = TaggedPath>>
where
    P: AsRef<Path>,
{
    Ok(if include.is_empty() && exclude.is_empty() {
        tagged_paths(dir)
    } else {
        filtered_tagged_paths(dir, move |path| {
            let tags = path.tags().collect::<FxHashSet<_>>();
            include.iter().all(|tag| tags.contains(&tag.as_ref()))
                && !exclude.iter().any(|tag| tags.contains(&tag.as_ref()))
        })
    })
}

fn tagged_paths<P>(dir: P) -> mpsc::IntoIter<TaggedPath>
where
    P: AsRef<Path>,
{
    let (sender, receiver) = mpsc::channel();
    for_each_tagged_path(dir, move |path| sender.send(path).unwrap());
    receiver.into_iter()
}

fn filtered_tagged_paths<P>(
    dir: P,
    predicate: impl Fn(&TaggedPath) -> bool + Send + Sync + 'static,
) -> mpsc::IntoIter<TaggedPath>
where
    P: AsRef<Path>,
{
    let (sender, receiver) = mpsc::channel();
    for_each_tagged_path(dir, move |path| {
        if predicate(&path) {
            sender.send(path).unwrap()
        }
    });
    receiver.into_iter()
}

fn for_each_tagged_path<P, C>(dir: P, callback: C)
where
    P: AsRef<Path>,
    C: Fn(TaggedPath) + Send + Sync + 'static,
{
    fn for_each_tagged_path_inner<'scope, C>(
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
                        for_each_tagged_path_inner(
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

    let root = dir.as_ref().to_owned();
    rayon::spawn(move || {
        rayon::scope(|s| {
            for_each_tagged_path_inner(
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
    use crate::testing::{create_files_relative_to, tag, with_temp_dir};

    use super::*;

    #[test]
    fn find_returns_paths_with_tag() {
        with_temp_dir(|dir| {
            create_files_relative_to(dir, ["foo.x", "bar.x"]);
            assert_eq!(
                find(dir, vec![tag("foo")], vec![])
                    .unwrap()
                    .collect::<Vec<_>>(),
                [TaggedPath::new("foo.x").unwrap()]
            );
        })
    }

    #[test]
    fn find_returns_paths_with_all_tags() {
        with_temp_dir(|dir| {
            create_files_relative_to(dir, ["foo/bar.x", "foo.x"]);
            assert_eq!(
                find(dir, vec![tag("foo"), tag("bar")], vec![])
                    .unwrap()
                    .collect::<Vec<_>>(),
                [TaggedPath::new("foo/bar.x").unwrap()]
            );
        })
    }

    #[test]
    fn find_does_not_return_paths_with_excluded_tags() {
        with_temp_dir(|dir| {
            create_files_relative_to(dir, ["foo/bar.x", "foo/_.x"]);
            assert_eq!(
                find(dir, vec![tag("foo")], vec![tag("bar")],)
                    .unwrap()
                    .collect::<Vec<_>>(),
                [TaggedPath::new("foo/_.x").unwrap()]
            );
        })
    }

    #[test]
    fn find_ignores_metadata() {
        with_temp_dir(|dir| {
            create_files_relative_to(dir, ["foo.x", "bar.x", ".tag/foo.x"]);
            assert_eq!(
                find(dir, vec![tag("foo")], vec![])
                    .unwrap()
                    .collect::<Vec<_>>(),
                [TaggedPath::new("foo.x").unwrap()]
            );
        })
    }
}
