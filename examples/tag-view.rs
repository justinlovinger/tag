use core::panic;
use std::{
    env::current_dir,
    fs::{create_dir_all, read, read_dir},
    iter::empty,
    mem,
    os::unix::fs::symlink,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
    sync::LazyLock,
};

use clap::Parser;
use itertools::Either;
use rayon::prelude::*;
use regex::Regex;
use rustc_hash::FxHashSet;
use tag::{
    combine, find, sort_tags_by_subfrequency, ExtRef, Root, Tag, TagRef, TaggedPath, TaggedPathRef,
};

#[derive(Parser)]
#[command(author, version, about = "Create a view of tagged paths", long_about = None)]
struct Args {
    /// Directory to create the view in
    ///
    /// The directory is not cleared automatically.
    /// Existing files may conflict.
    #[arg(long, short)]
    dir: Option<PathBuf>,

    /// Paths to create a view of
    paths: Vec<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let cwd = current_dir().unwrap();
    let root = Root::new(&cwd).unwrap().unwrap();

    let dir = args.dir.unwrap_or_else(|| {
        PathBuf::from_str(
            String::from_utf8(Command::new("mktemp").arg("-d").output().unwrap().stdout)
                .unwrap()
                .trim(),
        )
        .unwrap()
    });
    create_dir_all(&dir).unwrap();

    let paths = if args.paths.is_empty() {
        find(cwd, Vec::new(), Vec::new())
            .unwrap()
            .map(|path| path.into_path())
            .collect()
    } else {
        args.paths
    };

    let tagged_paths = {
        let mut tagged_paths = Vec::new();
        paths
            .par_iter()
            .map(|path| {
                TaggedPath::from_tags(
                    tags(&root, path),
                    TaggedPathRef::new(path.to_str().unwrap()).map_or(ExtRef::empty(), |x| x.ext()),
                )
            })
            .collect_into_vec(&mut tagged_paths);
        tagged_paths
    };

    paths
        .par_iter()
        .zip(combine(&sort_tags_by_subfrequency(&tagged_paths)))
        .for_each(|(path, link)| {
            let link = dir.join(link);
            create_dir_all(link.parent().unwrap()).unwrap();
            symlink(path.canonicalize().unwrap(), link).unwrap();
        });

    println!("{}", dir.display());
}

fn tags<'a>(root: &'a Root, path: &'a Path) -> impl Iterator<Item = Tag> + 'a {
    with_implied_tags(
        root,
        path_tags(path)
            .map(|s| s.to_owned())
            .chain(hashtag_tags(path))
            .chain(legacy_tags(root, path)),
    )
}

fn path_tags(path: &Path) -> impl Iterator<Item = &TagRef> {
    TaggedPathRef::new(path.to_str().unwrap())
        .map_or(Either::Right(empty()), |x| Either::Left(x.tags()))
}

fn hashtag_tags(path: &Path) -> impl Iterator<Item = Tag> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"(?:^| )#([0-9A-Za-z][0-9A-Za-z_]*)").unwrap());
    if path.extension().is_some_and(|s| s == "md") {
        let s = String::from_utf8(read(path).unwrap()).unwrap();
        Either::Left(
            RE.find_iter(&s)
                .map(|m| Tag::new(m.as_str()).unwrap())
                .collect::<Vec<_>>()
                .into_iter(),
        )
    } else {
        Either::Right(empty())
    }
}

fn legacy_tags(root: &Root, path: &Path) -> impl Iterator<Item = Tag> {
    tags_from_dir(root.metadata().join("legacy").join(path))
}

fn with_implied_tags(
    root: &Root,
    tags: impl IntoIterator<Item = Tag>,
) -> impl Iterator<Item = Tag> {
    let mut new_tags = tags.into_iter().collect::<FxHashSet<_>>();
    let mut tags = FxHashSet::default();
    while !new_tags.is_empty() {
        let newest_tags = new_tags
            .iter()
            .flat_map(|tag| tags_from_dir(root.metadata().join("implied").join(tag.as_path())))
            .collect::<FxHashSet<_>>();
        tags.extend(mem::take(&mut new_tags));
        new_tags = newest_tags
            .into_iter()
            .filter(|tag| !tags.contains(tag))
            .collect();
    }
    tags.into_iter()
}

fn tags_from_dir<P>(path: P) -> impl Iterator<Item = Tag>
where
    P: AsRef<Path>,
{
    match read_dir(path) {
        Ok(dir) => {
            Either::Left(dir.into_iter().map(|x| {
                Tag::new(x.unwrap().path().file_name().unwrap().to_str().unwrap()).unwrap()
            }))
        }
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => Either::Right(empty()),
            _ => panic!("{e}"),
        },
    }
}
