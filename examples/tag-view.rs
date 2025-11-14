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
use itertools::{Either, Itertools};
use regex::Regex;
use rustc_hash::FxHashSet;
use tag::{
    combine, sort_tags_by_subfrequency, Root, TaggedFilesystemBuilder, TaggedPath, DIR_SEPARATOR,
    EXT_SEPARATOR, INLINE_SEPARATOR,
};

#[derive(Parser)]
#[command(author, version, about = "A script to create a view of tagged paths", long_about = None)]
struct Args {
    paths: Vec<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let root = Root::new(current_dir().unwrap()).unwrap().unwrap();

    let paths = if args.paths.is_empty() {
        TaggedFilesystemBuilder::new(current_dir().unwrap())
            .build()
            .unwrap()
            .unwrap()
            .find(Vec::new(), Vec::new())
            .unwrap()
            .map(|path| path.into_path())
            .collect()
    } else {
        args.paths
    };

    let tags = paths.iter().map(|path| tags(&root, path));
    let exts = paths.iter().map(|path| {
        path.to_str()
            .unwrap()
            .split_once(EXT_SEPARATOR)
            .map_or("", |x| x.1)
    });

    let targets = paths.iter().map(|path| path.canonicalize().unwrap());
    let tmp = PathBuf::from_str(
        String::from_utf8(Command::new("mktemp").arg("-d").output().unwrap().stdout)
            .unwrap()
            .trim(),
    )
    .unwrap();
    let links = tags
        .zip(exts)
        .map(|(mut tags, ext)| format!("{}{EXT_SEPARATOR}{ext}", tags.join("-")))
        .map(|path| TaggedPath::new(path).unwrap())
        .collect::<Vec<_>>();
    let links = sort_tags_by_subfrequency(&links).collect::<Vec<_>>();
    let links = combine(&links).map(|path| tmp.join(path));
    for (target, link) in targets.zip(links) {
        create_dir_all(link.parent().unwrap()).unwrap();
        symlink(target, link).unwrap();
    }

    println!("{}", tmp.display());
}

fn tags<'a>(root: &'a Root, path: &'a Path) -> impl Iterator<Item = String> + 'a {
    with_implied_tags(
        root,
        path_tags(path)
            .map(|s| s.to_owned())
            .chain(hashtag_tags(path))
            .chain(legacy_tags(root, path)),
    )
}

fn path_tags(path: &Path) -> impl Iterator<Item = &str> {
    path.to_str()
        .unwrap()
        .split_once(EXT_SEPARATOR)
        .map_or(Either::Right(empty()), |x| {
            Either::Left(x.0.split([INLINE_SEPARATOR, DIR_SEPARATOR]))
        })
}

fn hashtag_tags(path: &Path) -> impl Iterator<Item = String> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"(?:^| )#([0-9A-Za-z][0-9A-Za-z_]*)").unwrap());
    if path.extension().is_some_and(|s| s == "md") {
        let s = String::from_utf8(read(path).unwrap()).unwrap();
        Either::Left(
            RE.find_iter(&s)
                .map(|m| m.as_str().to_owned())
                .collect::<Vec<_>>()
                .into_iter(),
        )
    } else {
        Either::Right(empty())
    }
}

fn legacy_tags(root: &Root, path: &Path) -> impl Iterator<Item = String> {
    tags_from_dir(root.metadata().join("legacy").join(path))
}

fn with_implied_tags(
    root: &Root,
    tags: impl IntoIterator<Item = String>,
) -> impl Iterator<Item = String> {
    let mut new_tags = tags.into_iter().collect::<FxHashSet<_>>();
    let mut tags = FxHashSet::default();
    while !new_tags.is_empty() {
        let newest_tags = new_tags
            .iter()
            .flat_map(|tag| tags_from_dir(root.metadata().join("implied").join(tag)))
            .collect::<FxHashSet<_>>();
        tags.extend(mem::take(&mut new_tags));
        new_tags = newest_tags
            .into_iter()
            .filter(|tag| !tags.contains(tag))
            .collect();
    }
    tags.into_iter()
}

fn tags_from_dir<P>(path: P) -> impl Iterator<Item = String>
where
    P: AsRef<Path>,
{
    match read_dir(path) {
        Ok(dir) => Either::Left(dir.into_iter().map(|x| {
            x.unwrap()
                .path()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned()
        })),
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => Either::Right(empty()),
            _ => panic!("{e}"),
        },
    }
}
