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
    combine, sort_tags_by_subfrequency, Name, NameRef, Root, TaggedPath, DIR_SEPARATOR,
    EXT_SEPARATOR, INLINE_SEPARATOR,
};

#[derive(Parser)]
#[command(author, version, about = "A script to create a view of tagged paths", long_about = None)]
struct Args {
    names: Vec<Name>,
}

fn main() {
    let args = Args::parse();

    let root = Root::new(current_dir().unwrap()).unwrap().unwrap();

    let names = if args.names.is_empty() {
        read_dir(root.files())
            .unwrap()
            .map(|path| Name::new(path.unwrap().file_name().into_string().unwrap()).unwrap())
            .collect()
    } else {
        args.names
    };

    let tags = names.iter().map(|name| tags(&root, name));
    let exts = names
        .iter()
        .map(|name| name.as_str().split_once(EXT_SEPARATOR).map_or("", |x| x.1));

    let targets = names.iter().map(|name| root.file(name));
    let tmp = PathBuf::from_str(
        String::from_utf8(Command::new("mktemp").arg("-d").output().unwrap().stdout)
            .unwrap()
            .trim(),
    )
    .unwrap();
    let paths = tags
        .zip(exts)
        .map(|(mut tags, ext)| format!("{}{EXT_SEPARATOR}{ext}", tags.join("-")))
        .map(|path| TaggedPath::new(path).unwrap())
        .collect::<Vec<_>>();
    let paths = sort_tags_by_subfrequency(&paths).collect::<Vec<_>>();
    let paths = combine(&paths).map(|path| tmp.join(path));
    for (target, path) in targets.zip(paths) {
        create_dir_all(path.parent().unwrap()).unwrap();
        symlink(target, path).unwrap();
    }

    println!("{}", tmp.display());
}

fn tags<'a>(root: &'a Root, name: &'a NameRef) -> impl Iterator<Item = String> + 'a {
    with_implied_tags(
        root,
        name_tags(name)
            .map(|s| s.to_owned())
            .chain(hashtag_tags(root, name))
            .chain(legacy_tags(root, name)),
    )
}

fn name_tags(name: &NameRef) -> impl Iterator<Item = &str> {
    name.as_str()
        .split_once(EXT_SEPARATOR)
        .map_or(Either::Right(empty()), |x| {
            Either::Left(x.0.split([INLINE_SEPARATOR, DIR_SEPARATOR]))
        })
}

fn hashtag_tags(root: &Root, name: &NameRef) -> impl Iterator<Item = String> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"(?:^| )#([0-9A-Za-z][0-9A-Za-z_]*)").unwrap());
    if name.as_str().ends_with(".md") {
        let s = String::from_utf8(read(root.file(name)).unwrap()).unwrap();
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

fn legacy_tags(root: &Root, name: &NameRef) -> impl Iterator<Item = String> {
    tags_from_dir(root.metadata().join("legacy").join(name.as_path()))
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
