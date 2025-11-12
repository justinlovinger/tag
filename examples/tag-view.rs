use std::{
    env::current_dir,
    fs::{create_dir_all, read_dir},
    io::Write,
    os::unix::fs::symlink,
    path::PathBuf,
    process::{Command, Stdio},
    str::FromStr,
    thread::scope,
};

use clap::Parser;
use itertools::Itertools;
use tag::{combine, sort_tags_by_subfrequency, Name, Root, TaggedPath};

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

    let tags = {
        let mut tags_script = Command::new(root.tags())
            .current_dir(&root)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        let mut stdin = tags_script.stdin.take().unwrap();
        scope(|s| {
            s.spawn(|| {
                writeln!(stdin, "{}", names.iter().format("\n")).unwrap();
                drop(stdin); // Scoped threads don't end on their own, so we need to explicitly drop stdin.
            });
            String::from_utf8(tags_script.wait_with_output().unwrap().stdout).unwrap()
        })
    };
    let tags = tags.split("\n\n").map(|s| s.split("\n"));

    let exts = names
        .iter()
        .map(|name| name.as_str().split_once(".").unwrap().1);

    let targets = names.iter().map(|name| root.file(name));
    let tmp = PathBuf::from_str(
        String::from_utf8(Command::new("mktemp").arg("-d").output().unwrap().stdout)
            .unwrap()
            .trim(),
    )
    .unwrap();
    let paths = tags
        .zip(exts)
        .map(|(mut tags, ext)| format!("{}.{ext}", tags.join("-")))
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
