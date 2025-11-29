use std::{
    collections::BTreeSet,
    env::{current_dir, set_current_dir},
    fs::{create_dir_all, remove_dir, rename},
    path::{Path, PathBuf},
};

use clap::{CommandFactory, Parser};
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};
use tag::{combine, find, sort_tags_by_subfrequency, uncombine};

#[derive(Parser)]
#[command(author, version, about = "Organize tagged paths in-place", long_about = None)]
struct Args {
    /// Directory to organize
    dir: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let dir = args.dir.unwrap_or_else(|| current_dir().unwrap());

    let old_paths = find(&dir, Vec::new(), Vec::new())
        .unwrap()
        .collect::<Vec<_>>();
    let new_paths = combine(&sort_tags_by_subfrequency(&uncombine(&old_paths)));

    set_current_dir(dir).unwrap();
    move_batch(
        old_paths.into_iter().map(|path| path.into_path()),
        new_paths,
    );
}

fn move_batch(from: impl IntoIterator<Item = PathBuf>, to: impl IntoIterator<Item = PathBuf>) {
    // We can assume each source exists
    // becaue it was found using `find`.

    // We can assume each destination is unique
    // because `to` was build using `combine`.

    let moves = from
        .into_iter()
        .zip(to)
        .filter(|(from, to)| from != to)
        .collect::<FxHashMap<_, _>>();

    let conflicts = moves
        .par_iter()
        .map(|(_, to)| to)
        .filter(|to| moves.contains_key(*to))
        .collect::<FxHashSet<_>>();

    rayon::join(
        || {
            moves.par_iter().for_each(|(from, to)| {
                if !conflicts.contains(to) && to.exists() {
                    Args::command()
                        .error(
                            clap::error::ErrorKind::Io,
                            format!(
                                "Destination `{}` for file `{}` already exists.",
                                to.display(),
                                from.display()
                            ),
                        )
                        .exit()
                }
            });
        },
        || {
            conflicts.par_iter().for_each(|from| {
                let tmp = tmp(from);
                if tmp.exists() {
                    Args::command()
                        .error(
                            clap::error::ErrorKind::Io,
                            format!(
                                "Temp file `{}` for file `{}` already exists.",
                                tmp.display(),
                                from.display()
                            ),
                        )
                        .exit()
                }
            });
        },
    );

    {
        let parents = moves
            .par_iter()
            .filter_map(|(_, to)| to.parent())
            .collect::<FxHashSet<_>>();
        parents
            .into_par_iter()
            .for_each(|path| create_dir_all(path).unwrap());
    }

    conflicts
        .par_iter()
        .for_each(|from| rename(from, tmp(from)).unwrap());
    moves.par_iter().for_each(|(from, to)| {
        if conflicts.contains(from) {
            rename(tmp(from), to).unwrap()
        } else {
            rename(from, to).unwrap()
        }
    });

    {
        let ancestors = moves
            .par_iter()
            .flat_map_iter(|(from, _)| {
                from.ancestors()
                    .skip(1)
                    .filter(|path| *path != PathBuf::new())
            })
            .collect::<BTreeSet<_>>();
        ancestors
            .into_iter()
            .rev()
            .for_each(|path| match remove_dir(path) {
                Ok(_) => {}
                Err(e) => match e.kind() {
                    std::io::ErrorKind::DirectoryNotEmpty => {}
                    _ => panic!("{e}"),
                },
            });
    }
}

fn tmp<P>(path: P) -> PathBuf
where
    P: AsRef<Path>,
{
    path.as_ref().with_file_name(
        path.as_ref()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned()
            + ".move_tmp",
    )
}
