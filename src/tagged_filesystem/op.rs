use std::collections::BTreeSet;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Op {
    EnsureDirectory(PathBuf),
    Move(MoveOp),
    DeleteDirectoryIfEmpty(PathBuf),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct MoveOp {
    pub from: PathBuf,
    pub to: PathBuf,
}

impl From<MoveOp> for Op {
    fn from(value: MoveOp) -> Self {
        Op::Move(value)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}, on {1:?}")]
pub struct OpError(std::io::Error, Op);

pub(crate) fn from_move_ops(ops: Vec<MoveOp>) -> impl Iterator<Item = Op> {
    // We know a directory exists
    // if a file is moving from it.
    let from_ancestors = FxHashSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from, to: _ }| from.ancestors().skip(1)),
    );
    let ensure_dirs = FxHashSet::from_iter(
        ops.iter()
            .filter_map(|MoveOp { from: _, to }| to.parent())
            .filter(|to| !from_ancestors.contains(to)),
    );
    // We do not need to ensure a parent exists
    // if its child is already being ensured.
    let ensure_dir_ancestors =
        FxHashSet::from_iter(ensure_dirs.iter().flat_map(|path| path.ancestors().skip(1)));
    // Order matters.
    let ensure_dirs = BTreeSet::from_iter(
        ensure_dirs
            .into_iter()
            .filter(|to| !ensure_dir_ancestors.contains(to))
            .map(|x| x.to_owned()),
    );

    // We know a directory will not be empty
    // if a file is moving to it.
    let to_ancestors = FxHashSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from: _, to }| to.ancestors().skip(1)),
    );
    // Order matters.
    let del_dirs = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from, to: _ }| {
                // If a child will not be empty,
                // neither will its parents.
                from.ancestors()
                    .skip(1)
                    .take_while(|from| !to_ancestors.contains(from))
            })
            .map(|x| x.to_owned()),
    );

    ensure_dirs
        .into_iter()
        .map(Op::EnsureDirectory)
        .chain(ops.into_iter().map_into())
        // `rev()` to check child directories before parents.
        .chain(del_dirs.into_iter().rev().map(Op::DeleteDirectoryIfEmpty))
}

impl TaggedFilesystem {
    pub(crate) fn apply_all(&self, ops: impl IntoIterator<Item = Op>) -> Result<(), OpError> {
        let ops = self.canonicalize_all(ops).collect_vec();

        if let Err(e) = self.apply_all_(&ops) {
            ops.into_iter()
                .filter_map(|op| match op {
                    Op::EnsureDirectory(path) => Some(Op::DeleteDirectoryIfEmpty(path)),
                    Op::Move(MoveOp { .. }) => None,
                    Op::DeleteDirectoryIfEmpty(path) => Some(Op::DeleteDirectoryIfEmpty(path)),
                })
                .for_each(|op| {
                    if let Err(e) = self.apply_(&op) {
                        eprintln!("{e}");
                    }
                });
            Err(e)
        } else {
            Ok(())
        }
    }

    fn canonicalize_all<'a>(
        &'a self,
        ops: impl IntoIterator<Item = Op> + 'a,
    ) -> impl Iterator<Item = Op> + 'a {
        ops.into_iter().map(|op| self.canonicalize(op))
    }

    fn canonicalize(&self, op: Op) -> Op {
        match op {
            Op::EnsureDirectory(path) => Op::EnsureDirectory(self.root.join(path)),
            Op::Move(MoveOp { from, to }) => Op::Move(MoveOp {
                from: self.root.join(from),
                to: self.root.join(to),
            }),
            Op::DeleteDirectoryIfEmpty(path) => Op::DeleteDirectoryIfEmpty(self.root.join(path)),
        }
    }

    fn apply_all_<'a>(&self, ops: impl IntoIterator<Item = &'a Op>) -> Result<(), OpError> {
        for op in ops.into_iter() {
            self.apply_(op)?
        }
        Ok(())
    }

    fn apply_(&self, op: &Op) -> Result<(), OpError> {
        (match op {
            Op::EnsureDirectory(path) => create_dir_all(path),
            Op::Move(MoveOp { from, to }) => {
                // This utility should only organize data,
                // never delete it.
                if to.try_exists().map_err(|e| OpError(e, op.clone()))? {
                    if to == from {
                        Ok(())
                    } else {
                        Err(std::io::Error::new(
                            std::io::ErrorKind::AlreadyExists,
                            format!(
                                "cannot move `{}` to `{}`, destination already exists",
                                from.display(),
                                to.display()
                            ),
                        ))
                    }
                } else {
                    rename(from, to)
                }
            }
            Op::DeleteDirectoryIfEmpty(path) => {
                // Note,
                // we can do the following with nightly Rust,
                // which may be more efficient than `self.fs.read_dir(&path)?.next().is_none()`:
                // ```
                // if let Err(e) = self.fs.remove_dir(path) {
                //     if e.kind() != std::io::ErrorKind::DirectoryNotEmpty {
                //         return Err(e);
                //     }
                // }
                // ```
                if std::fs::read_dir(path)
                    .map_err(|e| OpError(e, op.clone()))?
                    .next()
                    .is_none()
                {
                    remove_dir(path)
                } else {
                    Ok(())
                }
            }
        })
        .map_err(|e| OpError(e, op.clone()))
    }
}
