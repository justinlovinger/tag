use std::fs::create_dir;

use crate::testing::create_file_and_parent;

use super::*;

pub fn tagged_filesystem<P>(dir: P) -> TaggedFilesystem
where
    P: AsRef<Path>,
{
    create_dir(dir.as_ref().join(METADATA_DIR)).unwrap();
    TaggedFilesystemBuilder::new(dir.as_ref().to_owned())
        .build()
        .unwrap()
        .unwrap()
}

pub fn tagged_filesystem_with<P, Q>(dir: P, paths: impl IntoIterator<Item = Q>) -> TaggedFilesystem
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    let filesystem = tagged_filesystem(dir);
    for path in paths.into_iter() {
        create_file_and_parent(filesystem.root.as_path().join(&path));
    }
    filesystem
}
