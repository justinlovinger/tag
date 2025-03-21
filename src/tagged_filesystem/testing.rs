use std::fs::{remove_dir_all, remove_file, File};

use crate::testing::create_file_and_parent;

use super::*;

pub fn tagged_filesystem<P>(dir: P) -> TaggedFilesystem
where
    P: AsRef<Path>,
{
    TaggedFilesystem::init(dir).unwrap()
}

pub fn tagged_filesystem_with<P, Q>(dir: P, paths: impl IntoIterator<Item = Q>) -> TaggedFilesystem
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    let filesystem = TaggedFilesystem::init(dir).unwrap();
    for path in paths.into_iter() {
        let tagged_path = TaggedPath::from_path(path.as_ref().to_owned()).unwrap();
        filesystem.touch(
            tagged_path.tags().map(|tag| tag.to_owned()),
            tagged_path.name().to_owned(),
        );
    }
    filesystem
}

impl TaggedFilesystem {
    pub fn touch(&self, tags: impl IntoIterator<Item = Tag>, name: Name) {
        create_dir_all(self.root.program_tags(&name)).unwrap();
        for tag in tags {
            create_file_and_parent(self.root.tag(&name, tag))
        }

        File::create(self.root.file(&name)).unwrap();

        self.build_some(vec![name]).unwrap();
    }

    pub fn mkdir(&self, tags: impl IntoIterator<Item = Tag>, name: Name) {
        create_dir_all(self.root.program_tags(&name)).unwrap();
        for tag in tags {
            create_file_and_parent(self.root.tag(&name, tag))
        }

        create_dir(self.root.file(&name)).unwrap();

        self.build_some(vec![name]).unwrap();
    }

    pub fn rm(&self, name: Name) {
        let file_path = self.root.file(&name);
        if file_path.is_dir() {
            remove_dir_all(file_path).unwrap();
        } else {
            remove_file(file_path).unwrap();
        }

        let _ = remove_dir_all(self.root.file_tags(&name));

        self.build_some(vec![name]).unwrap();
    }
}

pub fn list_tagged_paths<P>(path: P) -> Vec<PathBuf>
where
    P: AsRef<Path>,
{
    list_files_(
        path.as_ref(),
        read_paths(path.as_ref())
            .unwrap()
            .filter(|path| !path.ends_with(METADATA_DIR))
            .collect(),
    )
}

pub fn list_files<P>(path: P) -> Vec<PathBuf>
where
    P: AsRef<Path>,
{
    list_files_(path.as_ref(), read_paths(path.as_ref()).unwrap().collect())
}

fn list_files_(root: &Path, mut queue: Vec<PathBuf>) -> Vec<PathBuf> {
    let mut files = Vec::new();
    while let Some(file) = queue.pop() {
        if file.is_dir() && std::fs::read_dir(&file).unwrap().next().is_some() {
            queue.extend(read_paths(file).unwrap());
        } else {
            files.push(file);
        }
    }
    for file in &mut files {
        *file = file.strip_prefix(root).unwrap().to_owned();
    }
    files.sort();
    files
}
