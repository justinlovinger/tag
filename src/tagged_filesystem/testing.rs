use std::fs::{remove_dir_all, remove_file, File};

use crate::TagRef;

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
    let filesystem = tagged_filesystem(dir);
    create_dir_all(filesystem.root.metadata().join("tags")).unwrap();
    for path in paths.into_iter() {
        let tagged_path = TaggedPath::from_path(path.as_ref().to_owned()).unwrap();
        filesystem.touch(
            tagged_path.tags().map(|tag| tag.to_owned()),
            tagged_path.name(),
        );
        filesystem.symlink_file(&tagged_path);
    }
    filesystem
}

impl TaggedFilesystem {
    pub fn touch<T, N>(&self, tags: impl IntoIterator<Item = T>, name: N)
    where
        T: AsRef<TagRef>,
        N: AsRef<NameRef>,
    {
        self.add_tags(&name, tags);
        File::create(self.root.file(&name)).unwrap();
    }

    pub fn mkdir<T, N>(&self, tags: impl IntoIterator<Item = T>, name: N)
    where
        T: AsRef<TagRef>,
        N: AsRef<NameRef>,
    {
        self.add_tags(&name, tags);
        create_dir(self.root.file(&name)).unwrap();
    }

    pub fn rm<N>(&self, name: N)
    where
        N: AsRef<NameRef>,
    {
        let file_path = self.root.file(&name);
        if file_path.is_dir() {
            remove_dir_all(file_path).unwrap();
        } else {
            remove_file(file_path).unwrap();
        }

        let _ = remove_dir_all(self.root.join("tags").join(name.as_ref().as_path()));
    }

    pub fn add_tags<N, T>(&self, name: N, tags: impl IntoIterator<Item = T>)
    where
        N: AsRef<NameRef>,
        T: AsRef<TagRef>,
    {
        create_dir_all(self.tags_path(&name)).unwrap();
        for tag in tags {
            self.add_tag(&name, tag);
        }
    }

    pub fn add_tag<N, T>(&self, name: N, tag: T)
    where
        N: AsRef<NameRef>,
        T: AsRef<TagRef>,
    {
        File::create(self.tag_path(name, tag)).unwrap();
    }

    pub fn del_tags<N>(&self, name: N)
    where
        N: AsRef<NameRef>,
    {
        remove_dir_all(self.tags_path(name).parent().unwrap()).unwrap();
    }

    pub fn del_tag<N, T>(&self, name: N, tag: T)
    where
        N: AsRef<NameRef>,
        T: AsRef<TagRef>,
    {
        remove_file(self.tag_path(name, tag)).unwrap();
    }

    fn tag_path<N, T>(&self, name: N, tag: T) -> PathBuf
    where
        N: AsRef<NameRef>,
        T: AsRef<TagRef>,
    {
        self.tags_path(name).join(tag.as_ref().as_path())
    }

    fn tags_path<N>(&self, name: N) -> PathBuf
    where
        N: AsRef<NameRef>,
    {
        self.root
            .metadata()
            .join("tags")
            .join(name.as_ref().as_path())
            .join("tag")
    }

    pub fn symlink_file(&self, path: &TaggedPath) {
        create_dir_all(self.root.join(path).parent().unwrap()).unwrap();
        symlink_file(self.root.file(path.name()), self.root.join(path)).unwrap();
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
