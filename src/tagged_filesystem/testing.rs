use std::{
    borrow::Borrow,
    fs::{remove_dir_all, remove_file, File},
    process::{Command, Stdio},
    thread,
    time::Duration,
};

use crate::{NameRef, TagRef};

use super::*;

pub use tagged_paths_with_names::*;

pub fn tagged_filesystem<P>(dir: P) -> TaggedFilesystem
where
    P: AsRef<Path>,
{
    let fs = TaggedFilesystem::init(dir).unwrap();

    // The OS will sometimes fail to close the tags-script as quickly as it should,
    // leading to an error when trying to execute it.
    for _ in 0..100 {
        // Note,
        // this should only wait for `ExecutableFileBusy`,
        // but that kind is unstable as of writing this.
        match Command::new(fs.root.tags())
            .current_dir(fs.root.as_path())
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .spawn()
        {
            Ok(mut x) => {
                let _ = x.kill();
                break;
            }
            Err(_) => thread::sleep(Duration::from_millis(1)),
        }
    }

    fs
}

pub fn tagged_filesystem_with<P, Q, N>(
    dir: P,
    paths: impl IntoIterator<Item = impl Borrow<(Q, N)>>,
) -> TaggedFilesystem
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
    N: AsRef<NameRef>,
{
    let filesystem = tagged_filesystem(dir);
    create_dir_all(filesystem.root.metadata().join("tags")).unwrap();
    for tup in paths.into_iter() {
        let (path, name) = tup.borrow();
        let tagged_path = TaggedPath::from_path(path.as_ref()).unwrap();
        filesystem.touch(tagged_path.tags().map(|tag| tag.to_owned()), name);
        filesystem.symlink_file(&tagged_path, name);
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

    pub fn symlink_file<N>(&self, path: &TaggedPath, name: N)
    where
        N: AsRef<NameRef>,
    {
        create_dir_all(self.root.join(path).parent().unwrap()).unwrap();
        symlink_file(self.root.file(name), self.root.join(path)).unwrap();
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

mod tagged_paths_with_names {
    use proptest::prelude::*;

    use crate::{
        testing::{TaggedPaths, TaggedPathsParams},
        Name, TaggedPath, EXT_SEPARATOR, INLINE_SEPARATOR, TAG_IGNORE,
    };

    #[derive(Debug, Clone)]
    pub struct TaggedPathsWithNames(pub Vec<(TaggedPath, Name)>);

    impl TaggedPathsWithNames {
        pub fn iter(&self) -> std::slice::Iter<'_, (TaggedPath, Name)> {
            self.0.iter()
        }

        pub fn len(&self) -> usize {
            self.0.len()
        }
    }

    impl IntoIterator for TaggedPathsWithNames {
        type Item = (TaggedPath, Name);
        type IntoIter = <Vec<(TaggedPath, Name)> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a TaggedPathsWithNames {
        type Item = &'a (TaggedPath, Name);
        type IntoIter = <&'a [(TaggedPath, Name)] as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.iter()
        }
    }

    impl Arbitrary for TaggedPathsWithNames {
        type Parameters = TaggedPathsParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            TaggedPaths::arbitrary_with(params)
                .prop_flat_map(|paths| {
                    paths
                        .into_iter()
                        .map(|path| {
                            let name = Name::arbitrary_with(Some(path.ext().to_owned()));
                            (Just(path), name)
                        })
                        .collect::<Vec<_>>()
                })
                .prop_map(|paths| {
                    paths
                        .into_iter()
                        .map(|(path, name)| {
                            let tags_str = path
                                .as_path()
                                .as_os_str()
                                .to_str()
                                .unwrap()
                                .split_once(EXT_SEPARATOR)
                                .unwrap()
                                .0;
                            let path = TaggedPath::new(format!(
                                "{}{INLINE_SEPARATOR}{TAG_IGNORE}{}",
                                tags_str, name
                            ))
                            .unwrap();
                            (path, name)
                        })
                        .collect()
                })
                .prop_map(TaggedPathsWithNames)
                .boxed()
        }
    }
}
