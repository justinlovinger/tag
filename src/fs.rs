use core::panic;
use std::{
    fs::{copy, create_dir, read_dir, read_link, symlink_metadata},
    path::Path,
};

pub fn symlink_file<P, Q>(original: P, link: Q) -> std::io::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    #[cfg(target_family = "unix")]
    {
        std::os::unix::fs::symlink(original, link)
    }

    #[cfg(target_family = "windows")]
    {
        std::os::windows::fs::symlink_file(original, link)
    }
}

pub fn symlink_dir<P, Q>(original: P, link: Q) -> std::io::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    #[cfg(target_family = "unix")]
    {
        std::os::unix::fs::symlink(original, link)
    }

    #[cfg(target_family = "windows")]
    {
        std::os::windows::fs::symlink_dir(original, link)
    }
}

pub fn copy_dir<P, Q>(from: P, to: Q) -> std::io::Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    let metadata = symlink_metadata(from.as_ref())?;
    if metadata.is_file() {
        copy(from, to)?;
    } else if metadata.is_symlink() {
        let target = read_link(from.as_ref())?;
        let target_metadata = std::fs::metadata(&target)?;
        if target_metadata.is_file() {
            symlink_file(from, to)?;
        } else if target_metadata.is_dir() {
            symlink_dir(from, to)?;
        } else {
            panic!("unknown file-type for {}", target.to_string_lossy());
        }
    } else if metadata.is_dir() {
        create_dir(to.as_ref())?;
        for entry in read_dir(from.as_ref())? {
            let entry = entry?;
            copy_dir(entry.path(), to.as_ref().join(entry.file_name()))?;
        }
    } else {
        panic!("unknown file-type for {}", from.as_ref().to_string_lossy());
    }
    Ok(())
}
