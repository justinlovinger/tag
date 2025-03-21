use std::{fs::symlink_metadata, path::Path};

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

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RemoveSymlinkError {
    #[error("Not a symlink")]
    NotSymlinkError,
    Filesystem(#[from] std::io::Error),
}

pub fn remove_symlink<P>(path: P) -> Result<(), RemoveSymlinkError>
where
    P: AsRef<Path>,
{
    if symlink_metadata(path.as_ref())?.is_symlink() {
        #[cfg(target_family = "unix")]
        {
            std::fs::remove_file(path)?;
        }

        #[cfg(target_family = "windows")]
        {
            if path.is_dir() {
                std::fs::remove_dir(path)?;
            } else {
                std::fs::remove_file(path)?;
            }
        }

        Ok(())
    } else {
        Err(RemoveSymlinkError::NotSymlinkError)
    }
}
