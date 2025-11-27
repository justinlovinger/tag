mod combine;
mod sort;

use std::{sync::mpsc, thread};

pub use self::{
    combine::{combine, uncombine},
    sort::sort_tags_by_subfrequency,
};

fn into_iter_rayon<T, F>(receiver: mpsc::Receiver<T>, mut f: F)
where
    F: FnMut(T),
{
    loop {
        match receiver.try_recv() {
            Ok(x) => (f)(x),
            Err(e) => match e {
                mpsc::TryRecvError::Empty => {
                    let r#yield = rayon::yield_now();
                    if let Some(rayon::Yield::Idle) = r#yield {
                        thread::yield_now()
                    }
                }
                mpsc::TryRecvError::Disconnected => break,
            },
        }
    }
}
