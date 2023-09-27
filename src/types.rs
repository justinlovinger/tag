use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq)]
pub struct MoveOp {
    pub from: PathBuf,
    pub to: PathBuf,
}
