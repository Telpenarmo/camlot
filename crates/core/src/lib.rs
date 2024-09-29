mod hir;
mod infer;
mod intern;
mod types;
pub use crate::hir::*;

pub use types::{Type, TypeIdx};

pub(crate) use hir::Name;

pub use infer::{infer, InferenceResult};

pub use intern::{Interned, Interner};
