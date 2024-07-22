mod hir;
mod infer;
mod intern;
mod types;
pub use crate::hir::*;

pub(crate) use hir::Name;

pub use infer::TypeInference;
