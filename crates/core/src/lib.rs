mod builtin;
mod hir;
mod infer;
mod intern;
mod types;
pub use crate::hir::*;

pub use types::{Type, TypeIdx, display_type};

pub(crate) use hir::Name;

pub use infer::{infer, ConstraintReason, InferenceResult, TypeError};

pub use intern::{Interned, Interner};
