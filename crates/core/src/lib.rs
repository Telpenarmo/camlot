mod builtin;
mod hir;
mod infer;
mod intern;
mod types;
pub use crate::hir::*;

pub use types::{display_type, Type, TypeIdx};

pub(crate) use hir::Name;

pub use infer::{infer, ConstraintReason, InferenceResult, TypeError};

pub use intern::{Interned, Interner};
