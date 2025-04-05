mod builtin;
mod hir;
mod infer;
mod intern;
mod types;

pub use crate::hir::*;

pub use types::{bound_variables, display_type, GeneralizedLabels, Type, TypeIdx};

pub use hir::Name;

pub use infer::{infer, InferenceResult, TypeError};

pub use intern::{Interned, Interner};
