use crate::hir::{ExprIdx, TypeExprIdx};
use crate::types::{Type, TypeIdx};
use crate::{Name, PatternIdx};

use super::unify::UnifcationError;
use super::{ConstraintReason, TypeInference};

#[derive(PartialEq, Debug)]
pub enum TypeError {
    TypeMismatch {
        expr: ExprIdx,
        expected: TypeIdx,
        actual: TypeIdx,
    },
    UnboundVariable {
        expr: ExprIdx,
        name: Name,
    },
    UnboundTypeVariable {
        type_expr: TypeExprIdx,
        name: Name,
    },
    CyclicType {
        expr: ExprIdx,
        typ: TypeIdx,
        var: TypeIdx,
    },
    UnexpectedUnitPattern {
        pattern: PatternIdx,
        expected: TypeIdx,
    },
    AppliedToNonFunction {
        lhs: ExprIdx,
        lhs_type: TypeIdx,
    },
    WrongArgument {
        arg: ExprIdx,
        expected: TypeIdx,
        actual: TypeIdx,
    },
    WrongAnnotation {
        annotation: TypeExprIdx,
        actual: TypeIdx,
        expected: TypeIdx,
    },
    ExpectedDueToAnnotation {
        expr: ExprIdx,
        annotation: TypeExprIdx,
        actual: TypeIdx,
        expected: TypeIdx,
    },
}

impl TypeInference<'_> {
    pub(super) fn map_unification_error(
        &mut self,
        reason: ConstraintReason,
        error: &UnifcationError,
    ) -> TypeError {
        match *error {
            UnifcationError::Occurs(typ, var) => {
                let src = match reason {
                    ConstraintReason::Application { rhs, .. } => rhs,
                    ConstraintReason::AnnotatedUnit { .. } => {
                        unreachable!("No unification variable occurs in unit type.")
                    }
                    ConstraintReason::WrongParamAnnotation { .. } => panic!(),
                    ConstraintReason::AnnotatedReturnType { expr, .. }
                    | ConstraintReason::Checking { expr, .. } => expr,
                };
                TypeError::CyclicType {
                    expr: src,
                    typ,
                    var,
                }
            }
            UnifcationError::NotUnifiable { left: _, right: _ } => match reason {
                ConstraintReason::Application {
                    lhs,
                    rhs,
                    lhs_type,
                    arg_type,
                    ..
                } => match self.types.get_type(lhs_type) {
                    &Type::Arrow(from, _to) => TypeError::WrongArgument {
                        arg: rhs,
                        expected: from,
                        actual: arg_type,
                    },
                    _ => TypeError::AppliedToNonFunction { lhs, lhs_type },
                },

                ConstraintReason::AnnotatedUnit(pattern, expected) => {
                    TypeError::UnexpectedUnitPattern { pattern, expected }
                }

                ConstraintReason::Checking {
                    expr,
                    actual,
                    expected,
                } => TypeError::TypeMismatch {
                    expr,
                    expected,
                    actual,
                },

                ConstraintReason::WrongParamAnnotation {
                    annotation,
                    actual,
                    expected,
                    ..
                } => TypeError::WrongAnnotation {
                    annotation,
                    actual,
                    expected,
                },

                ConstraintReason::AnnotatedReturnType {
                    annotation,
                    expected,
                    actual,
                    expr,
                } => TypeError::ExpectedDueToAnnotation {
                    expr,
                    annotation,
                    actual,
                    expected,
                },
            },
        }
    }
}
