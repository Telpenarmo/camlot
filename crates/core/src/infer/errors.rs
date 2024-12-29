use crate::hir::{ExprIdx, TypeExprIdx};
use crate::intern::Interner;
use crate::types::{Type, TypeIdx, UnificationVar};
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
        var: UnificationVar,
    },
    UnexpectedUnitPattern {
        pattern: PatternIdx,
        expected: TypeIdx,
    },
    AppliedToNonFunction {
        func: ExprIdx,
        func_type: TypeIdx,
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
}

impl TypeInference {
    pub(super) fn map_unification_error(
        &mut self,
        types: &mut Interner<Type>,
        reason: ConstraintReason,
        error: &UnifcationError,
    ) -> TypeError {
        match *error {
            UnifcationError::Occurs(typ, var) => {
                let src = match reason {
                    ConstraintReason::Checking(idx) => idx,
                    ConstraintReason::Application(_lhs, rhs) => rhs,
                    ConstraintReason::AnnotatedUnit(_) => {
                        unreachable!("No unification variable occurs in unit type.")
                    }
                    ConstraintReason::WrongAnnotation(_idx) => panic!(),
                };
                TypeError::CyclicType {
                    expr: src,
                    typ,
                    var,
                }
            }
            UnifcationError::NotUnifiable { expected, actual } => match reason {
                ConstraintReason::Application(lhs, rhs) => {
                    let expected = self.normalize(types, expected);
                    let actual = self.normalize(types, actual);
                    let expected_typ = types.lookup(expected);
                    if self.expr_types[lhs] == expected
                        && !matches!(expected_typ, Type::Arrow(_, _))
                    {
                        TypeError::AppliedToNonFunction {
                            func: lhs,
                            func_type: actual,
                        }
                    } else {
                        TypeError::WrongArgument {
                            arg: rhs,
                            expected,
                            actual,
                        }
                    }
                }

                ConstraintReason::AnnotatedUnit(idx) => TypeError::UnexpectedUnitPattern {
                    pattern: idx,
                    expected: self.normalize(types, expected),
                },

                ConstraintReason::Checking(src) => TypeError::TypeMismatch {
                    expr: src,
                    expected: self.normalize(types, expected),
                    actual: self.normalize(types, actual),
                },

                ConstraintReason::WrongAnnotation(idx) => TypeError::WrongAnnotation {
                    annotation: idx,
                    actual: self.normalize(types, actual),
                    expected: self.normalize(types, expected),
                },
            },
        }
    }
}
