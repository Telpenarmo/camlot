use crate::intern::Interner;
use crate::types::TypeIdx;
use crate::types::{Type, Unifier};
use crate::{Expr, ExprIdx, Module, PatternIdx, TypeExprIdx};

use super::{arrow, unit, TypeInference};

#[derive(PartialEq, Debug)]
pub(super) enum UnifcationError {
    Occurs(TypeIdx, Unifier),
    NotUnifiable { left: TypeIdx, right: TypeIdx },
}

impl TypeInference<'_> {
    pub(super) fn solve(&mut self, src: ConstraintReason) -> Vec<UnifcationError> {
        match src.constraint(self.types) {
            Constraint::EqualTypes(a, b) => self.unify_eq(a, b),
        }
    }
    fn occurs(&mut self, typ: TypeIdx, v: Unifier) -> bool {
        match self.types.get_type(typ) {
            Type::Unifier(u) if u.tag == v.tag => true,
            Type::Unifier(u) => {
                let min = if u.level < v.level { *u } else { v };
                let tag = self.next_tag();
                self.types
                    .replace_with_fresh(typ, Type::unifier(min.level, tag));
                false
            }
            &Type::Arrow(from, to) => self.occurs(from, v) || self.occurs(to, v),
            _ => false,
        }
    }

    fn unify_eq(&mut self, expected_idx: TypeIdx, actual_idx: TypeIdx) -> Vec<UnifcationError> {
        if expected_idx == actual_idx {
            return vec![];
        }

        let expected = self.types.lookup(expected_idx).clone();
        let actual = self.types.lookup(actual_idx).clone();
        match ((expected, expected_idx), (actual, actual_idx)) {
            ((Type::Bound(_, _), _), _) | (_, (Type::Bound(_, _), _)) => {
                unreachable!("Bound type should have been instantiated")
            }

            ((Type::Unifier(a), a_idx), (Type::Unifier(b), b_idx)) => {
                let ((min, min_idx), (_max, max_idx)) = if a.level < b.level {
                    ((a, a_idx), (b, b_idx))
                } else {
                    ((b, b_idx), (a, a_idx))
                };
                let tag = self.next_tag();
                self.types
                    .replace_with_fresh(min_idx, Type::unifier(min.level, tag));
                self.replace(max_idx, min_idx);
                vec![]
            }

            ((Type::Link(typ, _), _), (_, t2)) | ((_, t2), (Type::Link(typ, _), _)) => {
                self.unify_eq(typ, t2)
            }

            ((Type::Arrow(from, to), _), (Type::Arrow(from2, to2), _)) => {
                let mut from = self.unify_eq(from, from2);
                let mut to = self.unify_eq(to, to2);
                from.append(&mut to);
                from
            }

            ((Type::Unifier(u), u_idx), (_, ty_idx)) | ((_, ty_idx), (Type::Unifier(u), u_idx)) => {
                if self.occurs(ty_idx, u) {
                    return vec![UnifcationError::Occurs(ty_idx, u)];
                }
                self.replace(u_idx, ty_idx);
                vec![]
            }

            _ => vec![UnifcationError::NotUnifiable {
                left: expected_idx,
                right: actual_idx,
            }],
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub(super) enum ConstraintReason {
    Application {
        lhs: ExprIdx,
        rhs: ExprIdx,
        lhs_type: TypeIdx,
        arg_type: TypeIdx,
        return_type: TypeIdx,
    },
    Checking {
        expr: ExprIdx,
        expected: TypeIdx,
        actual: TypeIdx,
    },
    AnnotatedUnit(PatternIdx, TypeIdx),
    WrongParamAnnotation {
        annotation: TypeExprIdx,
        expected: TypeIdx,
        actual: TypeIdx,
    },
    AnnotatedReturnType {
        annotation: TypeExprIdx,
        expected: TypeIdx,
        actual: TypeIdx,
        expr: ExprIdx,
    },
}

enum Constraint {
    EqualTypes(TypeIdx, TypeIdx),
}

impl ConstraintReason {
    #[must_use]
    fn constraint(self, types: &mut Interner<Type>) -> Constraint {
        match self {
            ConstraintReason::Application {
                arg_type,
                return_type,
                lhs_type,
                ..
            } => Constraint::EqualTypes(lhs_type, arrow(types, arg_type, return_type)),
            ConstraintReason::Checking {
                actual, expected, ..
            }
            | ConstraintReason::WrongParamAnnotation {
                expected, actual, ..
            }
            | ConstraintReason::AnnotatedReturnType {
                expected, actual, ..
            } => Constraint::EqualTypes(expected, actual),
            ConstraintReason::AnnotatedUnit(_, typ) => {
                let unit_type = unit(types);
                Constraint::EqualTypes(typ, unit_type)
            }
        }
    }
}

impl ConstraintReason {
    pub(super) fn app_target(
        module: &Module,
        lhs: ExprIdx,
        rhs: ExprIdx,
        lhs_type: TypeIdx,
        arg_type: TypeIdx,
        return_type: TypeIdx,
    ) -> Self {
        Self::Application {
            lhs: collapse_lets(module, lhs),
            rhs: collapse_lets(module, rhs),
            lhs_type,
            arg_type,
            return_type,
        }
    }

    pub(super) fn check(
        module: &Module,
        expr: ExprIdx,
        actual: TypeIdx,
        expected: TypeIdx,
    ) -> Self {
        Self::Checking {
            expr: collapse_lets(module, expr),
            expected,
            actual,
        }
    }

    pub(super) fn annotated_return(
        module: &Module,
        expr: ExprIdx,
        annotation: TypeExprIdx,
        expected: TypeIdx,
        actual: TypeIdx,
    ) -> Self {
        ConstraintReason::AnnotatedReturnType {
            annotation,
            expected,
            actual,
            expr: collapse_lets(module, expr),
        }
    }
}

pub(super) trait ConstraintReasonFactory {
    fn make(self, actual: TypeIdx) -> ConstraintReason;
}

pub(super) fn check_constraint(
    module: &Module,
    expr: ExprIdx,
    to: TypeIdx,
) -> impl ConstraintReasonFactory + '_ {
    pub(super) struct MakeCheckConstraint<'a> {
        module: &'a Module,
        expr: ExprIdx,
        to: TypeIdx,
    }

    impl ConstraintReasonFactory for MakeCheckConstraint<'_> {
        fn make(self, actual: TypeIdx) -> ConstraintReason {
            ConstraintReason::check(self.module, self.expr, actual, self.to)
        }
    }

    MakeCheckConstraint { module, expr, to }
}

pub(super) fn annotated_return_constraint(
    module: &Module,
    expr: ExprIdx,
    annotation: TypeExprIdx,
    expected: TypeIdx,
) -> impl ConstraintReasonFactory + '_ {
    struct MakeAnnotatedReturnConstraint<'a> {
        module: &'a Module,
        expr: ExprIdx,
        annotation: TypeExprIdx,
        expected: TypeIdx,
    }

    impl ConstraintReasonFactory for MakeAnnotatedReturnConstraint<'_> {
        fn make(self, actual: TypeIdx) -> ConstraintReason {
            ConstraintReason::annotated_return(
                self.module,
                self.expr,
                self.annotation,
                self.expected,
                actual,
            )
        }
    }

    MakeAnnotatedReturnConstraint {
        module,
        expr,
        annotation,
        expected,
    }
}

fn collapse_lets(module: &Module, expr: ExprIdx) -> ExprIdx {
    let mut expr = expr;
    while let Expr::LetExpr(let_expr) = &module[expr] {
        expr = let_expr.body;
    }
    expr
}

pub(super) trait MaybeType {
    fn or_unification_var(self, inference: &mut TypeInference) -> TypeIdx;
}

impl MaybeType for Option<TypeIdx> {
    fn or_unification_var(self, inference: &mut TypeInference) -> TypeIdx {
        self.unwrap_or_else(|| inference.fresh())
    }
}
