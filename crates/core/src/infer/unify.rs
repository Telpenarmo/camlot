use std::collections::HashMap;

use crate::intern::Interner;
use crate::types::Type;
use crate::types::{TypeIdx, UnificationVar};
use crate::{Expr, ExprIdx, Module, PatternIdx, TypeExprIdx};

use super::normalize::normalize;
use super::{arrow, unit, TypeInference};

impl Type {
    fn occurs(&self, types: &Interner<Type>, v: UnificationVar) -> bool {
        match self {
            Type::Arrow(from, to) => {
                let from = types.lookup(*from);
                let to = types.lookup(*to);
                from.occurs(types, v) || to.occurs(types, v)
            }
            Type::Unifier(u) => *u == v,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub(super) enum UnifcationError {
    Occurs(TypeIdx, UnificationVar),
    NotUnifiable { left: TypeIdx, right: TypeIdx },
}

impl TypeInference {
    pub(super) fn solve(
        &mut self,
        types: &mut Interner<Type>,
        src: ConstraintReason,
    ) -> Vec<UnifcationError> {
        match src.constraint(types) {
            Constraint::EqualTypes(a, b) => self.unify_eq(types, a, b),
        }
    }

    fn unify_eq(
        &mut self,
        types: &mut Interner<Type>,
        expected_idx: TypeIdx,
        actual_idx: TypeIdx,
    ) -> Vec<UnifcationError> {
        let mut cache = HashMap::new();
        let expected_idx = normalize(types, &mut cache, &mut self.unification_table, expected_idx);
        let actual_idx = normalize(types, &mut cache, &mut self.unification_table, actual_idx);

        let expected = types.lookup(expected_idx);
        let actual = types.lookup(actual_idx);
        match ((expected, expected_idx), (actual, actual_idx)) {
            ((Type::Bool, _), (Type::Bool, _))
            | ((Type::Int, _), (Type::Int, _))
            | ((Type::Unit, _), (Type::Unit, _)) => vec![],

            ((&Type::Arrow(from, to), _), (&Type::Arrow(from2, to2), _)) => {
                let mut from = self.unify_eq(types, from, from2);
                let mut to = self.unify_eq(types, to, to2);
                from.append(&mut to);
                from
            }

            ((Type::Unifier(a), _), (Type::Unifier(b), _)) => self
                .unification_table
                .unify_var_var(*a, *b)
                .err()
                .map(|(a, b)| self.unify_eq(types, a, b))
                .unwrap_or_default(),

            ((Type::Unifier(u), _), (ty, ty_idx)) | ((ty, ty_idx), (Type::Unifier(u), _)) => {
                if ty.occurs(types, *u) {
                    return vec![UnifcationError::Occurs(ty_idx, *u)];
                }
                self.unification_table
                    .unify_var_value(*u, Some(ty_idx))
                    .err()
                    .map(|(a, b)| self.unify_eq(types, a, b))
                    .unwrap_or_default()
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
    fn or_unification_var(
        self,
        inference: &mut TypeInference,
        types: &mut Interner<Type>,
    ) -> TypeIdx;
}

impl MaybeType for Option<TypeIdx> {
    fn or_unification_var(
        self,
        inference: &mut TypeInference,
        types: &mut Interner<Type>,
    ) -> TypeIdx {
        self.unwrap_or_else(|| inference.next_unification_var(types))
    }
}
