mod errors;
mod normalize;
mod unify;

use std::collections::HashMap;

use la_arena::ArenaMap;
use unify::{
    annotated_return_constraint, check_constraint, ConstraintReason, ConstraintReasonFactory,
    MaybeType,
};

use crate::hir::{Expr, ExprIdx, Literal, Module, TypeExpr, TypeExprIdx};
use crate::intern::Interner;
use crate::types::{Type, TypeIdx, TypeScheme, UnificationTable, UnificationVar};
use crate::{Definition, DefinitionIdx, Name, Pattern, PatternIdx};
pub use errors::TypeError;

pub(crate) struct TypeInference<'a> {
    unification_table: UnificationTable,
    defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    expr_types: ArenaMap<ExprIdx, TypeIdx>,
    errors: Vec<TypeError>,
    substitution_cache: HashMap<TypeIdx, TypeIdx>,
    module: &'a Module,
    types: &'a mut Interner<Type>,
}

impl TypeScheme {
    pub(crate) fn instantiate(
        &self,
        types: &mut Interner<Type>,
        unification_table: &mut UnificationTable,
    ) -> TypeIdx {
        fn substitute(
            t: TypeIdx,
            fresh_vars: &HashMap<Name, UnificationVar>,
            types: &mut Interner<Type>,
        ) -> TypeIdx {
            match types.lookup(t) {
                Type::Var(v) => fresh_vars
                    .get(v)
                    .map_or(t, |v| types.intern(Type::Unifier(*v))),
                &Type::Arrow(lhs, rhs) => {
                    let lhs = substitute(lhs, fresh_vars, types);
                    let rhs = substitute(rhs, fresh_vars, types);
                    types.intern(Type::Arrow(lhs, rhs))
                }
                _ => t,
            }
        }
        let fresh_vars: HashMap<_, _> = self
            .params
            .iter()
            .map(|p| (*p, unification_table.new_key(None)))
            .collect();

        substitute(self.body, &fresh_vars, types)
    }
}

type Environment = imbl::HashMap<Name, TypeScheme>;

pub struct InferenceResult {
    pub expr_types: ArenaMap<ExprIdx, TypeIdx>,
    pub defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    pub diagnostics: Vec<TypeError>,
}

#[must_use]
pub fn infer(module: &Module, types: &mut Interner<Type>) -> InferenceResult {
    TypeInference::new(module, types).infer()
}

impl<'a> TypeInference<'a> {
    #[must_use]
    pub(crate) fn new(module: &'a Module, types: &'a mut Interner<Type>) -> Self {
        Self {
            unification_table: UnificationTable::new(),
            defn_types: ArenaMap::new(),
            expr_types: ArenaMap::new(),
            errors: Vec::new(),
            substitution_cache: HashMap::new(),
            module,
            types,
        }
    }

    fn load_definitions(&mut self, env: &mut Environment) {
        // Walk through the definitions and register them in the environment as fresh unification variables
        for (_, defn) in self.module.iter_definitions() {
            env.insert(
                defn.name,
                TypeScheme {
                    params: defn.type_params.clone(),
                    body: self.next_unification_var(),
                },
            );
        }
    }

    fn get_from_env(&mut self, env: &Environment, name: Name) -> Option<TypeIdx> {
        env.get(&name)
            .map(|scheme| scheme.instantiate(self.types, &mut self.unification_table))
    }

    fn load_type_aliases(&mut self, types_env: &mut Environment) {
        for (_, defn) in self.module.type_definitions() {
            let typ = self.resolve_type_expr_or_var(types_env, defn.defn);
            types_env.insert(defn.name, TypeScheme::empty(typ));
        }
    }

    fn load_type_params(
        types: &mut Interner<Type>,
        env: &Environment,
        type_params: &[Name],
    ) -> Environment {
        type_params
            .iter()
            .map(|&n| (n, TypeScheme::empty(types.intern(Type::Var(n)))))
            .collect::<Environment>()
            .union(env.clone())
    }

    fn infer(mut self) -> InferenceResult {
        let mut env = self.module.get_known_definitions();
        self.load_definitions(&mut env);

        let mut types_env = self.module.get_known_types();

        self.load_type_aliases(&mut types_env);

        for (idx, defn) in self.module.iter_definitions() {
            self.check_definition(&env, &types_env, idx, defn);
        }

        self.substitute();

        InferenceResult {
            expr_types: self.expr_types,
            defn_types: self.defn_types,
            diagnostics: self.errors,
        }
    }

    fn check_definition(
        &mut self,
        env: &Environment,
        types_env: &Environment,
        idx: DefinitionIdx,
        defn: &Definition,
    ) {
        let registered_var = self.get_from_env(env, defn.name).unwrap();

        let types_env = Self::load_type_params(self.types, types_env, &defn.type_params);

        let mut def_env = env.clone();
        let params: Vec<_> = defn
            .params
            .iter()
            .map(|param| {
                let param = &self.module[*param];
                let typ = self.resolve_type_expr(&types_env, param.typ);
                let (env, typ) = self.resolve_pattern(&def_env, param.pattern, typ);
                def_env = env;
                typ
            })
            .collect();

        let ret_type = self.check_or_infer(&def_env, &types_env, defn.defn, defn.return_type);

        let typ = curry(self.types, &params, ret_type);

        let reason = ConstraintReason::check(self.module, defn.defn, typ, registered_var);
        self.eq(reason);

        self.expr_types.insert(defn.defn, ret_type);
        self.defn_types.insert(idx, typ);
    }

    fn eq(&mut self, src: ConstraintReason) {
        let mut errors = std::mem::take(&mut self.errors);
        errors.extend(
            self.solve(src)
                .into_iter()
                .map(|error| self.map_unification_error(src, &error)),
        );
        self.errors = errors;
    }

    fn next_unification_var(&mut self) -> TypeIdx {
        let var = self.unification_table.new_key(None);
        self.types.intern(Type::Unifier(var))
    }

    fn infer_expr(&mut self, env: &Environment, types_env: &Environment, expr: ExprIdx) -> TypeIdx {
        let typ = self.infer_expr_impl(env, types_env, expr);
        self.expr_types.insert(expr, typ);
        typ
    }

    fn infer_expr_impl(
        &mut self,
        env: &Environment,
        types_env: &Environment,
        expr: ExprIdx,
    ) -> TypeIdx {
        match &self.module[expr] {
            Expr::Missing => self.next_unification_var(),
            Expr::LetExpr(let_expr) => {
                let def_typ = self.resolve_type_expr(types_env, let_expr.return_type);
                let (env, typ) = self.resolve_pattern(env, let_expr.lhs, def_typ);

                let constraint = annotated_return_constraint(
                    self.module,
                    let_expr.defn,
                    let_expr.return_type,
                    typ,
                );
                self.check_expr(&env, types_env, let_expr.defn, typ, constraint);

                self.infer_expr(&env, types_env, let_expr.body)
            }
            &Expr::IdentExpr { name } => self.get_from_env(env, name).unwrap_or_else(|| {
                let err = TypeError::UnboundVariable { expr, name };
                self.errors.push(err);
                error(self.types)
            }),

            Expr::LambdaExpr(lambda) => {
                let param = &self.module[lambda.param];
                let from = self.resolve_type_expr(types_env, param.typ);

                let (env, from) = self.resolve_pattern(env, param.pattern, from);

                let to = self.check_or_infer(&env, types_env, lambda.body, lambda.return_type);

                arrow(self.types, from, to)
            }
            &Expr::AppExpr { func, arg } => {
                let lhs_typ = self.infer_expr(env, types_env, func);

                let arg_typ = self.infer_expr(env, types_env, arg);
                let ret_typ = self.next_unification_var();

                let reason =
                    ConstraintReason::app_target(self.module, func, arg, lhs_typ, arg_typ, ret_typ);
                self.eq(reason);

                ret_typ
            }
            Expr::LiteralExpr(lit) => self.types.intern(match lit {
                Literal::Unit => Type::Unit,
                Literal::IntLiteral(_) => Type::Int,
                Literal::BoolLiteral(_) => Type::Bool,
            }),
        }
    }

    fn check_or_infer(
        &mut self,
        env: &Environment,
        types_env: &Environment,
        expr: ExprIdx,
        annotation: TypeExprIdx,
    ) -> TypeIdx {
        self.resolve_type_expr(types_env, annotation)
            .inspect(|&typ| {
                let constraint = annotated_return_constraint(self.module, expr, annotation, typ);
                self.check_expr(env, types_env, expr, typ, constraint);
            })
            .unwrap_or_else(|| self.infer_expr(env, types_env, expr))
    }

    fn check_expr<F>(
        &mut self,
        env: &Environment,
        types_env: &Environment,
        expr: ExprIdx,
        typ: TypeIdx,
        constraint_factory: F,
    ) where
        F: ConstraintReasonFactory,
    {
        let expected = self.types.lookup(typ);
        match (&self.module[expr], expected.clone()) {
            (Expr::LiteralExpr(Literal::BoolLiteral(_)), Type::Bool)
            | (Expr::LiteralExpr(Literal::IntLiteral(_)), Type::Int)
            | (Expr::LiteralExpr(Literal::Unit), Type::Unit) => {}

            (Expr::LambdaExpr(lambda), Type::Arrow(from, to)) => {
                let param = &self.module[lambda.param];

                let from = self
                    .resolve_type_expr(types_env, param.typ)
                    .inspect(|annotation| {
                        let reason = ConstraintReason::WrongParamAnnotation {
                            annotation: param.typ,
                            expected: from,
                            actual: *annotation,
                        };
                        self.eq(reason);
                    })
                    .unwrap_or(from);

                let (env, _) = self.resolve_pattern(env, param.pattern, Some(from));

                let constraint = check_constraint(self.module, expr, to);
                self.check_expr(&env, types_env, lambda.body, to, constraint);
            }

            _ => {
                let actual = self.infer_expr(env, types_env, expr);
                self.eq(constraint_factory.make(actual));
            }
        }
    }

    fn resolve_pattern(
        &mut self,
        env: &Environment,
        pat_idx: PatternIdx,
        ann: Option<TypeIdx>,
    ) -> (Environment, TypeIdx) {
        match self.module[pat_idx] {
            Pattern::Ident(name) => {
                let typ = ann.or_unification_var(self);
                (env.update(name, TypeScheme::empty(typ)), typ)
            }
            Pattern::Wildcard => {
                let typ = ann.or_unification_var(self);
                (env.clone(), typ)
            }
            Pattern::Unit => {
                if let Some(ann) = ann {
                    let reason = ConstraintReason::AnnotatedUnit(pat_idx, ann);
                    self.eq(reason);
                    (env.clone(), ann)
                } else {
                    (env.clone(), unit(self.types))
                }
            }
        }
    }

    fn resolve_type_expr(
        &mut self,
        types_env: &Environment,
        type_expr: TypeExprIdx,
    ) -> Option<TypeIdx> {
        match &self.module[type_expr] {
            TypeExpr::Missing => None,
            &TypeExpr::IdentTypeExpr { name } => {
                Some(self.get_from_env(types_env, name).unwrap_or_else(|| {
                    let err = TypeError::UnboundTypeVariable { type_expr, name };
                    self.errors.push(err);
                    error(self.types)
                }))
            }
            &TypeExpr::TypeArrow { from, to } => {
                let from = self.resolve_type_expr_or_var(types_env, from);
                let to = self.resolve_type_expr_or_var(types_env, to);
                Some(arrow(self.types, from, to))
            }
        }
    }

    fn resolve_type_expr_or_var(
        &mut self,
        types_env: &Environment,
        type_expr: TypeExprIdx,
    ) -> TypeIdx {
        self.resolve_type_expr(types_env, type_expr)
            .or_unification_var(self)
    }
}

fn curry(types: &mut Interner<Type>, params: &[TypeIdx], return_type: TypeIdx) -> TypeIdx {
    params.iter().rev().fold(return_type, |acc, param| {
        types.intern(Type::Arrow(*param, acc))
    })
}

fn error(types: &mut Interner<Type>) -> TypeIdx {
    types.intern(Type::Error)
}

fn arrow(types: &mut Interner<Type>, from: TypeIdx, to: TypeIdx) -> TypeIdx {
    types.intern(Type::Arrow(from, to))
}

fn unit(types: &mut Interner<Type>) -> TypeIdx {
    types.intern(Type::Unit)
}

#[cfg(test)]
mod tests;
