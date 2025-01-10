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

pub(crate) struct TypeInference {
    unification_table: UnificationTable,
    defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    expr_types: ArenaMap<ExprIdx, TypeIdx>,
    errors: Vec<TypeError>,
    types_env: Environment,
    substitution_cache: HashMap<TypeIdx, TypeIdx>,
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

fn get_from_env(
    env: &Environment,
    types: &mut Interner<Type>,
    unification_table: &mut UnificationTable,
    name: Name,
) -> Option<TypeIdx> {
    env.get(&name)
        .map(|scheme| scheme.instantiate(types, unification_table))
}

pub struct InferenceResult {
    pub expr_types: ArenaMap<ExprIdx, TypeIdx>,
    pub defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    pub diagnostics: Vec<TypeError>,
}

#[must_use]
pub fn infer(module: &Module, types: &mut Interner<Type>) -> InferenceResult {
    let inference = TypeInference::new();
    inference.infer(module, types)
}

impl TypeInference {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            unification_table: UnificationTable::new(),
            defn_types: ArenaMap::new(),
            expr_types: ArenaMap::new(),
            errors: Vec::new(),
            types_env: Environment::new(),
            substitution_cache: HashMap::new(),
        }
    }

    fn load_definitions(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        env: &mut Environment,
    ) {
        // Walk through the definitions and register them in the environment as fresh unification variables
        for (_, defn) in module.iter_definitions() {
            env.insert(
                defn.name,
                TypeScheme::empty(self.next_unification_var(types)), // "scheme built with resolved parameters",
            );
        }
    }

    fn load_type_aliases(&mut self, module: &Module, types: &mut Interner<Type>) {
        for (_, defn) in module.type_definitions() {
            let typ = self.resolve_type_expr_or_var(module, types, defn.defn);
            self.types_env.insert(defn.name, TypeScheme::empty(typ));
        }
    }

    fn infer(mut self, module: &Module, types: &mut Interner<Type>) -> InferenceResult {
        let mut initial_env = module.get_known_definitions();
        self.load_definitions(module, types, &mut initial_env);
        self.types_env = module.get_known_types();
        self.load_type_aliases(module, types);

        for (idx, defn) in module.iter_definitions() {
            self.check_definition(types, &initial_env, module, idx, defn);
        }

        self.substitute(types);

        InferenceResult {
            expr_types: self.expr_types,
            defn_types: self.defn_types,
            diagnostics: self.errors,
        }
    }

    fn check_definition(
        &mut self,
        types: &mut Interner<Type>,
        initial_env: &Environment,
        module: &Module,
        idx: DefinitionIdx,
        defn: &Definition,
    ) {
        let registered_unification_var =
            get_from_env(initial_env, types, &mut self.unification_table, defn.name).unwrap();

        let mut def_env = initial_env.clone();
        let params: Vec<_> = defn
            .params
            .iter()
            .map(|param| {
                let param = &module[*param];
                let typ = self.resolve_type_expr(module, types, param.typ);
                let (env, typ) = self.resolve_pattern(module, param.pattern, typ, types, &def_env);
                def_env = env;
                typ
            })
            .collect();

        let ret_type = self.check_or_infer(module, types, &def_env, defn.defn, defn.return_type);

        let typ = curry(types, &params, ret_type);

        let reason = ConstraintReason::check(module, defn.defn, typ, registered_unification_var);
        self.eq(types, reason);

        self.expr_types.insert(defn.defn, ret_type);
        self.defn_types.insert(idx, typ);
    }

    fn eq(&mut self, types: &mut Interner<Type>, src: ConstraintReason) {
        let mut errors = std::mem::take(&mut self.errors);
        errors.extend(
            self.solve(types, src)
                .into_iter()
                .map(|error| self.map_unification_error(types, src, &error)),
        );
        self.errors = errors;
    }

    fn next_unification_var(&mut self, types: &mut Interner<Type>) -> TypeIdx {
        let var = self.unification_table.new_key(None);
        types.intern(Type::Unifier(var))
    }

    fn infer_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        env: &Environment,
        expr: ExprIdx,
    ) -> TypeIdx {
        let typ = self.infer_expr_impl(module, types, env, expr);
        self.expr_types.insert(expr, typ);
        typ
    }

    fn infer_expr_impl(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        env: &Environment,
        expr: ExprIdx,
    ) -> TypeIdx {
        match &module[expr] {
            Expr::Missing => self.next_unification_var(types),
            Expr::LetExpr(let_expr) => {
                let def_typ = self.resolve_type_expr(module, types, let_expr.return_type);
                let (env, typ) = self.resolve_pattern(module, let_expr.lhs, def_typ, types, env);

                let constraint =
                    annotated_return_constraint(module, let_expr.defn, let_expr.return_type, typ);
                self.check_expr(module, types, let_expr.defn, &env, typ, constraint);

                self.infer_expr(module, types, &env, let_expr.body)
            }
            &Expr::IdentExpr { name } => {
                get_from_env(env, types, &mut self.unification_table, name).unwrap_or_else(|| {
                    let err = TypeError::UnboundVariable { expr, name };
                    self.errors.push(err);
                    error(types)
                })
            }

            Expr::LambdaExpr(lambda) => {
                let param = &module[lambda.param];
                let from = self.resolve_type_expr(module, types, param.typ);

                let (env, from) = self.resolve_pattern(module, param.pattern, from, types, env);

                let to = self.check_or_infer(module, types, &env, lambda.body, lambda.return_type);

                arrow(types, from, to)
            }
            &Expr::AppExpr { func, arg } => {
                let lhs_typ = self.infer_expr(module, types, env, func);

                let arg_typ = self.infer_expr(module, types, env, arg);
                let ret_typ = self.next_unification_var(types);

                let reason =
                    ConstraintReason::app_target(module, func, arg, lhs_typ, arg_typ, ret_typ);
                self.eq(types, reason);

                ret_typ
            }
            Expr::LiteralExpr(lit) => types.intern(match lit {
                Literal::Unit => Type::Unit,
                Literal::IntLiteral(_) => Type::Int,
                Literal::BoolLiteral(_) => Type::Bool,
            }),
        }
    }

    fn check_or_infer(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        env: &Environment,
        expr: ExprIdx,
        annotation: TypeExprIdx,
    ) -> TypeIdx {
        self.resolve_type_expr(module, types, annotation)
            .inspect(|&typ| {
                let constraint = annotated_return_constraint(module, expr, annotation, typ);
                self.check_expr(module, types, expr, env, typ, constraint);
            })
            .unwrap_or_else(|| self.infer_expr(module, types, env, expr))
    }

    fn check_expr<F>(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        expr: ExprIdx,
        env: &Environment,
        typ: TypeIdx,
        constraint_factory: F,
    ) where
        F: ConstraintReasonFactory,
    {
        let expected = types.lookup(typ);
        match (&module[expr], expected.clone()) {
            (Expr::LiteralExpr(Literal::BoolLiteral(_)), Type::Bool)
            | (Expr::LiteralExpr(Literal::IntLiteral(_)), Type::Int)
            | (Expr::LiteralExpr(Literal::Unit), Type::Unit) => {}

            (Expr::LambdaExpr(lambda), Type::Arrow(from, to)) => {
                let param = &module[lambda.param];

                let from = self
                    .resolve_type_expr(module, types, param.typ)
                    .inspect(|annotation| {
                        let reason = ConstraintReason::WrongParamAnnotation {
                            annotation: param.typ,
                            expected: from,
                            actual: *annotation,
                        };
                        self.eq(types, reason);
                    })
                    .unwrap_or(from);

                let (env, _) = self.resolve_pattern(module, param.pattern, Some(from), types, env);

                let constraint = check_constraint(module, expr, to);
                self.check_expr(module, types, lambda.body, &env, to, constraint);
            }

            _ => {
                let actual = self.infer_expr(module, types, env, expr);
                let reason = constraint_factory.make(actual);
                self.eq(types, reason);
            }
        }
    }

    fn resolve_pattern(
        &mut self,
        module: &Module,
        pat_idx: PatternIdx,
        ann: Option<TypeIdx>,
        types: &mut Interner<Type>,
        env: &Environment,
    ) -> (Environment, TypeIdx) {
        match module[pat_idx] {
            Pattern::Ident(name) => {
                let typ = ann.or_unification_var(self, types);
                (env.update(name, TypeScheme::empty(typ)), typ)
            }
            Pattern::Wildcard => {
                let typ = ann.or_unification_var(self, types);
                (env.clone(), typ)
            }
            Pattern::Unit => {
                if let Some(ann) = ann {
                    let reason = ConstraintReason::AnnotatedUnit(pat_idx, ann);
                    self.eq(types, reason);
                    (env.clone(), ann)
                } else {
                    (env.clone(), unit(types))
                }
            }
        }
    }

    fn resolve_type_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        type_expr: TypeExprIdx,
    ) -> Option<TypeIdx> {
        match &module[type_expr] {
            TypeExpr::Missing => None,
            &TypeExpr::IdentTypeExpr { name } => Some(
                get_from_env(&self.types_env, types, &mut self.unification_table, name)
                    .unwrap_or_else(|| {
                        let err = TypeError::UnboundTypeVariable { type_expr, name };
                        self.errors.push(err);
                        error(types)
                    }),
            ),
            &TypeExpr::TypeArrow { from, to } => {
                let from = self.resolve_type_expr_or_var(module, types, from);
                let to = self.resolve_type_expr_or_var(module, types, to);
                Some(arrow(types, from, to))
            }
        }
    }

    fn resolve_type_expr_or_var(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        type_expr: TypeExprIdx,
    ) -> TypeIdx {
        self.resolve_type_expr(module, types, type_expr)
            .or_unification_var(self, types)
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
