mod errors;
// mod normalize;
mod label;
mod nth_ident;
mod unify;

use std::collections::HashMap;

pub use errors::TypeError;
use la_arena::ArenaMap;
use unify::{
    annotated_return, check_constraint, ConstraintReason, ConstraintReasonFactory, MaybeType,
};

use crate::hir::{Expr, ExprIdx, Literal, Module, TypeExpr, TypeExprIdx};
use crate::intern::Interner;
use crate::types::{GeneralizedLabels, Level, Skolem, Type, TypeIdx, Unifier, Unique};
use crate::{Definition, DefinitionIdx, Name, Pattern, PatternIdx};

pub(crate) struct TypeInference<'a> {
    defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    expr_types: ArenaMap<ExprIdx, TypeIdx>,
    errors: Vec<TypeError>,
    module: &'a Module,
    types: &'a mut Interner<Type>,
    names: &'a mut Interner<String>,
    current_level: Level,
    next_unification_var: Unique,
    generalized_labels: GeneralizedLabels,
}

type Environment = imbl::HashMap<Name, TypeIdx>;

pub struct InferenceResult {
    pub expr_types: ArenaMap<ExprIdx, TypeIdx>,
    pub defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    pub diagnostics: Vec<TypeError>,
    pub generalized_labels: GeneralizedLabels,
}

#[must_use]
pub fn infer(
    module: &Module,
    names: &mut Interner<String>,
    types: &mut Interner<Type>,
) -> InferenceResult {
    TypeInference::new(module, names, types).infer()
}

impl<'a> TypeInference<'a> {
    #[must_use]
    pub(crate) fn new(
        module: &'a Module,
        names: &'a mut Interner<String>,
        types: &'a mut Interner<Type>,
    ) -> Self {
        Self {
            defn_types: ArenaMap::new(),
            expr_types: ArenaMap::new(),
            errors: Vec::new(),
            module,
            names,
            types,
            current_level: Level(0),
            next_unification_var: Unique::init(),
            generalized_labels: GeneralizedLabels::new(),
        }
    }

    fn get_from_env(&mut self, env: &Environment, name: Name) -> Option<TypeIdx> {
        env.get(&name).map(|typ| self.instantiate(*typ))
    }

    fn load_type_aliases(&mut self, types_env: &mut Environment) {
        for (idx, defn) in self.module.type_definitions() {
            let typ = self.resolve_type_expr_or_var(types_env, defn.defn);

            self.insert_new(types_env, defn.name, typ, |name| {
                TypeError::DuplicatedTypeDefinition { name, double: idx }
            });
        }
    }

    fn load_type_params(&mut self, env: &Environment, type_params: &[Name]) -> Environment {
        type_params
            .iter()
            .map(|&name| {
                let tag = self.next_tag();
                self.generalized_labels.add_label(tag, name);
                let skolem = Type::skolem(self.current_level, tag);
                let skolem = self.types.intern(skolem);
                (name, skolem)
            })
            .collect::<Environment>()
            .union(env.clone())
    }

    fn infer(mut self) -> InferenceResult {
        let mut env = self.module.get_known_definitions();

        let mut types_env = self.module.get_known_types();

        self.load_type_aliases(&mut types_env);

        for (idx, defn) in self.module.iter_definitions() {
            self.infer_definition(&mut env, &types_env, idx, defn);
        }

        InferenceResult {
            expr_types: self.expr_types,
            defn_types: self.defn_types,
            diagnostics: self.errors,
            generalized_labels: self.generalized_labels,
        }
    }

    fn enter(&mut self) {
        self.current_level.0 += 1;
    }

    fn exit(&mut self) {
        self.current_level.0 -= 1;
    }

    fn infer_definition(
        &mut self,
        global_env: &mut Environment,
        types_env: &Environment,
        idx: DefinitionIdx,
        defn: &Definition,
    ) {
        self.enter();

        let types_env = self.load_type_params(types_env, &defn.type_params);

        let mut env = global_env.clone();
        let params: Vec<_> = defn
            .params
            .iter()
            .map(|param| {
                let param = &self.module[*param];
                let typ = self.resolve_type_expr(&types_env, param.typ);
                let (env_with_param, typ) = self.resolve_pattern(&env, param.pattern, typ);
                env = env_with_param;
                typ
            })
            .collect();

        let body_type = self.resolve_type_expr_or_var(&types_env, defn.return_type);
        let defn_type = curry(self.types, &params, body_type);

        if defn.name != self.names.empty_name() {
            env.insert(defn.name, defn_type);
        }

        self.insert_new(global_env, defn.name, defn_type, |name| {
            TypeError::DuplicatedDefinition { name, double: idx }
        });
        self.defn_types.insert(idx, defn_type);

        let constraint = annotated_return(self.module, defn.defn, defn.return_type, body_type);
        self.check_expr(&env, &types_env, defn.defn, body_type, constraint);

        self.exit();

        self.generalize(defn_type);

        self.assign_labels(idx);
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

    fn next_tag(&mut self) -> Unique {
        self.next_unification_var.next()
    }

    fn fresh(&mut self) -> TypeIdx {
        let tag = self.next_tag();
        self.types.intern(Type::unifier(self.current_level, tag))
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
            Expr::Missing => self.fresh(),
            Expr::LetExpr(let_expr) => {
                self.enter();
                let types_env = self.load_type_params(types_env, &let_expr.type_params);
                let def_typ = self.resolve_type_expr(&types_env, let_expr.return_type);
                let (new_env, typ) = self.resolve_pattern(env, let_expr.lhs, def_typ);

                let constraint =
                    annotated_return(self.module, let_expr.defn, let_expr.return_type, typ);
                let env = if let_expr.rec { &new_env } else { env };
                self.check_expr(env, &types_env, let_expr.defn, typ, constraint);
                self.exit();

                self.generalize(typ);
                self.infer_expr(&new_env, &types_env, let_expr.body)
            }

            &Expr::IdentExpr { name } => self.get_from_env(env, name).unwrap_or_else(|| {
                let err = TypeError::UnboundVariable { expr, name };
                self.errors.push(err);
                self.fresh()
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
                let ret_typ = self.fresh();

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
                let constraint = annotated_return(self.module, expr, annotation, typ);
                self.check_expr(env, types_env, expr, typ, constraint);
                self.expr_types.insert(expr, typ);
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
        self.expr_types.insert(expr, typ);
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
                (env.update(name, typ), typ)
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
                    let tag = self.next_tag();
                    self.generalized_labels.add_label(tag, name);
                    self.types.intern(Type::unifier(self.current_level, tag))
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

    fn instantiate_impl(&mut self, vars: &mut HashMap<Unique, Unique>, t: TypeIdx) -> TypeIdx {
        match *self.types.get_type(t) {
            Type::Bound(idx) => {
                let tag = vars.entry(idx).or_insert_with(|| self.next_tag());
                self.types.intern(Type::unifier(self.current_level, *tag))
            }
            Type::Arrow(lhs, rhs) => {
                let lhs = self.instantiate_impl(vars, lhs);
                let rhs = self.instantiate_impl(vars, rhs);
                self.types.intern(Type::Arrow(lhs, rhs))
            }
            _ => t,
        }
    }

    pub(crate) fn instantiate(&mut self, typ: TypeIdx) -> TypeIdx {
        self.instantiate_impl(&mut HashMap::new(), typ)
    }

    fn generalize(&mut self, idx: TypeIdx) {
        self.generalize_impl(idx);
    }

    fn generalize_impl(&mut self, idx: TypeIdx) {
        let typ = self.types.lookup(idx).clone();
        match typ {
            Type::Link(idx, _) => self.generalize_impl(idx),
            Type::Unifier(Unifier { level, tag }) if level > self.current_level => {
                let new = self.types.intern(Type::Bound(tag));
                self.replace(idx, new);
            }
            Type::Skolem(Skolem { level, tag }) if level.0 == self.current_level.0 + 1 => {
                let new = self.types.intern(Type::Bound(tag));
                self.replace(idx, new);
            }
            Type::Skolem(Skolem { level, .. }) if level > self.current_level => {
                panic!("Skolem from a higher level");
            }
            Type::Arrow(from, to) => {
                self.generalize_impl(from);
                self.generalize_impl(to);
            }
            _ => {}
        }
    }

    fn replace(&mut self, idx: TypeIdx, new: TypeIdx) {
        let mut idx = idx;
        if idx == new {
            return;
        }
        while let Type::Link(linked, _) = self.types.lookup(idx) {
            idx = *linked;
            if idx == new {
                return;
            }
        }
        let new = Type::Link(new, self.next_tag());
        self.types.replace_with_fresh(idx, new);
    }

    fn insert_new<F>(&mut self, env: &mut Environment, name: Name, typ: TypeIdx, error: F)
    where
        F: Fn(Name) -> TypeError,
    {
        if name == self.names.empty_name() {
            return;
        }
        match env.entry(name) {
            imbl::hashmap::Entry::Occupied(_prev) => {
                self.errors.push(error(name));
            }
            imbl::hashmap::Entry::Vacant(entry) => {
                entry.insert(typ);
            }
        }
    }
}

fn curry(types: &mut Interner<Type>, params: &[TypeIdx], return_type: TypeIdx) -> TypeIdx {
    params.iter().rev().fold(return_type, |acc, param| {
        types.intern(Type::Arrow(*param, acc))
    })
}

fn arrow(types: &mut Interner<Type>, from: TypeIdx, to: TypeIdx) -> TypeIdx {
    types.intern(Type::Arrow(from, to))
}

fn unit(types: &mut Interner<Type>) -> TypeIdx {
    types.intern(Type::Unit)
}

#[cfg(test)]
mod tests;
