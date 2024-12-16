mod normalize;
mod unify;

use la_arena::ArenaMap;
use unify::UnifcationError;

use crate::hir::{Expr, ExprIdx, Literal, Module, TypeExpr, TypeExprIdx};
use crate::intern::Interner;
use crate::types::{Type, TypeIdx, UnificationTable, UnificationVar};
use crate::{Definition, DefinitionIdx, Name, Pattern, PatternIdx};

#[derive(PartialEq, Debug)]
pub enum TypeError {
    TypeMismatch {
        src: ConstraintReason,
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
        src: ConstraintReason,
        typ: TypeIdx,
    },
}

pub(crate) struct TypeInference {
    unification_table: UnificationTable,
    defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    expr_types: ArenaMap<ExprIdx, TypeIdx>,
    constraints: Vec<Constraint>,
    diagnostics: Vec<TypeError>,
    types_env: Environment,
}

#[derive(PartialEq, Debug)]
pub enum ConstraintReason {
    ApplicationTarget(ExprIdx),
    Checking(ExprIdx),
    UnitPattern(PatternIdx),
}

enum Constraint {
    TypeEqual(ConstraintReason, TypeIdx, TypeIdx),
}

type Environment = imbl::HashMap<Name, TypeIdx>;

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
            constraints: Vec::new(),
            diagnostics: Vec::new(),
            types_env: Environment::new(),
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
            env.insert(defn.name, self.next_unification_var(types));
        }
    }

    fn load_type_aliases(&mut self, module: &Module, types: &mut Interner<Type>) {
        for (_, defn) in module.type_definitions() {
            let typ = self
                .resolve_type_expr(module, types, defn.defn)
                .or_unification_var(self, types);
            self.types_env.insert(defn.name, typ);
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

        self.diagnostics.extend(
            unify::Unifcation::new(&mut self.unification_table, types)
                .unify(self.constraints)
                .into_iter()
                .map(|(src, error)| match error {
                    UnifcationError::Occurs(typ, _v) => TypeError::CyclicType { src, typ },
                    UnifcationError::NotUnifiable { expected, actual } => TypeError::TypeMismatch {
                        src,
                        expected,
                        actual,
                    },
                }),
        );

        Self::substitute(
            types,
            &mut self.expr_types,
            &mut self.defn_types,
            &mut self.unification_table,
        );

        InferenceResult {
            expr_types: self.expr_types,
            defn_types: self.defn_types,
            diagnostics: self.diagnostics,
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
        let ret_type = self.resolve_type_expr(module, types, defn.return_type);
        let mut def_env = initial_env.clone();
        let params: Vec<_> = defn
            .params
            .iter()
            .map(|param| {
                let param = &module[*param];
                let typ = self.resolve_type_expr(module, types, param.typ);
                let (pat_env, typ) =
                    self.resolve_pattern(module, param.pattern, unit(types), typ, types);
                def_env = pat_env.union(def_env.clone());
                typ
            })
            .collect();

        let ret_type = self.check_or_infer_expr(module, types, &def_env, defn.defn, ret_type);

        let typ = curry(types, &params, ret_type);

        self.expr_types.insert(defn.defn, ret_type);
        self.defn_types.insert(idx, typ);
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
                let (pat_env, def_typ) =
                    self.resolve_pattern(module, let_expr.lhs, unit(types), def_typ, types);
                self.check_expr(module, types, let_expr.defn, env, def_typ);

                let env = pat_env.union(env.clone());
                self.infer_expr(module, types, &env, let_expr.body)
            }
            Expr::IdentExpr { name } => {
                if let Some(typ) = env.get(name) {
                    *typ
                } else {
                    self.diagnostics
                        .push(TypeError::UnboundVariable { expr, name: *name });
                    error(types)
                }
            }
            Expr::LambdaExpr(lambda) => {
                let param = &module[lambda.param];
                let from = self.resolve_type_expr(module, types, param.typ);

                let (pat_env, from) =
                    self.resolve_pattern(module, param.pattern, unit(types), from, types);

                let env = pat_env.union(env.clone());
                let to = self.resolve_type_expr(module, types, lambda.return_type);

                let to = self.check_or_infer_expr(module, types, &env, lambda.body, to);

                arrow(types, from, to)
            }
            &Expr::AppExpr {
                func: lhs,
                arg: rhs,
            } => {
                let lhs_typ = self.infer_expr(module, types, env, lhs);

                let arg_typ = self.infer_expr(module, types, env, rhs);
                let ret_typ = self.next_unification_var(types);

                let func_typ = arrow(types, arg_typ, ret_typ);

                let reason = ConstraintReason::ApplicationTarget(lhs);
                self.constraints
                    .push(Constraint::TypeEqual(reason, func_typ, lhs_typ));

                ret_typ
            }
            Expr::LiteralExpr(lit) => types.intern(match lit {
                Literal::Unit => Type::Unit,
                Literal::IntLiteral(_) => Type::Int,
                Literal::BoolLiteral(_) => Type::Bool,
            }),
        }
    }

    fn check_or_infer_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        env: &Environment,
        expr: ExprIdx,
        typ: Option<TypeIdx>,
    ) -> TypeIdx {
        typ.inspect(|typ| self.check_expr(module, types, expr, env, *typ))
            .unwrap_or_else(|| self.infer_expr(module, types, env, expr))
    }

    fn check_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        expr: ExprIdx,
        env: &Environment,
        typ: TypeIdx,
    ) {
        let unit_type = unit(types);
        let expected = types.lookup(typ);
        match (&module[expr], expected.clone()) {
            (Expr::LiteralExpr(Literal::BoolLiteral(_)), Type::Bool)
            | (Expr::LiteralExpr(Literal::IntLiteral(_)), Type::Int)
            | (Expr::LiteralExpr(Literal::Unit), Type::Unit) => {}

            (Expr::LambdaExpr(lambda), Type::Arrow(from, to)) => {
                let param = &module[lambda.param];
                let (pat_env, _) =
                    self.resolve_pattern(module, param.pattern, unit_type, Some(from), types);
                let env = pat_env.union(env.clone());
                self.check_expr(module, types, lambda.body, &env, to);
            }

            _ => {
                let actual = self.infer_expr(module, types, env, expr);
                let reason = ConstraintReason::Checking(expr);
                self.constraints
                    .push(Constraint::TypeEqual(reason, typ, actual));
            }
        }
    }

    fn resolve_pattern(
        &mut self,
        module: &Module,
        pat_idx: PatternIdx,
        unit_type: TypeIdx,
        ann: Option<TypeIdx>,
        types: &mut Interner<Type>,
    ) -> (Environment, TypeIdx) {
        match module[pat_idx] {
            Pattern::Ident(name) => {
                let ann = ann.or_unification_var(self, types);
                (Environment::unit(name, ann), ann)
            }
            Pattern::Wildcard => {
                let ann = ann.or_unification_var(self, types);
                (Environment::new(), ann)
            }
            Pattern::Unit => {
                if let Some(ann) = ann {
                    self.constraints.push(Constraint::TypeEqual(
                        ConstraintReason::UnitPattern(pat_idx),
                        ann,
                        unit_type,
                    ));
                }
                (Environment::new(), unit_type)
            }
        }
    }

    fn resolve_type_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        ty_idx: TypeExprIdx,
    ) -> Option<TypeIdx> {
        match &module[ty_idx] {
            TypeExpr::Missing => None,
            &TypeExpr::IdentTypeExpr { name } => Some(self.types_env.get(&name).map_or_else(
                || {
                    self.diagnostics.push(TypeError::UnboundTypeVariable {
                        type_expr: ty_idx,
                        name,
                    });
                    error(types)
                },
                |typ| *typ,
            )),
            &TypeExpr::TypeArrow { from, to } => {
                let from = self
                    .resolve_type_expr(module, types, from)
                    .or_unification_var(self, types);
                let to = self
                    .resolve_type_expr(module, types, to)
                    .or_unification_var(self, types);
                Some(arrow(types, from, to))
            }
        }
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

trait MaybeType {
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

#[cfg(test)]
mod tests {
    use super::{InferenceResult, TypeInference};
    use crate::{hir, infer::arrow, intern::Interner, types::Type, TypeIdx};

    fn infer_from_str(code: &str) -> (hir::Module, Interner<Type>, InferenceResult) {
        let module_ast = parser::parse(code).module();
        let mut types = Interner::new();
        let mut module = hir::Module::new(&mut types);
        let infer = TypeInference::new();
        module.lower_module(&module_ast);
        let result = infer.infer(&module, &mut types);
        (module, types, result)
    }

    #[track_caller]
    #[inline]
    fn assert_empty<T: std::fmt::Debug>(vec: &[T]) {
        assert!(vec.is_empty(), "{vec:?}");
    }

    #[track_caller]
    #[inline]
    fn get_first_defn_types(module: &hir::Module, result: &InferenceResult) -> (TypeIdx, TypeIdx) {
        let (idx, defn) = module.iter_definitions().next().unwrap();
        (result.defn_types[idx], result.expr_types[defn.defn])
    }

    #[track_caller]
    #[inline]
    fn assert_types_eq(actual: TypeIdx, expected: TypeIdx, types: &Interner<Type>) {
        assert_eq!(types.lookup(actual), types.lookup(expected), "{types:?}");
    }

    #[test]
    fn test_infer_def_with_annotated_param() {
        let (module, mut types, result) = infer_from_str("def f(x: int) = x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn test_infer_def_lambda_with_annotated_param() {
        let (module, mut types, result) = infer_from_str("def f = \\(x: int) -> x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, expected, &types);
    }

    #[test]
    fn test_infer_def_lambda_with_return_type() {
        let (module, mut types, result) = infer_from_str("def f = \\x : int -> x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, expected, &types);
    }

    #[test]
    fn test_infer_annotated_def_lambda() {
        let (module, mut types, result) = infer_from_str("def f : int -> int = \\x -> x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, expected, &types);
    }

    #[test]
    fn test_infer_unannotated_id() {
        let (module, types, result) = infer_from_str("def f x = x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        assert!(
            matches!(types.lookup(actual_defn), Type::Arrow(from, to) if matches!(types.lookup(*from), Type::Unifier(_)) && matches!(types.lookup(*to), Type::Unifier(_))),
            "actual: {actual_defn:?}\n{types:?}"
        );
        assert!(
            matches!(types.lookup(actual_body), Type::Unifier(_)),
            "actual: {actual_body:?}\n{types:?}"
        );
    }

    #[test]
    fn test_infer_empty_def() {
        let (module, mut types, result) = infer_from_str("def f { }");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let unit = types.intern(Type::Unit);

        assert_types_eq(actual_defn, unit, &types);
        assert_types_eq(actual_body, unit, &types);
    }

    #[test]
    fn test_infer_empty_def_with_annotated_param() {
        let (module, mut types, result) = infer_from_str("def f (x: int) { }");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let unit = types.intern(Type::Unit);
        let int = types.intern(Type::Int);

        assert_types_eq(actual_body, unit, &types);
        assert_types_eq(actual_defn, arrow(&mut types, int, unit), &types);
    }

    #[test]
    fn test_infer_block_returning_let() {
        let (module, mut types, result) = infer_from_str("def f (x: int) { let y = x; y }");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);

        assert_types_eq(actual_defn, arrow(&mut types, int, int), &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn infer_endless_recursion() {
        let (module, types, result) = infer_from_str("def f = f;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        assert!(
            matches!(types.lookup(actual_body), Type::Unifier(_)),
            "actual: {:?}\n{types:?}",
            types.lookup(actual_body)
        );
        assert!(
            matches!(types.lookup(actual_defn), Type::Unifier(_)),
            "actual: {:?}\n{types:?}",
            types.lookup(actual_defn)
        );
    }

    #[test]
    fn test_infer_annotated_int() {
        let (module, mut types, result) = infer_from_str("def f : int = 1;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);

        assert_types_eq(actual_defn, int, &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn infer_application() {
        let (module, mut types, result) = infer_from_str("def f (y: int) = ((\\x -> x) y));");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);

        assert_types_eq(actual_defn, arrow(&mut types, int, int), &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn test_infer_int_by_using_add() {
        let (module, mut types, result) = infer_from_str("def f x = (add x x);");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn type_aliases_are_associative() {
        let (module, mut types, result) = infer_from_str("type A = int; type B = A; def i: B = 1;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);

        assert_types_eq(actual_defn, int, &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn types_declarations_are_order_sensitive() {
        let (_module, _types, result) = infer_from_str("type A = B; type B = int; def i: A = 1;");

        assert_eq!(result.diagnostics.len(), 2);

        assert!(matches!(
            &result.diagnostics[0],
            &crate::TypeError::UnboundTypeVariable { .. }
        ));

        assert!(matches!(
            &result.diagnostics[1],
            &crate::TypeError::TypeMismatch { .. }
        ));
    }

    #[test]
    fn test_infer_let_function_with_return_type() {
        let (module, mut types, result) = infer_from_str("def f { let f y : int = y; f }");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);

        let int_to_int = arrow(&mut types, int, int);
        assert_types_eq(actual_defn, int_to_int, &types);
        assert_types_eq(actual_body, int_to_int, &types);
    }

    #[test]
    fn test_infer_let_function_with_param_type() {
        let (module, mut types, result) = infer_from_str("def f { let f (y : int) = y; f }");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let int = types.intern(Type::Int);

        let int_to_int = arrow(&mut types, int, int);
        assert_types_eq(actual_defn, int_to_int, &types);
        assert_types_eq(actual_body, int_to_int, &types);
    }
}
