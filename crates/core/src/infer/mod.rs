mod unify;

use im::HashMap;
use la_arena::ArenaMap;
use unify::UnifcationError;

use crate::hir::{Expr, ExprIdx, Literal, Module, TypeExpr, TypeExprIdx};
use crate::intern::{Interned, Interner};
use crate::types::{Type, TypeIdx, UnificationTable};
use crate::{Definition, DefinitionIdx, Name};

#[derive(PartialEq, Debug)]
pub enum Diagnostic {
    TypeMismatch {
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
    UnifcationError {
        expr: ExprIdx,
        error: UnifcationError,
    },
}

pub struct TypeInference {
    unification_table: UnificationTable,
    defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    expr_types: ArenaMap<ExprIdx, TypeIdx>,
    constraints: Vec<Constraint>,
    diagnostics: Vec<Diagnostic>,
    types_env: Environment,
}

enum Constraint {
    TypeEqual(ExprIdx, TypeIdx, TypeIdx),
}

type Environment = im::HashMap<Name, TypeIdx>;

pub struct InferenceResult {
    pub expr_types: ArenaMap<ExprIdx, TypeIdx>,
    pub defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    pub diagnostics: Vec<Diagnostic>,
}

#[must_use]
pub fn infer(module: &Module, types: &mut Interner<Type>) -> InferenceResult {
    let inference = TypeInference::new();
    inference.infer(module, types)
}

impl TypeInference {
    #[must_use]
    pub fn new() -> Self {
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
            let typ = self.resolve_type_expr(module, types, defn.defn);
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
                .map(|(expr, error)| Diagnostic::UnifcationError { expr, error }),
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
                let typ = self.resolve_type_expr(module, types, param.typ);
                def_env = def_env.update(param.name, typ);
                typ
            })
            .collect();

        self.check_expr(module, types, defn.defn, &def_env, ret_type);

        let typ = curry(types, &params, ret_type);

        self.expr_types.insert(defn.defn, ret_type);
        self.defn_types.insert(idx, typ);
    }

    fn get_new_type(
        types: &mut Interner<Type>,
        old_to_new: &mut HashMap<TypeIdx, TypeIdx>,
        unification_table: &mut UnificationTable,
        idx: TypeIdx,
    ) -> Interned<Type> {
        if let Some(x) = old_to_new.get(&idx) {
            *x
        } else {
            let res = Self::substitute_type(types, old_to_new, unification_table, idx);
            old_to_new.insert(idx, res);
            res
        }
    }

    fn substitute_type(
        types: &mut Interner<Type>,
        old_to_new: &mut HashMap<TypeIdx, TypeIdx>,
        unification_table: &mut UnificationTable,
        idx: TypeIdx,
    ) -> TypeIdx {
        match types.lookup(idx) {
            Type::Unifier(u) => unification_table.probe_value(*u).map_or(idx, |idx| {
                Self::get_new_type(types, old_to_new, unification_table, idx)
            }),
            &Type::Arrow(from, to) => {
                let from = Self::get_new_type(types, old_to_new, unification_table, from);
                let to = Self::get_new_type(types, old_to_new, unification_table, to);
                arrow(types, from, to)
            }
            _ => idx,
        }
    }

    fn substitute(
        types: &mut Interner<Type>,
        expr_types: &mut ArenaMap<ExprIdx, TypeIdx>,
        defn_types: &mut ArenaMap<DefinitionIdx, TypeIdx>,
        unification_table: &mut UnificationTable,
    ) {
        let mut old_to_new = HashMap::new();

        expr_types.values_mut().for_each(|idx| {
            *idx = Self::get_new_type(types, &mut old_to_new, unification_table, *idx);
        });

        defn_types.values_mut().for_each(|idx| {
            *idx = Self::get_new_type(types, &mut old_to_new, unification_table, *idx);
        });
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
        match module.get_expr(expr) {
            Expr::Missing => self.next_unification_var(types),
            Expr::LetExpr(let_expr) => {
                let def_typ = self.resolve_type_expr(module, types, let_expr.return_type);
                let mut def_env = env.clone();
                for param in &let_expr.params {
                    let typ = self.resolve_type_expr(module, types, param.typ);
                    def_env = def_env.update(param.name, typ);
                }
                self.check_expr(module, types, let_expr.defn, &def_env, def_typ);

                let env = env.update(let_expr.name, def_typ);
                self.infer_expr(module, types, &env, let_expr.body)
            }
            Expr::IdentExpr { name } => {
                if let Some(typ) = env.get(name) {
                    *typ
                } else {
                    self.diagnostics
                        .push(Diagnostic::UnboundVariable { expr, name: *name });
                    error(types)
                }
            }
            Expr::LambdaExpr(lambda) => {
                let from = self.resolve_type_expr(module, types, lambda.param.typ);

                let env = env.update(lambda.param.name, from);
                let to = self.resolve_type_expr(module, types, lambda.return_type);
                self.check_expr(module, types, lambda.body, &env, to);

                arrow(types, from, to)
            }
            &Expr::AppExpr {
                func: lhs,
                arg: rhs,
            } => {
                let lhs_typ = self.infer_expr(module, types, env, lhs);

                let arg_typ = self.next_unification_var(types);
                let ret_typ = self.next_unification_var(types);

                let func_typ = arrow(types, arg_typ, ret_typ);

                self.constraints
                    .push(Constraint::TypeEqual(lhs, func_typ, lhs_typ));

                self.check_expr(module, types, rhs, env, arg_typ);

                ret_typ
            }
            Expr::LiteralExpr(lit) => types.intern(match lit {
                Literal::Unit => Type::Unit,
                Literal::IntLiteral(_) => Type::Int,
                Literal::BoolLiteral(_) => Type::Bool,
            }),
        }
    }

    fn check_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        expr: ExprIdx,
        env: &Environment,
        typ: TypeIdx,
    ) {
        let expected = types.lookup(typ);
        match (module.get_expr(expr), expected) {
            (Expr::LiteralExpr(Literal::BoolLiteral(_)), Type::Bool)
            | (Expr::LiteralExpr(Literal::IntLiteral(_)), Type::Int)
            | (Expr::LiteralExpr(Literal::Unit), Type::Unit) => {}

            (Expr::LambdaExpr(lambda), Type::Arrow(from, to)) => {
                let env = env.update(lambda.param.name, *from);
                self.check_expr(module, types, lambda.body, &env, *to);
            }

            _ => {
                let actual = self.infer_expr(module, types, env, expr);
                self.constraints
                    .push(Constraint::TypeEqual(expr, typ, actual));
            }
        }
    }

    fn resolve_type_expr(
        &mut self,
        module: &Module,
        types: &mut Interner<Type>,
        ty_idx: TypeExprIdx,
    ) -> TypeIdx {
        let ty = module.get_type_expr(ty_idx);
        match ty {
            TypeExpr::Missing => self.next_unification_var(types),
            &TypeExpr::IdentTypeExpr { name } => self.types_env.get(&name).map_or_else(
                || {
                    self.diagnostics.push(Diagnostic::UnboundTypeVariable {
                        type_expr: ty_idx,
                        name,
                    });
                    error(types)
                },
                |typ| *typ,
            ),
            &TypeExpr::TypeArrow { from, to } => {
                let from = self.resolve_type_expr(module, types, from);
                let to = self.resolve_type_expr(module, types, to);
                arrow(types, from, to)
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
            &crate::Diagnostic::UnboundTypeVariable { .. }
        ));

        assert!(matches!(
            &result.diagnostics[1],
            &crate::Diagnostic::UnifcationError { .. }
        ));
    }
}
