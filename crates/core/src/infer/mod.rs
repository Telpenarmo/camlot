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
    types: Interner<Type>,
}

enum Constraint {
    TypeEqual(ExprIdx, TypeIdx, TypeIdx),
}

type Environment = im::HashMap<Name, TypeIdx>;

pub struct InferenceResult {
    pub types: Interner<Type>,
    pub expr_types: ArenaMap<ExprIdx, TypeIdx>,
    pub defn_types: ArenaMap<DefinitionIdx, TypeIdx>,
    pub diagnostics: Vec<Diagnostic>,
}

#[must_use]
pub fn infer(module: &Module) -> InferenceResult {
    let inference = TypeInference::new();
    inference.infer(module)
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
            types: Interner::new(),
        }
    }

    fn curry(&mut self, params: &[TypeIdx], return_type: TypeIdx) -> TypeIdx {
        params.iter().rev().fold(return_type, |acc, param| {
            self.types.intern(Type::Arrow(*param, acc))
        })
    }

    fn generate_env(&mut self, module: &Module) -> Environment {
        // Walk through the definitions and register them in the environment as fresh unification variables
        module
            .iter_definitions()
            .map(|(_, defn)| (defn.name, self.next_unification_var()))
            .collect()
    }

    fn infer(mut self, module: &Module) -> InferenceResult {
        let initial_env = self.generate_env(module);

        for (idx, defn) in module.iter_definitions() {
            self.check_definition(&initial_env, module, idx, defn);
        }

        self.diagnostics.extend(
            unify::Unifcation::new(&mut self.unification_table, &self.types)
                .unify(self.constraints)
                .into_iter()
                .map(|(expr, error)| Diagnostic::UnifcationError { expr, error }),
        );

        Self::substitute(
            &mut self.types,
            &mut self.expr_types,
            &mut self.defn_types,
            &mut self.unification_table,
        );

        InferenceResult {
            types: self.types,
            expr_types: self.expr_types,
            defn_types: self.defn_types,
            diagnostics: self.diagnostics,
        }
    }

    fn check_definition(
        &mut self,
        initial_env: &Environment,
        module: &Module,
        idx: DefinitionIdx,
        defn: &Definition,
    ) {
        let ret_type = self.resolve_type_expr(module, defn.return_type);
        let mut def_env = initial_env.clone();
        let params: Vec<_> = defn
            .params
            .iter()
            .map(|param| {
                let typ = self.resolve_type_expr(module, param.typ);
                def_env = def_env.update(param.name, typ);
                typ
            })
            .collect();

        self.check_expr(module, defn.defn, &def_env, ret_type);

        let typ = self.curry(&params, ret_type);

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

    fn next_unification_var(&mut self) -> TypeIdx {
        let var = self.unification_table.new_key(None);
        self.types.intern(Type::Unifier(var))
    }

    fn infer_expr(&mut self, module: &Module, env: &Environment, expr: ExprIdx) -> TypeIdx {
        let typ = self.infer_expr_impl(module, env, expr);
        self.expr_types.insert(expr, typ);
        typ
    }

    fn infer_expr_impl(&mut self, module: &Module, env: &Environment, expr: ExprIdx) -> TypeIdx {
        match module.get_expr(expr) {
            Expr::Missing => self.next_unification_var(),
            Expr::LetExpr(let_expr) => {
                let def_typ = self.resolve_type_expr(module, let_expr.return_type);
                let mut def_env = env.clone();
                for param in &let_expr.params {
                    let typ = self.resolve_type_expr(module, param.typ);
                    def_env = def_env.update(param.name, typ);
                }
                self.check_expr(module, let_expr.defn, &def_env, def_typ);

                let env = env.update(let_expr.name, def_typ);
                self.infer_expr(module, &env, let_expr.body)
            }
            Expr::IdentExpr { name } => {
                if let Some(typ) = env.get(name) {
                    *typ
                } else {
                    self.diagnostics
                        .push(Diagnostic::UnboundVariable { expr, name: *name });
                    error(&mut self.types)
                }
            }
            Expr::LambdaExpr(lambda) => {
                let from = self.resolve_type_expr(module, lambda.param.typ);

                let env = env.update(lambda.param.name, from);
                let to = self.resolve_type_expr(module, lambda.return_type);
                self.check_expr(module, lambda.body, &env, to);

                arrow(&mut self.types, from, to)
            }
            &Expr::AppExpr {
                func: lhs,
                arg: rhs,
            } => {
                let lhs_typ = self.infer_expr(module, env, lhs);

                let arg_typ = self.next_unification_var();
                let ret_typ = self.next_unification_var();

                let func_typ = arrow(&mut self.types, arg_typ, ret_typ);

                self.constraints
                    .push(Constraint::TypeEqual(lhs, func_typ, lhs_typ));

                self.check_expr(module, rhs, env, arg_typ);

                ret_typ
            }
            Expr::LiteralExpr(lit) => self.types.intern(match lit {
                Literal::Unit => Type::Unit,
                Literal::IntLiteral(_) => Type::Int,
                Literal::BoolLiteral(_) => Type::Bool,
            }),
        }
    }

    fn check_expr(&mut self, module: &Module, expr: ExprIdx, env: &Environment, typ: TypeIdx) {
        let expected = self.types.lookup(typ);
        match (module.get_expr(expr), expected) {
            (Expr::LiteralExpr(Literal::BoolLiteral(_)), Type::Bool)
            | (Expr::LiteralExpr(Literal::IntLiteral(_)), Type::Int)
            | (Expr::LiteralExpr(Literal::Unit), Type::Unit) => {}

            (Expr::LambdaExpr(lambda), Type::Arrow(from, to)) => {
                let env = env.update(lambda.param.name, *from);
                self.check_expr(module, lambda.body, &env, *to);
            }

            _ => {
                let actual = self.infer_expr(module, env, expr);
                self.constraints
                    .push(Constraint::TypeEqual(expr, typ, actual));
            }
        }
    }

    fn resolve_type_expr(
        &mut self,
        module: &Module,
        ty_idx: TypeExprIdx,
    ) -> TypeIdx {
        let ty = module.get_type_expr(ty_idx);
        match ty {
            TypeExpr::Missing => self.next_unification_var(),
            &TypeExpr::IdentTypeExpr { name } => {
                self.diagnostics.push(Diagnostic::UnboundTypeVariable {
                    type_expr: ty_idx,
                    name,
                });
                error(&mut self.types)
            }
            &TypeExpr::TypeArrow { from, to } => {
                let from = self.resolve_type_expr(module, from);
                let to = self.resolve_type_expr(module, to);
                arrow(&mut self.types, from, to)
            }
        }
    }
}

fn error(types: &mut Interner<Type>) -> TypeIdx {
    types.intern(Type::Error)
}

fn arrow(types: &mut Interner<Type>, from: TypeIdx, to: TypeIdx) -> TypeIdx {
    types.intern(Type::Arrow(from, to))
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::{InferenceResult, TypeInference};
    use crate::{hir, infer::arrow, intern::Interner, types::Type, TypeIdx};

    fn infer_from_str(code: &str) -> (hir::Module, InferenceResult) {
        let infer = TypeInference::new();
        let module_ast = parser::parse(code).module();
        let mut module = hir::Module::new();
        module.lower_module(&module_ast);
        let result = infer.infer(&module);
        (module, result)
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
        let (module, result) = infer_from_str("def f(x: int) = x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let mut types = result.types;

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, int, &types);
    }

    #[test]
    fn test_infer_def_lambda_with_annotated_param() {
        let (module, result) = infer_from_str("def f = \\(x: int) -> x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let mut types = result.types;

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, expected, &types);
    }

    #[test]
    fn test_infer_def_lambda_with_return_type() {
        let (module, result) = infer_from_str("def f = \\x : int -> x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let mut types = result.types;

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, expected, &types);
    }

    #[test]
    fn test_infer_annotated_def_lambda() {
        let (module, result) = infer_from_str("def f : int -> int = \\x -> x;");

        assert_empty(&result.diagnostics);

        let (actual_defn, actual_body) = get_first_defn_types(&module, &result);

        let mut types = result.types;

        let int = types.intern(Type::Int);
        let expected = arrow(&mut types, int, int);

        assert_types_eq(actual_defn, expected, &types);
        assert_types_eq(actual_body, expected, &types);
    }
}
