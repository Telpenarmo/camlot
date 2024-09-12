mod unify;

use std::collections::HashMap;

use la_arena::ArenaMap;
use unify::UnifcationError;

use crate::hir::{Expr, ExprIdx, Literal, Module, TypeExpr, TypeExprIdx};
use crate::intern::Interner;
use crate::types::{Type, TypeIdx, UnificationTable};
use crate::Name;

pub enum Diagnostic {
    TypeMismatch {
        expected: TypeIdx,
        actual: TypeIdx,
    },
    UnboundVariable {
        expr: ExprIdx,
        name: Name,
    },
    UnifcationError {
        expr: ExprIdx,
        error: UnifcationError,
    },
}

pub struct TypeInference {
    unification_table: UnificationTable,
    expr_types: ArenaMap<ExprIdx, TypeIdx>,
    constraints: Vec<Constraint>,
    diagnostics: Vec<Diagnostic>,
    types: Interner<Type>,
}

enum Constraint {
    TypeEqual(ExprIdx, TypeIdx, TypeIdx),
}

type Environment = im::HashMap<Name, TypeIdx>;

impl TypeInference {
    #[must_use]
    pub fn new() -> Self {
        Self {
            unification_table: UnificationTable::new(),
            expr_types: ArenaMap::new(),
            constraints: Vec::new(),
            diagnostics: Vec::new(),
            types: Interner::new(),
        }
    }

    fn generate_env(&mut self, module: &Module) -> Environment {
        // Walk through the definitions and register them in the environment as fresh unification variables
        module
            .definitions()
            .map(|defn| (defn.name, self.next_unification_var()))
            .collect()
    }

    /// # Panics
    ///
    /// Panics if initial environment is not populated.
    #[must_use]
    pub fn infer(mut self, module: &Module) -> (ArenaMap<ExprIdx, TypeIdx>, Vec<Diagnostic>) {
        let initial_env = self.generate_env(module);

        for defn in module.definitions() {
            let expected = initial_env
                .get(&defn.name)
                .expect("Every definition should be in env");
            self.check_expr(module, defn.defn, &initial_env, *expected);
        }

        self.diagnostics.extend(
            unify::Unifcation::new(&mut self.unification_table, &self.expr_types, &self.types)
                .unify(self.constraints)
                .into_iter()
                .map(|(expr, error)| Diagnostic::UnifcationError { expr, error }),
        );

        let types = Self::substitute(&self.expr_types, &mut self.unification_table);

        (types, self.diagnostics)
    }

    fn substitute(
        types: &ArenaMap<ExprIdx, TypeIdx>,
        unification_table: &mut UnificationTable,
    ) -> ArenaMap<ExprIdx, TypeIdx> {
        todo!()
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
                let def_typ =
                    self.type_annotation_to_unification_item(module, let_expr.return_type);
                let mut def_env = env.clone();
                for param in let_expr.params.iter() {
                    let typ = self.type_annotation_to_unification_item(module, param.typ);
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
                    self.error()
                }
            }
            Expr::LambdaExpr(lambda) => {
                let from = self.type_annotation_to_unification_item(module, lambda.param.typ);

                let env = env.update(lambda.param.name, from);
                let to = self.type_annotation_to_unification_item(module, lambda.return_type);
                self.check_expr(module, lambda.body, &env, to);

                self.arrow(from, to)
            }
            &Expr::AppExpr {
                func: lhs,
                arg: rhs,
            } => {
                let lhs_typ = self.infer_expr(module, env, lhs);

                let arg_typ = self.next_unification_var();
                let ret_typ = self.next_unification_var();

                let func_typ = self.arrow(arg_typ, ret_typ);

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

    fn type_annotation_to_unification_item(&mut self, module: &Module, ty: TypeExprIdx) -> TypeIdx {
        let ty = module.get_type_expr(ty);
        match ty {
            TypeExpr::Missing => self.next_unification_var(),
            TypeExpr::IdentTypeExpr { name } => self.var(*name),
            TypeExpr::TypeArrow { from, to } => {
                let from = self.type_annotation_to_unification_item(module, *from);
                let to = self.type_annotation_to_unification_item(module, *to);
                self.arrow(from, to)
            }
        }
    }

    fn arrow(&mut self, from: TypeIdx, to: TypeIdx) -> TypeIdx {
        self.types.intern(Type::Arrow(from, to))
    }

    fn var(&mut self, name: Name) -> TypeIdx {
        self.types.intern(Type::Var(name))
    }

    fn error(&mut self) -> TypeIdx {
        self.types.intern(Type::Error)
    }
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}
