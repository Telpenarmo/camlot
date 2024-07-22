mod unify;

use std::collections::HashMap;

use unify::UnifcationError;

use crate::hir::{Expr, ExprIdx, Literal, Module, TypeExpr, TypeExprIdx};
use crate::types::{Type, UnificationTable};
use crate::Name;

pub enum Diagnostic {
    TypeMismatch {
        expected: Type,
        actual: Type,
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
    expr_types: HashMap<ExprIdx, Type>,
    constraints: Vec<Constraint>,
    diagnostics: Vec<Diagnostic>,
}

enum Constraint {
    TypeEqual(ExprIdx, Type, Type),
}

type Environment = im::HashMap<Name, Type>;

impl TypeInference {
    #[must_use]
    pub fn new() -> Self {
        Self {
            unification_table: UnificationTable::new(),
            expr_types: HashMap::new(),
            constraints: Vec::new(),
            diagnostics: Vec::new(),
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
    pub fn infer(mut self, module: &Module) -> (HashMap<ExprIdx, Type>, Vec<Diagnostic>) {
        let initial_env = self.generate_env(module);

        for defn in module.definitions() {
            let expected = initial_env
                .get(&defn.name)
                .expect("Every definition should be in env");
            self.check_expr(module, defn.defn, &initial_env, expected.clone());
        }

        self.diagnostics.extend(
            unify::Unifcation::new(&mut self.unification_table, &self.expr_types)
                .unify(self.constraints)
                .into_iter()
                .map(|(expr, error)| Diagnostic::UnifcationError { expr, error }),
        );

        let types = Self::substitute(&self.expr_types, &mut self.unification_table);

        (types, self.diagnostics)
    }

    fn substitute(
        types: &HashMap<ExprIdx, Type>,
        unification_table: &mut UnificationTable,
    ) -> HashMap<ExprIdx, Type> {
        todo!()
    }

    fn next_unification_var(&mut self) -> Type {
        let var = self.unification_table.new_key(None);
        Type::Unifier(var)
    }

    fn infer_expr(&mut self, module: &Module, env: &Environment, expr: ExprIdx) -> Type {
        let typ = self.infer_expr_impl(module, env, expr);
        self.expr_types.insert(expr, typ.clone());
        typ
    }

    fn infer_expr_impl(&mut self, module: &Module, env: &Environment, expr: ExprIdx) -> Type {
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
                self.check_expr(module, let_expr.defn, &def_env, def_typ.clone());

                let env = env.update(let_expr.name, def_typ);
                self.infer_expr(module, &env, let_expr.body)
            }
            Expr::IdentExpr { name } => {
                if let Some(typ) = env.get(name) {
                    typ.clone()
                } else {
                    self.diagnostics
                        .push(Diagnostic::UnboundVariable { expr, name: *name });
                    Type::Error
                }
            }
            Expr::LambdaExpr(lambda) => {
                let from = self.type_annotation_to_unification_item(module, lambda.param.typ);

                let env = env.update(lambda.param.name, from.clone());
                let to = self.type_annotation_to_unification_item(module, lambda.return_type);
                self.check_expr(module, lambda.body, &env, to.clone());

                Type::arrow(from, to)
            }
            &Expr::AppExpr {
                func: lhs,
                arg: rhs,
            } => {
                let lhs_typ = self.infer_expr(module, env, lhs);

                let arg_typ = self.next_unification_var();
                let ret_typ = self.next_unification_var();

                let func_typ = Type::arrow(arg_typ.clone(), ret_typ.clone());

                self.constraints
                    .push(Constraint::TypeEqual(lhs, func_typ, lhs_typ));

                self.check_expr(module, rhs, env, arg_typ);

                ret_typ
            }
            Expr::LiteralExpr(lit) => match lit {
                Literal::Unit => Type::Unit,
                Literal::IntLiteral(_) => Type::Int,
                Literal::BoolLiteral(_) => Type::Bool,
            },
        }
    }

    fn check_expr(&mut self, module: &Module, expr: ExprIdx, env: &Environment, typ: Type) {
        match (module.get_expr(expr), typ) {
            (Expr::LiteralExpr(Literal::BoolLiteral(_)), Type::Bool)
            | (Expr::LiteralExpr(Literal::IntLiteral(_)), Type::Int)
            | (Expr::LiteralExpr(Literal::Unit), Type::Unit) => {}

            (Expr::LambdaExpr(lambda), Type::Arrow(from, to)) => {
                let env = env.update(lambda.param.name, *from);
                self.check_expr(module, lambda.body, &env, *to);
            }

            (_expr, expected) => {
                let actual = self.infer_expr(module, env, expr);
                self.constraints
                    .push(Constraint::TypeEqual(expr, expected, actual));
            }
        }
    }

    fn type_annotation_to_unification_item(&mut self, module: &Module, ty: TypeExprIdx) -> Type {
        let ty = module.get_type_expr(ty);
        match ty {
            TypeExpr::Missing => self.next_unification_var(),
            TypeExpr::IdentTypeExpr { name } => Type::Var(*name),
            TypeExpr::TypeArrow { from, to } => {
                let from = self.type_annotation_to_unification_item(module, *from);
                let to = self.type_annotation_to_unification_item(module, *to);
                Type::arrow(from, to)
            }
        }
    }
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}
