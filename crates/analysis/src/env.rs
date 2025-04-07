use core::{DefinitionIdx, Expr, ExprIdx, PatternIdx};
use std::collections::HashMap;

use line_index::TextSize;
use parser::{
    nodes::{self, ModuleItem},
    AstNode, SyntaxToken,
};

use crate::Environment;

impl crate::Document {
    pub(crate) fn env_at(&self, needle: TextSize) -> Environment {
        let hir = self.hir();

        let mut env = HashMap::new();

        let module = self.parsed().module();
        for it in module.module_items() {
            if let ModuleItem::Definition(ref def_syntax) = it {
                if def_syntax.ident_lit().is_none() {
                    continue;
                };
                let defn: DefinitionIdx = hir.idx_at(it.syntax()).unwrap();
                let defn = &hir[defn];

                env.insert(defn.name, def_syntax.ident_lit().unwrap());

                if it.syntax().text_range().contains_inclusive(needle) {
                    for &p in &defn.params {
                        let param = &self.hir()[p];
                        self.extend_env(&mut env, param.pattern);
                    }
                    self.env_inside_expr(&mut env, defn.defn, needle);
                    break;
                }
            }
        }

        env
    }

    fn env_inside_expr(&self, env: &mut Environment, scope: ExprIdx, needle: TextSize) {
        match &self.hir()[scope] {
            Expr::LetExpr(let_expr) => {
                if let_expr.rec {
                    self.extend_env(env, let_expr.lhs);
                }

                self.try_recurse(env, let_expr.defn, needle);
                self.extend_env(env, let_expr.lhs);
                self.env_inside_expr(env, let_expr.body, needle);
            }
            Expr::LambdaExpr(lambda_expr) => {
                let param = &self.hir()[lambda_expr.param];
                self.extend_env(env, param.pattern);

                self.env_inside_expr(env, lambda_expr.body, needle);
            }
            &Expr::AppExpr { func, arg } => {
                self.try_recurse(env, func, needle);
                self.try_recurse(env, arg, needle);
            }
            Expr::LiteralExpr(_) | Expr::IdentExpr { .. } | Expr::Missing => (),
        }
    }

    fn try_recurse(&self, env: &mut Environment, scope: ExprIdx, needle: TextSize) {
        if let Some(syntax) = self.hir().syntax(scope) {
            if syntax.text_range().contains_inclusive(needle) {
                self.env_inside_expr(env, scope, needle);
            }
        }
    }

    fn extend_env(&self, env: &mut Environment, pattern: PatternIdx) {
        if let Some(binding) = self.pattern_binding(pattern) {
            let name = self.names().idx_of(binding.text());
            env.insert(name, binding);
        }
    }

    fn pattern_binding(&self, pattern: core::PatternIdx) -> Option<SyntaxToken> {
        let syntax = self.hir().syntax(pattern)?;
        let syntax = self.ptr_to_node(syntax)?;
        match syntax {
            nodes::Pattern::IdentPattern(ident) => ident.ident_lit(),
            nodes::Pattern::UnderscorePattern(_) | nodes::Pattern::UnitPattern(_) => None,
        }
    }
}
