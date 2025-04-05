use std::collections::HashMap;
use std::ops::Index;

use la_arena::{Arena, ArenaMap};

use parser::{SyntaxNode, SyntaxNodePtr};

use crate::types::Type;
use crate::{builtin, TypeIdx};
use crate::{intern::Interner, Name};

use super::{
    Definition, DefinitionIdx, Expr, ExprIdx, Param, ParamIdx, Pattern, PatternIdx, TypeDefinition,
    TypeDefinitionIdx, TypeExpr, TypeExprIdx,
};

#[derive(Debug)]
struct HirNodeStorage<T> {
    arena: Arena<T>,
    syntax_of_hir: ArenaMap<la_arena::Idx<T>, SyntaxNodePtr>,
    hir_of_syntax: HashMap<SyntaxNodePtr, la_arena::Idx<T>>,
}

impl<T> HirNodeStorage<T> {
    fn new() -> Self {
        Self {
            arena: Arena::new(),
            syntax_of_hir: ArenaMap::new(),
            hir_of_syntax: HashMap::new(),
        }
    }
}

#[derive(Debug)]
#[allow(unused)]
pub struct Module {
    definitions: HirNodeStorage<Definition>,
    type_definitions: HirNodeStorage<TypeDefinition>,
    expressions: HirNodeStorage<Expr>,
    type_expressions: HirNodeStorage<TypeExpr>,
    parameters: HirNodeStorage<Param>,
    patterns: HirNodeStorage<Pattern>,
    known_definitions: imbl::HashMap<Name, TypeIdx>,
    known_types: imbl::HashMap<Name, TypeIdx>,
}

fn pattern_deep_eq(a_module: &Module, b_module: &Module, a: PatternIdx, b: PatternIdx) -> bool {
    match (a_module[a], b_module[b]) {
        (Pattern::Ident(a), Pattern::Ident(b)) => a == b,
        (Pattern::Wildcard, Pattern::Wildcard) | (Pattern::Unit, Pattern::Unit) => true,
        _ => false,
    }
}

fn param_deep_eq(a_module: &Module, b_module: &Module, a: ParamIdx, b: ParamIdx) -> bool {
    let a = &a_module[a];
    let b = &b_module[b];
    pattern_deep_eq(a_module, b_module, a.pattern, b.pattern)
        && type_expr_deep_eq(a_module, b_module, a.typ, b.typ)
}

pub(super) fn type_expr_deep_eq(
    a_module: &Module,
    b_module: &Module,
    a: TypeExprIdx,
    b: TypeExprIdx,
) -> bool {
    match (&a_module[a], &b_module[b]) {
        (TypeExpr::Missing, TypeExpr::Missing) => true,
        (TypeExpr::IdentTypeExpr { name: a }, TypeExpr::IdentTypeExpr { name: b }) => a == b,
        (
            TypeExpr::TypeArrow { from, to },
            TypeExpr::TypeArrow {
                from: b_from,
                to: b_to,
            },
        ) => {
            type_expr_deep_eq(a_module, b_module, *from, *b_from)
                && type_expr_deep_eq(a_module, b_module, *to, *b_to)
        }
        _ => false,
    }
}

pub(super) fn expr_deep_eq(a_module: &Module, b_module: &Module, a: ExprIdx, b: ExprIdx) -> bool {
    match (&a_module[a], &b_module[b]) {
        (Expr::Missing, Expr::Missing) => true,
        (Expr::LiteralExpr(a), Expr::LiteralExpr(b)) => a == b,
        (Expr::IdentExpr { name: a }, Expr::IdentExpr { name: b }) => a == b,
        (
            Expr::AppExpr { func, arg },
            Expr::AppExpr {
                func: b_func,
                arg: b_arg,
            },
        ) => {
            expr_deep_eq(a_module, b_module, *func, *b_func)
                && expr_deep_eq(a_module, b_module, *arg, *b_arg)
        }
        (Expr::LambdaExpr(l_lambda), Expr::LambdaExpr(b_lambda)) => {
            param_deep_eq(a_module, b_module, l_lambda.param, b_lambda.param)
                && expr_deep_eq(a_module, b_module, l_lambda.body, b_lambda.body)
                && type_expr_deep_eq(
                    a_module,
                    b_module,
                    l_lambda.return_type,
                    b_lambda.return_type,
                )
        }
        (Expr::LetExpr(l_let), Expr::LetExpr(b_let)) => {
            pattern_deep_eq(a_module, b_module, l_let.lhs, b_let.lhs)
                && type_expr_deep_eq(a_module, b_module, l_let.return_type, b_let.return_type)
                && expr_deep_eq(a_module, b_module, l_let.body, b_let.body)
                && expr_deep_eq(a_module, b_module, l_let.defn, b_let.defn)
                && l_let.type_params == b_let.type_params
        }
        _ => false,
    }
}

fn slice_deep_eq<T, F: Fn(&T, &T) -> bool>(a: &[T], b: &[T], eq: F) -> bool {
    a.len() == b.len() && a.iter().enumerate().all(|(i, v)| eq(v, &b[i]))
}

fn defn_deep_eq(a_module: &Module, b_module: &Module, a: DefinitionIdx, b: DefinitionIdx) -> bool {
    let a = &a_module[a];
    let b = &b_module[b];

    a.name == b.name
        && expr_deep_eq(a_module, b_module, a.defn, b.defn)
        && slice_deep_eq(&a.params, &b.params, |&a, &b| {
            param_deep_eq(a_module, b_module, a, b)
        })
        && a.type_params == b.type_params
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.definitions
            .arena
            .iter()
            .zip(other.definitions.arena.iter())
            .all(|((a, _), (b, _))| defn_deep_eq(self, other, a, b))
            && self
                .type_definitions
                .arena
                .values()
                .zip(other.type_definitions.arena.values())
                .all(|(a, b)| a.name == b.name && type_expr_deep_eq(self, other, a.defn, b.defn))
    }
}

#[allow(unused)]
impl Module {
    #[must_use]
    pub fn new(names: &mut Interner<String>, types: &mut Interner<Type>) -> Self {
        let mut module = Self {
            expressions: HirNodeStorage::new(),
            type_expressions: HirNodeStorage::new(),
            definitions: HirNodeStorage::new(),
            type_definitions: HirNodeStorage::new(),
            parameters: HirNodeStorage::new(),
            patterns: HirNodeStorage::new(),
            known_definitions: imbl::HashMap::new(),
            known_types: imbl::HashMap::new(),
        };

        module.load_builtin(names, types);
        module
    }

    fn load_builtin(&mut self, names: &mut Interner<String>, types: &mut Interner<Type>) {
        self.known_definitions = builtin::builtin_defs(names, types);
        self.known_types = builtin::builtin_types(names, types);
    }

    pub(super) fn alloc_missing<T: Missable>(&mut self) -> la_arena::Idx<T> {
        T::missing().alloc_no_syntax(self)
    }

    pub(crate) fn iter_definitions(&self) -> impl Iterator<Item = (DefinitionIdx, &Definition)> {
        self.definitions.arena.iter()
    }

    pub fn type_definitions(
        &self,
    ) -> impl Iterator<Item = (TypeDefinitionIdx, &TypeDefinition)> + '_ {
        self.type_definitions.arena.iter()
    }

    #[must_use]
    pub fn get_known_types(&self) -> imbl::HashMap<Name, TypeIdx> {
        self.known_types.clone()
    }

    #[must_use]
    pub fn get_known_definitions(&self) -> imbl::HashMap<Name, TypeIdx> {
        self.known_definitions.clone()
    }

    pub(super) fn iter_type_definitions(
        &self,
    ) -> impl Iterator<Item = (TypeDefinitionIdx, &TypeDefinition)> {
        self.type_definitions.arena.iter()
    }

    pub(super) fn iter_expressions(&self) -> impl Iterator<Item = (ExprIdx, &Expr)> {
        self.expressions.arena.iter()
    }

    pub(super) fn iter_type_expressions(&self) -> impl Iterator<Item = (TypeExprIdx, &TypeExpr)> {
        self.type_expressions.arena.iter()
    }

    #[must_use]
    pub fn syntax<'a, T: HirNode + 'a>(&'a self, idx: la_arena::Idx<T>) -> Option<SyntaxNodePtr> {
        T::get_syntax(self, idx)
    }

    #[must_use]
    pub fn idx_at<T: HirNode>(&self, syntax: &SyntaxNode) -> Option<la_arena::Idx<T>> {
        let ptr = SyntaxNodePtr::new(syntax);
        T::get_at_syntax(self, ptr)
    }
}

impl<T: HirNodeInternal> Index<la_arena::Idx<T>> for Module {
    type Output = T;

    fn index(&self, index: la_arena::Idx<T>) -> &T {
        &T::storage(self).arena[index]
    }
}

trait HirNodeInternal: std::marker::Sized {
    fn storage(module: &Module) -> &HirNodeStorage<Self>;
    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self>;
}

pub trait HirNode: std::marker::Sized {
    fn get_at_syntax(module: &Module, ptr: SyntaxNodePtr) -> Option<la_arena::Idx<Self>>;
    fn alloc_opt(self, module: &mut Module, syntax: Option<&SyntaxNode>) -> la_arena::Idx<Self>;
    fn get_syntax(module: &Module, idx: la_arena::Idx<Self>) -> Option<SyntaxNodePtr>;

    fn alloc<N: parser::AstNode>(self, module: &mut Module, node: &N) -> la_arena::Idx<Self> {
        self.alloc_opt(module, Some(parser::AstNode::syntax(node)))
    }

    fn alloc_no_syntax(self, module: &mut Module) -> la_arena::Idx<Self> {
        self.alloc_opt(module, None)
    }
}

impl<T: HirNodeInternal> HirNode for T {
    fn alloc_opt(self, module: &mut Module, syntax: Option<&SyntaxNode>) -> la_arena::Idx<Self> {
        let idx = Self::storage_mut(module).arena.alloc(self);
        if let Some(syntax) = syntax {
            let storage = Self::storage_mut(module);
            let ptr = SyntaxNodePtr::new(syntax);

            storage.syntax_of_hir.insert(idx, ptr);
            storage.hir_of_syntax.insert(ptr, idx);
        }
        idx
    }

    fn get_syntax(module: &Module, idx: la_arena::Idx<Self>) -> Option<SyntaxNodePtr> {
        Self::storage(module).syntax_of_hir.get(idx).copied()
    }

    fn get_at_syntax(module: &Module, ptr: SyntaxNodePtr) -> Option<la_arena::Idx<Self>> {
        Self::storage(module).hir_of_syntax.get(&ptr).copied()
    }
}

pub(super) trait Missable: HirNode {
    fn missing() -> Self;
}

impl HirNodeInternal for Expr {
    fn storage(module: &Module) -> &HirNodeStorage<Self> {
        &module.expressions
    }

    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self> {
        &mut module.expressions
    }
}

impl Missable for Expr {
    fn missing() -> Self {
        Expr::Missing
    }
}

impl HirNodeInternal for TypeExpr {
    fn storage(module: &Module) -> &HirNodeStorage<Self> {
        &module.type_expressions
    }

    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self> {
        &mut module.type_expressions
    }
}

impl Missable for TypeExpr {
    fn missing() -> Self {
        TypeExpr::Missing
    }
}

impl HirNodeInternal for Param {
    fn storage(module: &Module) -> &HirNodeStorage<Self> {
        &module.parameters
    }

    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self> {
        &mut module.parameters
    }
}

impl HirNodeInternal for Pattern {
    fn storage(module: &Module) -> &HirNodeStorage<Self> {
        &module.patterns
    }

    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self> {
        &mut module.patterns
    }
}

impl HirNodeInternal for Definition {
    fn storage(module: &Module) -> &HirNodeStorage<Self> {
        &module.definitions
    }

    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self> {
        &mut module.definitions
    }
}

impl HirNodeInternal for TypeDefinition {
    fn storage(module: &Module) -> &HirNodeStorage<Self> {
        &module.type_definitions
    }

    fn storage_mut(module: &mut Module) -> &mut HirNodeStorage<Self> {
        &mut module.type_definitions
    }
}
