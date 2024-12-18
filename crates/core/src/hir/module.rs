use std::ops::Index;

use imbl::HashMap;
use la_arena::{Arena, ArenaMap};

use parser::{SyntaxNode, SyntaxNodePtr};

use crate::types::Type;
use crate::{builtin, TypeIdx};
use crate::{intern::Interner, Name};

use super::{
    Definition, DefinitionIdx, Expr, ExprIdx, Open, Param, ParamIdx, Pattern, PatternIdx,
    TypeDefinition, TypeDefinitionIdx, TypeExpr, TypeExprIdx,
};

#[derive(Debug)]
#[allow(unused)]
pub struct Module {
    definitions: Arena<Definition>,
    definitions_syntax: ArenaMap<DefinitionIdx, SyntaxNodePtr>,

    opens: Arena<Open>,

    type_definitions: Arena<TypeDefinition>,
    type_definitions_syntax: ArenaMap<TypeDefinitionIdx, SyntaxNodePtr>,

    expressions: Arena<Expr>,
    exprs_syntax: ArenaMap<ExprIdx, SyntaxNodePtr>,

    type_expressions: Arena<TypeExpr>,
    type_exprs_syntax: ArenaMap<TypeExprIdx, SyntaxNodePtr>,

    parameters: Arena<Param>,
    parameters_syntax: ArenaMap<ParamIdx, SyntaxNodePtr>,

    patterns: Arena<Pattern>,
    patterns_syntax: ArenaMap<PatternIdx, SyntaxNodePtr>,

    names: Interner<String>,

    known_definitions: HashMap<Name, TypeIdx>,
    known_types: HashMap<Name, TypeIdx>,
}

fn name_deep_eq(a_module: &Module, b_module: &Module, a: Name, b: Name) -> bool {
    a_module.names.lookup(a) == b_module.names.lookup(b)
}

fn pattern_deep_eq(a_module: &Module, b_module: &Module, a: PatternIdx, b: PatternIdx) -> bool {
    match (a_module[a], b_module[b]) {
        (Pattern::Ident(a), Pattern::Ident(b)) => name_deep_eq(a_module, b_module, a, b),
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
        (TypeExpr::IdentTypeExpr { name: a }, TypeExpr::IdentTypeExpr { name: b }) => {
            name_deep_eq(a_module, b_module, *a, *b)
        }
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
        (Expr::IdentExpr { name: a }, Expr::IdentExpr { name: b }) => {
            name_deep_eq(a_module, b_module, *a, *b)
        }
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
        }
        _ => false,
    }
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.definitions
            .values()
            .zip(other.definitions.values())
            .all(|(a, b)| {
                expr_deep_eq(self, other, a.defn, b.defn)
                    && name_deep_eq(self, other, a.name, b.name)
            })
            && self
                .opens
                .values()
                .zip(other.opens.values())
                .all(|(a, b)| name_deep_eq(self, other, a.path, b.path))
            && self
                .type_definitions
                .values()
                .zip(other.type_definitions.values())
                .all(|(a, b)| {
                    name_deep_eq(self, other, a.name, b.name)
                        && type_expr_deep_eq(self, other, a.defn, b.defn)
                })
    }
}

#[allow(unused)]
impl Module {
    #[must_use]
    pub fn new(types: &mut Interner<Type>) -> Self {
        let mut module = Self {
            expressions: Arena::new(),
            type_expressions: Arena::new(),
            definitions: Arena::new(),
            opens: Arena::new(),
            type_definitions: Arena::new(),
            parameters: Arena::new(),
            parameters_syntax: ArenaMap::new(),
            patterns: Arena::new(),
            patterns_syntax: ArenaMap::new(),
            names: Interner::new(),
            exprs_syntax: ArenaMap::new(),
            type_exprs_syntax: ArenaMap::new(),
            definitions_syntax: ArenaMap::new(),
            type_definitions_syntax: ArenaMap::new(),
            known_definitions: HashMap::new(),
            known_types: HashMap::new(),
        };

        module.load_builtin(types);
        module
    }

    fn load_builtin(&mut self, types: &mut Interner<Type>) {
        self.known_definitions = builtin::builtin_defs(self, types);
        self.known_types = builtin::builtin_types(self, types);
    }

    pub(super) fn alloc_missing<T: Missable>(&mut self) -> la_arena::Idx<T> {
        T::missing().alloc_no_syntax(self)
    }

    pub(super) fn empty_name(&mut self) -> Name {
        self.names.intern("_".into())
    }

    pub(crate) fn name<S: Into<String>>(&mut self, name: S) -> Name {
        self.names.intern(name.into())
    }

    #[must_use]
    pub fn get_name(&self, name: Name) -> &str {
        self.names.lookup(name)
    }

    pub(crate) fn iter_definitions(&self) -> impl Iterator<Item = (DefinitionIdx, &Definition)> {
        self.definitions.iter()
    }

    pub fn type_definitions(
        &self,
    ) -> impl Iterator<Item = (TypeDefinitionIdx, &TypeDefinition)> + '_ {
        self.type_definitions.iter()
    }

    #[must_use]
    pub fn get_known_types(&self) -> HashMap<Name, TypeIdx> {
        self.known_types.clone()
    }

    #[must_use]
    pub fn get_known_definitions(&self) -> HashMap<Name, TypeIdx> {
        self.known_definitions.clone()
    }

    pub(super) fn iter_type_definitions(
        &self,
    ) -> impl Iterator<Item = (TypeDefinitionIdx, &TypeDefinition)> {
        self.type_definitions.iter()
    }

    pub(super) fn iter_expressions(&self) -> impl Iterator<Item = (ExprIdx, &Expr)> {
        self.expressions.iter()
    }

    pub(super) fn iter_type_expressions(&self) -> impl Iterator<Item = (TypeExprIdx, &TypeExpr)> {
        self.type_expressions.iter()
    }

    #[must_use]
    pub fn syntax<'a, T: StoredInArena + 'a>(
        &'a self,
        idx: la_arena::Idx<T>,
    ) -> Option<&SyntaxNodePtr> {
        T::syntax_map(self).get(idx)
    }
}

impl<T: StoredInArena> Index<la_arena::Idx<T>> for Module {
    type Output = T;

    fn index(&self, index: la_arena::Idx<T>) -> &T {
        &T::arena(self)[index]
    }
}

pub trait StoredInArena: std::marker::Sized {
    fn arena(module: &Module) -> &Arena<Self>;
    fn arena_mut(module: &mut Module) -> &mut Arena<Self>;
    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr>;
    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr>;

    fn alloc_opt_syntax(
        self,
        module: &mut Module,
        syntax: Option<&SyntaxNode>,
    ) -> la_arena::Idx<Self> {
        let idx = Self::arena_mut(module).alloc(self);
        if let Some(syntax) = syntax {
            Self::syntax_map_mut(module).insert(idx, SyntaxNodePtr::new(syntax));
        }
        idx
    }

    fn alloc<N: parser::AstNode>(self, module: &mut Module, node: &N) -> la_arena::Idx<Self> {
        self.alloc_opt_syntax(module, Some(parser::AstNode::syntax(node)))
    }

    fn alloc_no_syntax(self, module: &mut Module) -> la_arena::Idx<Self> {
        self.alloc_opt_syntax(module, None)
    }
}

pub(super) trait Missable: StoredInArena {
    fn missing() -> Self;
}

impl StoredInArena for Expr {
    fn arena(module: &Module) -> &Arena<Self> {
        &module.expressions
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.expressions
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &module.exprs_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &mut module.exprs_syntax
    }
}

impl Missable for Expr {
    fn missing() -> Self {
        Expr::Missing
    }
}

impl StoredInArena for TypeExpr {
    fn arena(module: &Module) -> &Arena<Self> {
        &module.type_expressions
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.type_expressions
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &module.type_exprs_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &mut module.type_exprs_syntax
    }
}

impl Missable for TypeExpr {
    fn missing() -> Self {
        TypeExpr::Missing
    }
}

impl StoredInArena for Param {
    fn arena(module: &Module) -> &Arena<Self> {
        &module.parameters
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.parameters
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &module.parameters_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &mut module.parameters_syntax
    }
}

impl StoredInArena for Pattern {
    fn arena(module: &Module) -> &Arena<Self> {
        &module.patterns
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.patterns
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &module.patterns_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &mut module.patterns_syntax
    }
}

impl StoredInArena for Definition {
    fn arena(module: &Module) -> &Arena<Self> {
        &module.definitions
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.definitions
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &module.definitions_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &mut module.definitions_syntax
    }
}

impl StoredInArena for TypeDefinition {
    fn arena(module: &Module) -> &Arena<Self> {
        &module.type_definitions
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.type_definitions
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &module.type_definitions_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, SyntaxNodePtr> {
        &mut module.type_definitions_syntax
    }
}
