use std::ops::Index;

use imbl::HashMap;
use la_arena::{Arena, ArenaMap};

use parser::{
    DefinitionPtr, ExprPtr, ParamPtr, PatternPtr, SyntaxNode, SyntaxNodePtr, TypeDefinitionPtr,
    TypeExprPtr,
};

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
    definitions_syntax: ArenaMap<DefinitionIdx, DefinitionPtr>,

    opens: Arena<Open>,

    type_definitions: Arena<TypeDefinition>,
    type_definitions_syntax: ArenaMap<TypeDefinitionIdx, TypeDefinitionPtr>,

    expressions: Arena<Expr>,
    exprs_syntax: ArenaMap<ExprIdx, ExprPtr>,

    type_expressions: Arena<TypeExpr>,
    type_exprs_syntax: ArenaMap<TypeExprIdx, TypeExprPtr>,

    parameters: Arena<Param>,
    parameters_syntax: ArenaMap<ParamIdx, ParamPtr>,

    patterns: Arena<Pattern>,
    patterns_syntax: ArenaMap<PatternIdx, PatternPtr>,

    names: Interner<String>,

    known_definitions: HashMap<Name, TypeIdx>,
    known_types: HashMap<Name, TypeIdx>,
}

fn name_deep_eq(a_module: &Module, b_module: &Module, a: Name, b: Name) -> bool {
    a_module.names.lookup(a) == b_module.names.lookup(b)
}

fn pattern_deep_eq(a_module: &Module, b_module: &Module, a: PatternIdx, b: PatternIdx) -> bool {
    let a = a_module.get_pattern(a);
    let b = b_module.get_pattern(b);
    match (a, b) {
        (Pattern::Ident(a), Pattern::Ident(b)) => name_deep_eq(a_module, b_module, *a, *b),
        (Pattern::Wildcard, Pattern::Wildcard) | (Pattern::Unit, Pattern::Unit) => true,
        _ => false,
    }
}

fn param_deep_eq(a_module: &Module, b_module: &Module, a: ParamIdx, b: ParamIdx) -> bool {
    let a = a_module.get_param(a);
    let b = b_module.get_param(b);
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

    pub(super) fn alloc_definition(&mut self, definition: Definition) -> DefinitionIdx {
        self.definitions.alloc(definition)
    }

    pub(crate) fn get_definition(&self, idx: DefinitionIdx) -> &Definition {
        &self.definitions[idx]
    }

    pub(super) fn alloc_type_definition(
        &mut self,
        type_definition: TypeDefinition,
    ) -> TypeDefinitionIdx {
        self.type_definitions.alloc(type_definition)
    }

    pub(crate) fn get_type_definition(&self, idx: TypeDefinitionIdx) -> &TypeDefinition {
        &self.type_definitions[idx]
    }

    pub(super) fn alloc_type_expr(&mut self, type_expr: TypeExpr) -> TypeExprIdx {
        self.type_expressions.alloc(type_expr)
    }

    pub(crate) fn get_type_expr(&self, idx: TypeExprIdx) -> &TypeExpr {
        &self.type_expressions[idx]
    }

    pub(super) fn alloc_param(&mut self, param: Param) -> ParamIdx {
        self.parameters.alloc(param)
    }

    pub(super) fn alloc_pattern(&mut self, pattern: Pattern) -> PatternIdx {
        self.patterns.alloc(pattern)
    }

    pub(super) fn empty_name(&mut self) -> Name {
        self.names.intern("_".into())
    }

    pub(crate) fn name<S: Into<String>>(&mut self, name: S) -> Name {
        self.names.intern(name.into())
    }

    pub(crate) fn get_name(&self, name: Name) -> &str {
        self.names.lookup(name)
    }

    pub(crate) fn get_param(&self, idx: ParamIdx) -> &Param {
        &self.parameters[idx]
    }

    pub(crate) fn get_pattern(&self, idx: PatternIdx) -> &Pattern {
        &self.patterns[idx]
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
}

impl<T: StoredInArena> Index<la_arena::Idx<T>> for Module {
    type Output = T;

    fn index(&self, index: la_arena::Idx<T>) -> &T {
        &T::arena(self)[index]
    }
}

pub(crate) trait StoredInArena: std::marker::Sized {
    fn arena(module: &Module) -> &Arena<Self>;
    fn arena_mut(module: &mut Module) -> &mut Arena<Self>;
    #[allow(unused)]
    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, Self::SyntaxPtr>;
    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, Self::SyntaxPtr>;
    fn make_ptr(syntax: &Self::Syntax) -> Self::SyntaxPtr;

    type Syntax;
    type SyntaxPtr;

    fn alloc_opt_syntax(
        self,
        module: &mut Module,
        syntax: Option<&Self::Syntax>,
    ) -> la_arena::Idx<Self> {
        let idx = Self::arena_mut(module).alloc(self);
        if let Some(syntax) = syntax {
            Self::syntax_map_mut(module).insert(idx, Self::make_ptr(syntax));
        }
        idx
    }

    fn alloc(self, module: &mut Module, syntax: &Self::Syntax) -> la_arena::Idx<Self> {
        self.alloc_opt_syntax(module, Some(syntax))
    }

    fn alloc_no_syntax(self, module: &mut Module) -> la_arena::Idx<Self> {
        self.alloc_opt_syntax(module, None)
    }
}

pub(super) trait Missable: StoredInArena {
    fn missing() -> Self;
    fn alloc_missing(module: &mut Module) -> la_arena::Idx<Self> {
        Self::missing().alloc_no_syntax(module)
    }
}

impl StoredInArena for Expr {
    type Syntax = SyntaxNode;
    type SyntaxPtr = SyntaxNodePtr;

    fn arena(module: &Module) -> &Arena<Self> {
        &module.expressions
    }

    fn arena_mut(module: &mut Module) -> &mut Arena<Self> {
        &mut module.expressions
    }

    fn syntax_map(module: &Module) -> &ArenaMap<la_arena::Idx<Self>, Self::SyntaxPtr> {
        &module.exprs_syntax
    }

    fn syntax_map_mut(module: &mut Module) -> &mut ArenaMap<la_arena::Idx<Self>, Self::SyntaxPtr> {
        &mut module.exprs_syntax
    }

    fn make_ptr(syntax: &Self::Syntax) -> Self::SyntaxPtr {
        SyntaxNodePtr::new(syntax)
    }
}

impl Missable for Expr {
    fn missing() -> Self {
        Expr::Missing
    }
}
