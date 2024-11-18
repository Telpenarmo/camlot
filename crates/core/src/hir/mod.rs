mod lower;
mod module;
mod pprint;

pub use module::Module;

use la_arena::Idx;

use crate::intern::Interned;

pub type ExprIdx = Idx<Expr>;
pub type TypeExprIdx = Idx<TypeExpr>;
pub type DefinitionIdx = Idx<Definition>;
pub type TypeDefinitionIdx = Idx<TypeDefinition>;
pub type ParamIdx = Idx<Param>;

pub(crate) type Name = Interned<String>;

#[derive(PartialEq, Debug)]
pub struct Definition {
    pub name: Name,
    pub params: Box<[ParamIdx]>,
    pub return_type: TypeExprIdx,
    pub defn: ExprIdx,
}

#[derive(PartialEq, Debug)]
pub struct Open {
    pub path: Name,
}

#[derive(PartialEq, Debug)]
pub struct TypeDefinition {
    pub name: Name,
    pub defn: TypeExprIdx,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Missing,
    LetExpr(Box<LetExpr>),
    IdentExpr { name: Name },
    LambdaExpr(LambdaExpr),
    AppExpr { func: ExprIdx, arg: ExprIdx },
    LiteralExpr(Literal),
}

impl Expr {
    pub(crate) fn let_expr(
        lhs: Pattern,
        return_type: TypeExprIdx,
        defn: ExprIdx,
        body: ExprIdx,
    ) -> Self {
        Self::LetExpr(Box::new(LetExpr {
            lhs,
            return_type,
            defn,
            body,
        }))
    }

    pub(crate) fn lambda_expr(param: ParamIdx, return_type: TypeExprIdx, body: ExprIdx) -> Self {
        Self::LambdaExpr(LambdaExpr {
            param,
            return_type,
            body,
        })
    }

    pub(crate) fn ident_expr<T: Into<Name>>(name: T) -> Self {
        Self::IdentExpr { name: name.into() }
    }

    pub(crate) fn int_expr(val: i64) -> Self {
        Self::LiteralExpr(Literal::IntLiteral(val))
    }

    #[allow(unused)]
    pub(crate) fn bool_expr(val: bool) -> Self {
        Self::LiteralExpr(Literal::BoolLiteral(val))
    }

    #[cfg(test)]
    fn ident_let(name: Name, typ: TypeExprIdx, defn: ExprIdx, body: ExprIdx) -> Self {
        Self::let_expr(Pattern::Ident(name), typ, defn, body)
    }
}

#[allow(unused)]
#[derive(PartialEq, Debug)]
pub struct LetExpr {
    pub lhs: Pattern,
    pub return_type: TypeExprIdx,
    pub defn: ExprIdx,
    pub body: ExprIdx,
}

#[allow(unused)]
#[derive(PartialEq, Debug)]
pub struct LambdaExpr {
    pub param: ParamIdx,
    pub return_type: TypeExprIdx,
    pub body: ExprIdx,
}

#[derive(PartialEq, Debug)]
pub enum TypeExpr {
    Missing,
    IdentTypeExpr { name: Name },
    TypeArrow { from: TypeExprIdx, to: TypeExprIdx },
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Literal {
    Unit,
    IntLiteral(i64),
    BoolLiteral(bool),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::IntLiteral(val) => val.fmt(f),
            Literal::BoolLiteral(val) => val.fmt(f),
            Literal::Unit => "()".fmt(f),
        }
    }
}

#[allow(unused)]
#[derive(PartialEq, Debug, Clone)]
pub struct Param {
    pub(crate) pattern: Pattern,
    pub(crate) typ: TypeExprIdx,
}

#[cfg(test)]
fn ident_param(name: Name, typ: TypeExprIdx) -> Param {
    Param {
        pattern: Pattern::Ident(name),
        typ,
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Pattern {
    Ident(Name),
    Wildcard,
    Unit,
}
