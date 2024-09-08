use la_arena::Idx;

use crate::intern::Interned;

pub(crate) type ExprIdx = Idx<Expr>;
pub(crate) type TypeExprIdx = Idx<TypeExpr>;
pub(crate) type DefinitionIdx = Idx<Definition>;
pub(crate) type OpenIdx = Idx<Open>;
pub(crate) type TypeDefinitionIdx = Idx<TypeDefinition>;

pub(crate) type Name = Interned<String>;

#[derive(PartialEq, Debug)]
pub struct Definition {
    pub name: Name,
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
    LambdaExpr(Box<LambdaExpr>),
    AppExpr { func: ExprIdx, arg: ExprIdx },
    LiteralExpr(Literal),
}

impl Expr {
    pub(crate) fn let_expr(
        name: Name,
        params: Box<[Param]>,
        return_type: TypeExprIdx,
        defn: ExprIdx,
        body: ExprIdx,
    ) -> Self {
        Self::LetExpr(Box::new(LetExpr {
            name,
            params,
            return_type,
            defn,
            body,
        }))
    }

    pub(crate) fn lambda_expr(param: Param, return_type: TypeExprIdx, body: ExprIdx) -> Self {
        Self::LambdaExpr(Box::new(LambdaExpr {
            param,
            return_type,
            body,
        }))
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
}

#[allow(unused)]
#[derive(PartialEq, Debug)]
pub struct LetExpr {
    pub name: Name,
    pub params: Box<[Param]>,
    pub return_type: TypeExprIdx,
    pub defn: ExprIdx,
    pub body: ExprIdx,
}

#[allow(unused)]
#[derive(PartialEq, Debug)]
pub struct LambdaExpr {
    pub param: Param,
    pub return_type: TypeExprIdx,
    pub body: ExprIdx,
}

#[derive(PartialEq, Debug)]
pub enum TypeExpr {
    Missing,
    IdentTypeExpr { name: Name },
    TypeArrow { from: TypeExprIdx, to: TypeExprIdx },
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    IntLiteral(i64),
    BoolLiteral(bool),
}

#[allow(unused)]
#[derive(PartialEq, Debug, Clone)]
pub struct Param {
    pub(crate) name: Name,
    pub(crate) typ: TypeExprIdx,
}
