use la_arena::Idx;

pub(crate) type ExprIdx = Idx<Expr>;
pub(crate) type TypeExprIdx = Idx<TypeExpr>;
pub(crate) type DeclarationIdx = Idx<Declaration>;

pub(crate) type Name = String;

#[allow(unused)]
pub struct Module {
    pub declarations: Box<[DeclarationIdx]>,
}

#[derive(PartialEq, Debug)]
pub struct DefDecl {
    pub name: Name,
    pub defn: ExprIdx,
}

#[derive(PartialEq, Debug)]
pub enum Declaration {
    TypeDecl { name: Name, defn: TypeExprIdx },
    DefDecl(DefDecl),
    OpenDecl { path: Name },
}

impl Declaration {
    pub(crate) fn def_decl(name: Name, defn: ExprIdx) -> Self {
        Self::DefDecl(DefDecl { name, defn })
    }
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
