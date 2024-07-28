use la_arena::Idx;

pub(crate) type ExprIdx = Idx<Expr>;
pub(crate) type TypeExprIdx = Idx<TypeExpr>;
pub(crate) type DeclarationIdx = Idx<Declaration>;

#[allow(unused)]
pub struct Module {
    pub declarations: Box<[DeclarationIdx]>,
}

#[derive(PartialEq, Debug)]
pub struct DefDecl {
    pub name: String,
    pub defn: ExprIdx,
}

#[derive(PartialEq, Debug)]
pub enum Declaration {
    TypeDecl { name: String, defn: TypeExprIdx },
    DefDecl(DefDecl),
    OpenDecl { path: String },
}

impl Declaration {
    pub(crate) fn def_decl(name: String, defn: ExprIdx) -> Self {
        Self::DefDecl(DefDecl { name, defn })
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Missing,
    LetExpr(Box<LetExpr>),
    IdentExpr { name: String },
    LambdaExpr(Box<LambdaExpr>),
    AppExpr { func: ExprIdx, arg: ExprIdx },
    LiteralExpr(Literal),
}

impl Expr {
    pub(crate) fn let_expr(
        name: String,
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

    pub(crate) fn ident_expr<T: Into<String>>(name: T) -> Self {
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
    pub name: String,
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
    IdentTypeExpr { name: String },
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
    pub(crate) name: String,
    pub(crate) typ: TypeExprIdx,
}
