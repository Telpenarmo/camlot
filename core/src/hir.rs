use la_arena::Idx;

pub(crate) type ExprIdx = Idx<Expr>;
pub(crate) type TypeExprIdx = Idx<TypeExpr>;

#[allow(unused)]
pub struct Module {
    declarations: Box<[Declaration]>,
}

pub struct LetDecl {
    pub name: String,
    pub params: Box<[Param]>,
    pub defn: Box<Expr>,
}

pub enum Declaration {
    TypeDecl { name: String, defn: Box<TypeExpr> },
    LetDecl(Box<LetDecl>),
    OpenDecl { path: String },
}

pub enum Expr {
    Missing,
    LetExpr(Box<LetExpr>),
    IdentExpr { name: String },
    LambdaExpr { params: Box<[Param]>, body: ExprIdx },
    AppExpr { func: ExprIdx, arg: ExprIdx },
    LiteralExpr(Literal),
}

#[allow(unused)]
pub struct LetExpr {
    pub name: String,
    pub params: Box<[Param]>,
    pub defn: ExprIdx,
    pub body: ExprIdx,
}

pub enum TypeExpr {
    Missing,
    IdentTypeExpr { name: String },
    TypeArrow { from: TypeExprIdx, to: TypeExprIdx },
}

pub enum Literal {
    IntLiteral(i64),
    BoolLiteral(bool),
    UnitLiteral,
}

#[allow(unused)]
pub struct Param {
    pub(crate) name: String,
    pub(crate) typ: Option<TypeExprIdx>,
}
