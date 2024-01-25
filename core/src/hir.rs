use la_arena::Idx;

pub(crate) type ExprIdx = Idx<Expr>;
pub(crate) type TypeExprIdx = Idx<TypeExpr>;

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
    LiteralExpr { lit: Literal },
}

#[allow(unused)]
pub struct LetExpr {
    name: String,
    params: Box<[Param]>,
    defn: ExprIdx,
    body: ExprIdx,
}

pub enum TypeExpr {
    Missing,
    IdentTypeExpr { name: String },
    TypeArrow { from: TypeExprIdx, to: TypeExprIdx },
}

pub enum Literal {
    IntLiteral { value: i64 },
    BoolLiteral { value: bool },
    UnitLiteral,
}

pub struct Param {
    pub(crate) name: String,
    pub(crate) typ: Option<TypeExprIdx>,
}
