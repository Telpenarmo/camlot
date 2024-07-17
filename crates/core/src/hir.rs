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
    DefDecl(Box<DefDecl>),
    OpenDecl { path: String },
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Missing,
    LetExpr(Box<LetExpr>),
    IdentExpr { name: String },
    LambdaExpr { param: Box<Param>, body: ExprIdx },
    AppExpr { func: ExprIdx, arg: ExprIdx },
    LiteralExpr(Literal),
}

#[allow(unused)]
#[derive(PartialEq, Debug)]
pub struct LetExpr {
    pub name: String,
    pub params: Box<[Param]>,
    pub defn: ExprIdx,
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
    pub(crate) typ: Option<TypeExprIdx>,
}

impl Param {
    /// Empty, "fake" parameter, used when no actual parameter is present,
    /// but HIR requires at least one (e.g. in lambdas).
    #[must_use]
    pub fn empty() -> Param {
        Param {
            name: String::new(),
            typ: None,
        }
    }
}
