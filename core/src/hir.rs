use la_arena::Idx;

type ExprIdx = Idx<Expr>;
type TypeExprIdx = Idx<TypeExpr>;

pub struct Module {
    sdeclarations: Vec<Declaration>,
}

pub enum Declaration {
    TypeDecl { name: String, defn: TypeExpr },
    LetDecl { name: String, params: Vec<Param>, defn: Expr },
    OpenDecl { path: String },
}

pub enum Expr {
    Missing,
    ParenExpr {
        expr: ExprIdx,
    },
    LetExpr {
        name: String,
        params: Vec<Param>,
        defn: ExprIdx,
        body: ExprIdx,
    },
    IdentExpr {
        name: String,
    },
    LambdaExpr {
        params: Vec<Param>,
        body: ExprIdx,
    },
    AppExpr {
        func: ExprIdx,
        arg: ExprIdx,
    },
    LiteralExpr {
        lit: Literal,
    },
}

pub enum TypeExpr {
    Missing,
    ParenTypeExpr { expr: TypeExprIdx },
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
    pub(crate) typ: Option<TypeExpr>,
}
