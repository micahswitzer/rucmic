use crate::lexer::Span;

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub node: Expr_,
}

#[derive(Debug)]
pub enum Expr_ {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    CmpLT(Box<Expr>, Box<Expr>),
    CmpGT(Box<Expr>, Box<Expr>),
    CmpLTE(Box<Expr>, Box<Expr>),
    CmpGTE(Box<Expr>, Box<Expr>),
    CmpEq(Box<Expr>, Box<Expr>),
    CmpNE(Box<Expr>, Box<Expr>),
    // var-name, array index, expression
    Assign(String, Option<Box<Expr>>, Box<Expr>),
    Var(String, Option<Box<Expr>>),
    Call(String, Vec<Expr>),
    Literal(i64),
}

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub node: Stmt_,
}

#[derive(Debug)]
pub enum Stmt_ {
    // var decl statement (not sure if this is the right
    // way of implementing this...)
    Decl(Box<Decl>),
    // compound statement
    Comp(Vec<Stmt>),
    // expression statement
    Expr(Box<Expr>),
    // if-else statement
    Cond(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    // iteration statement
    Iter(Box<Expr>, Box<Stmt>),
    // return statement
    Ret(Option<Box<Expr>>),
}

#[derive(Debug)]
pub struct Decl {
    pub span: Span,
    pub node: Decl_,
}

#[derive(Debug)]
pub enum Decl_ {
    VarDecl(String, Option<i64>),
    // the statement must be a compound statement
    FunDecl(Type, String, Vec<(String, bool)>, Box<Stmt>),
}

#[derive(Debug)]
pub enum Type {
    Int,
    Void,
}
