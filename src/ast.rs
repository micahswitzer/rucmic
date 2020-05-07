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
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    CmpLT,
    CmpGT,
    CmpLTE,
    CmpGTE,
    CmpEq,
    CmpNE,
}

#[derive(Debug)]
pub enum Expr_ {
    BinOp(BinOp, Box<Expr>, Box<Expr>),
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
    VarDecl(Type, String),
    // the statement must be a compound statement (gramatically yes, but the code generator doesn't care)
    FunDecl(Type, String, Vec<(String, Type)>, Box<Stmt>),
}

#[derive(Debug)]
pub struct Type {
    base_type: BaseType,
    array_size: Option<u32>,
    is_ptr: bool,
}

#[derive(Debug)]
pub enum BaseType {
    Compound(Box<Type>),
    Int,
    Char,
    Void,
}

impl Type {
    pub fn new(base_type: BaseType, array_size: Option<u32>, is_ptr: bool) -> Type {
        Type {
            base_type,
            array_size,
            is_ptr,
        }
    }

    pub fn new_basic(bt: BaseType) -> Type {
        Type {
            base_type: bt,
            array_size: None,
            is_ptr: false,
        }
    }

    pub fn new_ptr(bt: BaseType) -> Type {
        Type {
            base_type: bt,
            array_size: None,
            is_ptr: true,
        }
    }

    pub fn into_arr(self, array_size: u32) -> Type {
        assert_eq!(self.array_size, None);
        Type {
            base_type: self.base_type,
            is_ptr: self.is_ptr,
            array_size: Some(array_size)
        }
    }
}
