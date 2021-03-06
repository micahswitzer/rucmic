use crate::ast::*;
use crate::lexer::Token::*;
use crate::lexer::*;

extern crate plex;
use plex::parser;

use std::error::Error;
use std::convert::TryInto;

parser! {
    fn parse_(Token, Span);

    (a, b) {
        Span {
            lo: a.lo,
            hi: b.hi,
        }
    }

    program: Program {
        decls[d] => Program { decls: d }
    }

    decls: Vec<Decl> {
        => vec![],
        decls[mut d] fundef[f] => {
            d.push(f);
            d
        },
        decls[mut d] vardecl[v] => {
            d.push(v);
            d
        }
    }

    basictype: BaseType {
        Int => BaseType::Int,
        Char => BaseType::Char
    }

    typeident: (Type, String) {
        basictype[t] Ident(s) => (Type::new_basic(t), s)
    }

    vardecl: Decl {
        typeident[ti] Semicolon => Decl {
            span: span!(),
            node: Decl_::VarDecl(ti.0, ti.1),
        },
        typeident[ti] LeftSquare Integer(n) RightSquare Semicolon => {
            let arr_type = ti.0.into_arr(n.try_into().unwrap());
            Decl {
                span: span!(),
                node: Decl_::VarDecl(arr_type, ti.1),
            }
        } 
    }

    fundecl: (Type, String, ParamList) {
        typeident[ti] LeftParen paramlist[pl] RightParen => (ti.0, ti.1, pl),
        Void Ident(s) LeftParen paramlist[pl] RightParen => (Type::new_basic(BaseType::Void), s, pl),
    }

    fundef: Decl {
        fundecl[fd] compoundstmt[st] => Decl {
            span: span!(),
            node: Decl_::FunDecl(fd.0, fd.1, fd.2, Some(Box::new(st))),
        },
        fundecl[fd] Semicolon => Decl {
            span: span!(),
            node: Decl_::FunDecl(fd.0, fd.1, fd.2, None),
        }
    }

    singleparam: Param {
        basictype[t] Ident(s) => (s, Type::new_basic(t)),
        basictype[t] LeftSquare RightSquare Ident(s) => (s, Type::new_ptr(t))
    }

    otherparams: ParamList {
        => vec![],
        otherparams[mut pl] Comma singleparam[p] => {
            pl.push(p);
            pl
        },
        otherparams[mut pl] Comma singleparam[p] => {
            pl.push(p);
            pl
        }
    }

    paramlist: ParamList {
        Void => vec![],
        singleparam[fp] otherparams[mut op] => {
            let mut pl = vec![fp];
            pl.append(&mut op);
            pl
        }
    }

    compoundstmt: Stmt {
        LeftCurly stmtlst[s] RightCurly => Stmt {
            span: span!(),
            node: Stmt_::Comp(s),
        }
    }

    stmtlst: Vec<Stmt> {
        => vec![],
        // allow an abritrary number of semicolons
        stmtlst[l] Semicolon => l,
        stmtlst[mut l] stmt[s] => {
            l.push(s);
            l
        }
    }

    stmt: Stmt {
        vardecl[vd] => Stmt {
            span: span!(),
            node: Stmt_::Decl(Box::new(vd)),
        },
        compoundstmt[cs] => Stmt {
            span: span!(),
            node: cs.node,
        },
        Return Semicolon => Stmt {
            span: span!(),
            node: Stmt_::Ret(None),
        },
        Return expr[e] Semicolon => Stmt {
            span: span!(),
            node: Stmt_::Ret(Some(Box::new(e))),
        },
        // this forces the parser to prefer a shift over a reduce
        // technically this could be fixed with changes to the grammar,
        // but Mr. Louden says this way is more elegant and I happen to agree
        #[no_reduce(Else)]
        If LeftParen expr[e] RightParen stmt[s] => Stmt {
            span: span!(),
            node: Stmt_::Cond(Box::new(e), Box::new(s), None),
        },
        If LeftParen expr[e] RightParen stmt[s] Else stmt[el] => Stmt {
            span: span!(),
            node: Stmt_::Cond(Box::new(e), Box::new(s), Some(Box::new(el))),
        },
        While LeftParen expr[e] RightParen stmt[s] => Stmt {
            span: span!(),
            node: Stmt_::Iter(Box::new(e), Box::new(s)),
        },
        // I don't like that arithmetic and comparison expressions are valid statements...
        expr[e] Semicolon => Stmt {
            span: span!(),
            node: Stmt_::Expr(Box::new(e)),
        }
    }

    expr: Expr {
        Ident(s) Assign expr[e] => Expr {
            span: span!(),
            node: Expr_::Assign(s, None, Box::new(e)),
        },
        Ident(s) LeftSquare expr[a] RightSquare Assign expr[e] => Expr {
            span: span!(),
            node: Expr_::Assign(s, Some(Box::new(a)), Box::new(e)),
        },
        simpleExpression[e] => e
    }

    simpleExpression: Expr {
        additiveExpr[a] LessThan additiveExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::CmpLT, Box::new(a), Box::new(b)),
        },
        additiveExpr[a] GreaterThan additiveExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::CmpGT, Box::new(a), Box::new(b)),
        },
        additiveExpr[a] LessThanEqual additiveExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::CmpLTE, Box::new(a), Box::new(b)),
        },
        additiveExpr[a] GreaterThanEqual additiveExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::CmpGTE, Box::new(a), Box::new(b)),
        },
        additiveExpr[a] Equals additiveExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::CmpEq, Box::new(a), Box::new(b)),
        },
        additiveExpr[a] NotEquals additiveExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::CmpNE, Box::new(a), Box::new(b)),
        },
        additiveExpr[e] => e
    }

    additiveExpr: Expr {
        additiveExpr[a] Plus termExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::Add, Box::new(a), Box::new(b)),
        },
        additiveExpr[a] Minus termExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::Sub, Box::new(a), Box::new(b)),
        },
        termExpr[e] => e
    }

    termExpr: Expr {
        termExpr[a] Multiply factorExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::Mul, Box::new(a), Box::new(b)),
        },
        termExpr[a] Divide factorExpr[b] => Expr {
            span: span!(),
            node: Expr_::BinOp(BinOp::Div, Box::new(a), Box::new(b)),
        },
        factorExpr[e] => e
    }

    factorExpr: Expr {
        Integer(i) => Expr {
            span: span!(),
            node: Expr_::Literal(i),
        },
        Ident(i) => Expr {
            span: span!(),
            node: Expr_::Var(i, None),
        },
        Ident(i) LeftSquare expr[e] RightSquare => Expr {
            span: span!(),
            node: Expr_::Var(i, Some(Box::new(e))),
        },
        Ident(i) LeftParen args[a] RightParen => Expr {
            span: span!(),
            node: Expr_::Call(i, a),
        },
        LeftParen expr[e] RightParen => e
    }

    args: Vec<Expr> {
        => vec![],
        argList[al] => al
    }

    argList: Vec<Expr> {
        expr[e] => vec![e],
        argList[mut al] Comma expr[e] => {
            al.push(e);
            al
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub token: Option<(Token, Span)>,
    pub message: &'static str,
}

// the public parsing function
pub fn parse<I: Iterator<Item = (Token, Span)>>(
    i: I,
) -> Result<Program, ParseError> {
    match parse_(i) {
        Ok(k) => Ok(k),
        Err(e) => Err(ParseError {
            token: e.0,
            message: e.1
        }),
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.token {
            Some(s) => write!(f, "{}: {:?}, {:?}", self.message, s.0, s.1),
            None => write!(f, "{}", self.message)
        }
    }
}

// default implementation
impl Error for ParseError { }
