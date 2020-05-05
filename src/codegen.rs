use inkwell::{ context::Context, module::Module, builder::Builder };
use crate::ast::*;

// TODO: keep track of register numbers?
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, mod_name: &str) -> CodeGen<'ctx> {
        CodeGen {
            context,
            module: context.create_module(mod_name),
            builder: context.create_builder(),
        }
    }

    pub fn gen(&self, program: &Program) -> Result<(), CodeGenError> {
        program.gencode(self)
    }
}

#[derive(Debug)]
pub struct CodeGenError {
    message: String,
    span: Option<crate::lexer::Span>,
}

pub trait GenCode {
    fn gencode(&self, context: &CodeGen) -> Result<(), CodeGenError>;
}

impl GenCode for Program {
    fn gencode(&self, context: &CodeGen) -> Result<(), CodeGenError> {
        for decl in self.decls.iter() {
            match &decl.node {
                Decl_::VarDecl(name, size) => { /* declare var here */ },
                Decl_::FunDecl(ret_type, name, params, body) => {
                    body.gencode(context)?;
                }
            }
        }
        Ok(())
    }
}

impl GenCode for Stmt {
    fn gencode(&self, context: &CodeGen) -> Result<(), CodeGenError> {
        match &self.node {
            Stmt_::Comp(stmts) => {
                for stmt in stmts.iter() {
                    stmt.gencode(context)?;
                }
            },
            Stmt_::Expr(e) => {
                e.gencode(context)?;
            },
            // TODO: match the rest of these variants
            n => { return Err(CodeGenError { message: format!("unknown type {:?}", n), span: Some(self.span) }); }
        }
        Ok(())
    }
}

impl GenCode for Expr {
    fn gencode(&self, context: &CodeGen) -> Result<(), CodeGenError> {
        match &self.node {
            Expr_::Add(lhs, rhs) => {
                lhs.gencode(context)?;
                rhs.gencode(context)?;
                // do something else
            },
            Expr_::Var(name, _) => {
                println!("I'm a var! Name: {}, Span: {}-{}", name, self.span.lo, self.span.hi);
            },
            // TODO: match the rest of these variants
            _ => { return Err(CodeGenError { message: format!("unknown type {:?}", self.node), span: Some(self.span) }); }
        };
        // we okay
        Ok(())
    }
}
